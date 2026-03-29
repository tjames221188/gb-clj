(ns gb-clj.cpu
  (:require [clojure.tools.logging :as log]
            [gb-clj.bus :as bus]))

(defn initial-state []
  {:a 0x01   ; Initial Accumulator
   :f 0xB0   ; Flags: Z=1, N=0, H=1, C=1 (on original DMG)
   :b 0x00 :c 0x13
   :d 0x00 :e 0xD8
   :h 0x01 :l 0x4D
   :pc 0x0100 ; The entry point of every GB ROM
   :sp 0xFFFE ; Top of stack
   :halted? false
   :t-cycles 0
   :interrupts-enabled? true})

(defn tick [state n]
  (update-in state [:cpu :t-cycles] + n))

(defn inc-pc
  ([state]
   (inc-pc state 1))
  ([state n]
   (update-in state [:cpu :pc] #(bit-and 0xFFFF (+ % n)))))

(def Z-mask 2r10000000)
(def N-mask 2r01000000)
(def H-mask 2r00100000)
(def C-mask 2r00010000)

(defn flag-set? [state mask]
  (pos? (bit-and (get-in state [:cpu :f]) mask)))

(defn set-flag [state mask]
  (update-in state [:cpu :f] bit-or mask))

(defn unset-flag [state mask]
  (update-in state [:cpu :f] bit-and-not mask))

(defn update-flag [state mask ?]
  (if ?
    (set-flag state mask)
    (unset-flag state mask)))

(defn combine [high low]
  (bit-or (bit-shift-left high 8) low))

(defn split [word]
  [(bit-shift-right (bit-and word 0xFF00) 8) ; High byte
   (bit-and word 0x00FF)])                   ; Low byte

(defn get16 [state r1 r2]
  (->> (:cpu state)
       ((juxt r1 r2))
       (apply combine)))

(defn set16 [state r1 r2 v]
  (let [[high low] (split v)]
    (-> state
        (assoc-in [:cpu r1] high)
        (assoc-in [:cpu r2] low))))

(defn dec8 [state r]
  (let [prev (get-in state [:cpu r])
        val (bit-and 0xFF (dec prev))
        z? (zero? val)
        h? (zero? (bit-and 0xF prev)) ;; will always "borrow" from upper nibble if lower nibble is 0000
        ]
    (-> state
        (assoc-in [:cpu r] val)
        (set-flag N-mask)
        (cond-> z? (set-flag Z-mask)
                (not z?) (unset-flag Z-mask)
                h? (set-flag H-mask)
                (not h?) (unset-flag H-mask)))))

(defn inc8 [state r]
  (let [prev (get-in state [:cpu r])
        val (bit-and 0xFF (inc prev))
        z? (zero? val)
        h? (= 0x0F (bit-and prev 0x0F))]
    (-> state
        (assoc-in [:cpu r] val)
        (unset-flag N-mask)
        (update-flag Z-mask z?)
        (update-flag H-mask h?))))

(defn inc16 [state r1 r2]
  ;; TODO - this currently only works for register pairs. the stack pointer is 
  ;;        a single 16 bit field - add a case for this
  (let [[r1-v r2-v] (->> (:cpu state)
                         ((juxt r1 r2))
                         (apply combine)
                         (inc)
                         (bit-and 0xFFFF)
                         (split))]
    (-> state
        (assoc-in [:cpu r1] r1-v)
        (assoc-in [:cpu r2] r2-v))))

(defn load8-immediate
  [state r1]
  (let [pc (get-in state [:cpu :pc])
        n (bus/read-byte state (inc pc))]
    (-> state
        (assoc-in [:cpu r1] n)
        (inc-pc 2))))

(defn load16-immediate
  [state r1 r2]
  (let [pc (get-in state [:cpu :pc])
        nn (bus/read-word state (inc pc))]
    (-> state
        (set16 r1 r2 nn)
        (inc-pc 3))))

(defn pop16 [state r1 r2]
  (let [sp (get-in state [:cpu :sp])
        low (cond-> (bus/read-byte state sp)
              ; lower nibble of f register must always be zero
              (= :f r2) (bit-and 0xF0))
        high (bus/read-byte state (inc sp))]
    (-> state
        (assoc-in [:cpu r1] high)
        (assoc-in [:cpu r2] low)
        (update-in [:cpu :sp] #(bit-and 0xFFFF (+ % 2))))))

(defn push16 [state r1 r2]
  (let [sp (get-in state [:cpu :sp])
        h-addr (bit-and 0xFFFF (dec sp))
        l-addr (bit-and 0xFFFF (dec h-addr))]
    (-> state
        (bus/write-byte h-addr (get-in state [:cpu r1]))
        (bus/write-byte l-addr (get-in state [:cpu r2]))
        (assoc-in [:cpu :sp] l-addr))))

(defn copy-register [state r-src r-dest]
  (let [v (get-in state [:cpu r-src])]
    (assoc-in state [:cpu r-dest] v)))

(defn as-signed-8 [n]
  (if (>= n 128)
    (- n 256)
    n))

(defn jump-relative-pred-r8
  [state pred?]
  (let [offset (as-signed-8 (bus/read-byte state (inc (get-in state [:cpu :pc]))))]
    (if (pred? state)
      (-> state
          (inc-pc (+ 2 offset))
          (tick 12))
      (-> state
          (inc-pc 2)
          (tick 8)))))

(defmulti execute (fn [_state opcode] opcode))

(defmethod execute 0x00 NOP
  [state _]
  (-> (inc-pc state)
      (tick 4)))

(defmethod execute 0x01 LD_BC_NN
  [state _]
  (-> (load16-immediate state :b :c)
      (tick 12)))

(defmethod execute 0x03 INC_BC
  [state _]
  (-> (inc16 state :b :c)
      (inc-pc)
      (tick 8)))

(defmethod execute 0x05 DEC_B
  [state _]
  (-> (dec8 state :b)
      (inc-pc)
      (tick 4)))

(defmethod execute 0x06 LD_B_N
  [state _]
  (-> (load8-immediate state :b)
      (tick 8)))

(defmethod execute 0x0D DEC_C
  [state _]
  (-> (dec8 state :c)
      (inc-pc)
      (tick 4)))

(defmethod execute 0x0E LD_C_N
  [state _]
  (-> (load8-immediate state :c)
      (tick 8)))

(defmethod execute 0x11 LD_DE_NN
  [state _]
  (-> (load16-immediate state :d :e)
      (tick 12)))

(defmethod execute 0x12 LD_ADDR_DE_A
  [state _]
  (let [addr (get16 state :d :e)]
    (-> state
        (bus/write-byte addr (get-in state [:cpu :a]))
        (inc-pc)
        (tick 8))))

(defmethod execute 0x13 INC_DE
  [state _]
  (-> (inc16 state :d :e)
      (inc-pc)
      (tick 8)))

(defmethod execute 0x14 INC_D
  [state _]
  (-> (inc8 state :d)
      (inc-pc)
      (tick 4)))

(defmethod execute 0x18 JR_r8
  [state _]
  (let [offset (as-signed-8 (bus/read-byte state (inc (get-in state [:cpu :pc]))))]
    (-> state
        (inc-pc (+ 2 offset))
        (tick 12))))

(defmethod execute 0x1A LD_A_DE_ADDR
  [state _]
  (let [addr (get16 state :d :e)
        val (bus/read-byte state addr)]
    (-> state
        (assoc-in [:cpu :a] val)
        (inc-pc)
        (tick 8))))

(defmethod execute 0x1C INC_E
  [state _]
  (-> (inc8 state :e)
      (inc-pc)
      (tick 4)))

(defmethod execute 0x20 JR_NZ_r8
  [state _]
  (jump-relative-pred-r8 state #(not (flag-set? % Z-mask))))

(defmethod execute 0x21 LD_HL_NN
  [state _]
  (-> (load16-immediate state :h :l)
      (tick 12)))

(defmethod execute 0x23 INC_HL
  [state _]
  (-> (inc16 state :h :l)
      (inc-pc)
      (tick 8)))

(defmethod execute 0x24 INC_H
  [state _]
  (-> (inc8 state :h)
      (inc-pc)
      (tick 4)))

(defmethod execute 0x28 JR_Z_r8
  [state _]
  (jump-relative-pred-r8 state #(flag-set? % Z-mask)))

(defmethod execute 0x2A LD_A_ADDR_HLI
  [state _]
  (let [addr (get16 state :h :l)
        val (bus/read-byte state addr)]
    (-> state
        (assoc-in [:cpu :a] val)
        (inc16 :h :l)
        (inc-pc)
        (tick 8))))

(defmethod execute 0x2C INC_L
  [state _]
  (-> (inc8 state :l)
      (inc-pc)
      (tick 4)))

(defmethod execute 0x31 LD_SP_NN
  [state _]
  (let [pc (get-in state [:cpu :pc])
        nn (bus/read-word state (inc pc))]
    (-> state
        (assoc-in [:cpu :sp] nn)
        (inc-pc 3)
        (tick 12))))

(defmethod execute 0x3E LD_A_N
  [state _]
  (let [pc (get-in state [:cpu :pc])
        n (bus/read-byte state (inc pc))]
    (-> state
        (assoc-in [:cpu :a] n)
        (inc-pc 2)
        (tick 8))))

(defmethod execute 0x47 LD_B_A
  [state _]
  (-> (copy-register state :a :b)
      (inc-pc)
      (tick 4)))

(defmethod execute 0x77 LD_HL_ADDR_A
  [state _]
  (let [addr (get16 state :h :l)]
    (-> state
        (bus/write-byte addr (get-in state [:cpu :a]))
        (inc-pc)
        (tick 8))))

(defmethod execute 0x78 LD_A_B
  [state _]
  (-> (copy-register state :b :a)
      (inc-pc)
      (tick 4)))

(defmethod execute 0x7C LD_A_H
  [state _]
  (-> (copy-register state :h :a)
      (inc-pc)
      (tick 4)))

(defmethod execute 0x7D LD_A_L
  [state _]
  (-> (copy-register state :l :a)
      (inc-pc)
      (tick 4)))

(defmethod execute 0xA9 XOR_C
  [state _]
  (let [{:keys [a c]} (:cpu state)
        val (bit-xor a c)]
    (-> state
        (assoc-in [:cpu :a] val)
        (update-flag Z-mask (zero? val))
        (unset-flag N-mask)
        (unset-flag H-mask)
        (unset-flag C-mask)
        (inc-pc)
        (tick 4))))

(defmethod execute 0xB1 OR_C
  [state _]
  (let [{:keys [a c]} (:cpu state)
        val (bit-or a c)]
    (-> state
        (assoc-in [:cpu :a] val)
        (update-flag Z-mask (zero? val))
        (unset-flag N-mask)
        (unset-flag H-mask)
        (unset-flag C-mask)
        (inc-pc)
        (tick 4))))

(defmethod execute 0xC1 POP_BC
  [state _]
  (-> state
      (pop16 :b :c)
      (inc-pc)
      (tick 12)))

(defmethod execute 0xC3 JP_NN
  [state _]
  (let [addr (bus/read-word state (inc (get-in state [:cpu :pc])))]
    (-> (assoc-in state [:cpu :pc] addr)
        (tick 16))))

(defmethod execute 0xC4 CALL_NZ_NN
  [state _]
  (let [pc (get-in state [:cpu :pc])
        return-addr (bit-and 0xFFFF (+ pc 3))
        [high low] (split return-addr)
        target-addr (bus/read-word state (inc pc))
        sp (get-in state [:cpu :sp])]
    (if (flag-set? state Z-mask)
      (-> state
          (inc-pc 3)
          (tick 12))
      (-> state
        ;; Push return address to stack
          (bus/write-byte (dec sp) high)
          (bus/write-byte (- sp 2) low)
        ;; Update Registers
          (assoc-in [:cpu :sp] (- sp 2))
          (assoc-in [:cpu :pc] target-addr)
          (tick 24)))))

(defmethod execute 0xC5 PUSH_BC
  [state _]
  (-> state
      (push16 :b :c)
      (inc-pc)
      (tick 16)))

(defmethod execute 0xC9 RET
  [state _]
  (let [sp (get-in state [:cpu :sp])
        return-addr (bus/read-word state sp)]
    (-> state
        (assoc-in [:cpu :sp] (bit-and 0xFFFF (+ sp 2)))
        (assoc-in [:cpu :pc] return-addr)
        (tick 16))))

(defmethod execute 0xCD CALL_NN
  [state _]
  (let [pc (get-in state [:cpu :pc])
        return-addr (bit-and 0xFFFF (+ pc 3))
        [high low] (split return-addr)
        target-addr (bus/read-word state (inc pc))
        sp (get-in state [:cpu :sp])]
    (-> state
        ;; Push return address to stack
        (bus/write-byte (dec sp) high)
        (bus/write-byte (- sp 2) low)
        ;; Update Registers
        (assoc-in [:cpu :sp] (- sp 2))
        (assoc-in [:cpu :pc] target-addr)
        (tick 24))))

(defmethod execute 0xE0 LDH_ADDR_A8_A
  [state _]
  (let [addr (-> (bus/read-byte state (inc (get-in state [:cpu :pc])))
                 (+ 0xFF00))]
    (log/info (str "0xE0 addr:" (Integer/toHexString addr) " a: " (get-in state [:cpu :a])))
    (-> state
        (bus/write-byte addr (get-in state [:cpu :a]))
        (inc-pc 2)
        (tick 12))))

(defmethod execute 0xE1 POP_HL
  [state _]
  (-> state
      (pop16 :h :l)
      (inc-pc)
      (tick 12)))

(defmethod execute 0xE6 AND_N
  [state _]
  (let [a (get-in state [:cpu :a])
        n (bus/read-byte state (inc (get-in state [:cpu :pc])))
        val (bit-and a n)]
    (-> state
        (assoc-in [:cpu :a] val)
        (update-flag Z-mask (zero? val))
        (unset-flag N-mask)
        (set-flag H-mask)
        (unset-flag C-mask)
        (inc-pc 2)
        (tick 8))))

(defmethod execute 0xE5 PUSH_HL
  [state _]
  (-> state
      (push16 :h :l)
      (inc-pc)
      (tick 16)))

(defmethod execute 0xEA LD_ADDR_A16_A
  [state _]
  (let [addr (bus/read-word state (inc (get-in state [:cpu :pc])))]
    (-> state
        (bus/write-byte addr (get-in state [:cpu :a]))
        (inc-pc 3)
        (tick 16))))

(defmethod execute 0xF0 LDH_A_ADDR_A8
  [state _]
  (let [addr (-> (bus/read-byte state (inc (get-in state [:cpu :pc])))
                 (+ 0xFF00))
        val (bus/read-byte state addr)]
    (-> state
        (assoc-in [:cpu :a] val)
        (inc-pc 2)
        (tick 12))))

(defmethod execute 0xF1 POP_AF
  [state _]
  (-> state
      (pop16 :a :f)
      (inc-pc)
      (tick 12)))

(defmethod execute 0xF3 DI
  [state _]
  (-> state
      (assoc-in [:cpu :interrupts-enabled?] false)
      (inc-pc)
      (tick 4)))

(defmethod execute 0xF5 PUSH_AF
  [state _]
  (-> state
      (push16 :a :f)
      (inc-pc)
      (tick 16)))

(defmethod execute 0xFA LD_A_ADDR_A16
  [state _]
  (let [addr (bus/read-word state (inc (get-in state [:cpu :pc])))
        val (bus/read-byte state addr)]
    (-> state
        (assoc-in [:cpu :a] val)
        (inc-pc 3)
        (tick 16))))

(defmethod execute 0xFE CP_N
  [state _]
  (let [val (bus/read-byte state (inc (get-in state [:cpu :pc])))
        acc (get-in state [:cpu :a])
        result (- acc val)
        half-carry? (< (bit-and 0x0F acc) (bit-and 0x0F val))]
    (-> state
        (update-flag Z-mask (zero? result))
        (set-flag N-mask)
        (update-flag H-mask half-carry?)
        (update-flag C-mask (< acc val))
        (inc-pc 2)
        (tick 8))))

(defmethod execute :default
  [state opcode]
  (throw (Exception. (format "Opcode 0x%02X not implemented at PC: 0x%04X"
                             opcode (get-in state [:cpu :pc])))))

(defn format-trace [state opcode]
  (let [{:keys [pc a f sp t-cycles]} (:cpu state)]
    (format "PC: 0x%04X | Op: 0x%02X | A: %02X F: %02X | BC: %04X | DE: %04X | HL: %04X | SP: %04X (t-cycles: %d)"
            pc opcode a f
            (get16 state :b :c) (get16 state :d :e) (get16 state :h :l)
            sp t-cycles)))

(defn step [state]
  (if (:halted? state)
    state
    (let [pc (get-in state [:cpu :pc])
          opcode (bus/read-byte state pc)]
      (try
        (log/info (format-trace state opcode))
        (execute state opcode)
        (catch Exception e
          (log/error e (format "CPU error! Trace: \n%s\n" (format-trace state opcode)))
          (throw e))))))

