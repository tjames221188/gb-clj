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
   :halted? false})

(defn inc-pc
  ([state]
   (inc-pc state 1))
  ([state n]
   (update-in state [:cpu :pc] #(+ % n))))

(def Z-mask 2r10000000)
(def N-mask 2r01000000)
(def H-mask 2r00100000)
(def C-mask 2r00010000)

(defn set-flag [state mask]
  (update-in state [:cpu :f] bit-or mask))

(defn unset-flag [state mask]
  (update-in state [:cpu :f] bit-and-not mask))

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

(defn inc8 [state r]
  (let [prev (get-in state [:cpu r])
        val (bit-and 0xFF (inc prev))
        z? (zero? val)
        h? (= 0x0F (bit-and prev 0x0F))]
    (-> state
        (assoc-in [:cpu r] val)
        (unset-flag N-mask)
        (cond-> z? (set-flag Z-mask)
                (not z?) (unset-flag Z-mask)
                h? (set-flag H-mask)
                (not h?) (unset-flag H-mask)))))

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

(defn copy-register [state r-src r-dest]
  (let [v (get-in state [:cpu r-src])]
    (assoc-in state [:cpu r-dest] v)))

(defmulti execute (fn [_state opcode] opcode))

(defmethod execute 0x00 NOP
  [state _]
  (inc-pc state))

(defmethod execute 0x0E LD_C_N
  [state _]
  (load8-immediate state :c))

(defmethod execute 0x11 LD_DE_NN
  [state _]
  (load16-immediate state :d :e))

(defmethod execute 0x12 LD_ADDR_DE_A
  [state _]
  (let [addr (get16 state :d :e)]
    (-> state
        (bus/write-byte addr (get-in state [:cpu :a]))
        (inc-pc))))

(defmethod execute 0x13 INC_DE
  [state _]
  (-> (inc16 state :d :e)
      (inc-pc)))

(defmethod execute 0x1C INC_E
  [state _]
  (-> (inc8 state :e)
      (inc-pc)))

(defmethod execute 0x21 LD_HL_NN
  [state _]
  (load16-immediate state :h :l))

(defmethod execute 0x2A LD_A_ADDR_HLI
  [state _]
  (let [addr (get16 state :h :l)
        val (bus/read-byte state addr)]
    (-> state
        (assoc-in [:cpu :a] val)
        (inc16 :h :l)
        (inc-pc))))

(defmethod execute 0x47 LD_B_A
  [state _]
  (-> (copy-register state :a :b)
      (inc-pc)))

(defmethod execute 0xC3 JP_NN
  [state _]
  (let [addr (bus/read-word state (inc (get-in state [:cpu :pc])))]
    (assoc-in state [:cpu :pc] addr)))

(defmethod execute :default
  [state opcode]
  (throw (Exception. (format "Opcode 0x%02X not implemented at PC: 0x%04X"
                             opcode (get-in state [:cpu :pc])))))

(defn format-trace [state opcode]
  (let [{:keys [pc a f sp]} (:cpu state)]
    (format "PC: 0x%04X | Op: 0x%02X | A: %02X F: %02X | BC: %04X | DE: %04X | HL: %04X | SP: %04X"
            pc opcode a f
            (get16 state :b :c) (get16 state :d :e) (get16 state :h :l)
            sp)))

(defn step [state]
  (if (:halted? state)
    state
    (let [pc (get-in state [:cpu :pc])
          opcode (bus/read-byte state pc)]
      (try
        (execute state opcode)
        (catch Exception e
          (log/error e (format "CPU error! Trace: \n%s\n" (format-trace state opcode)))
          (throw e))))))

