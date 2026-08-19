(ns gb-clj.cpu.instructions
  (:require
   [clojure.tools.logging :as log]
   [gb-clj.bus :as bus]
   [gb-clj.cpu.bits :as bits]
   [gb-clj.cpu.prefix-instructions :as prefix]
   [gb-clj.cpu.util :as util]))

(defn rotate-a [gb-state rotate-fn]
  (let [old-a (get-in gb-state [:cpu :a])
        old-c (if (util/flag-set? gb-state util/C-mask) 1 0)
        [new-a new-c] (rotate-fn old-a old-c)]
    (-> gb-state
        (assoc-in [:cpu :a] new-a)
        (util/unset-flag util/Z-mask)
        (util/unset-flag util/N-mask)
        (util/unset-flag util/H-mask)
        (util/update-flag util/C-mask (pos? new-c)))))

(defn low-nibble-overflow? [a]
  (> (bit-and a 0x0F) 0x09))

(defn high-nibble-overflow? [a]
  (> a 0x99))

(defn compare-val [gb-state val]
  (let [acc (get-in gb-state [:cpu :a])
        result (- acc val)
        half-carry? (< (bit-and 0x0F acc) (bit-and 0x0F val))]
    (-> gb-state
        (util/update-flag util/Z-mask (zero? result))
        (util/set-flag util/N-mask)
        (util/update-flag util/H-mask half-carry?)
        (util/update-flag util/C-mask (< acc val)))))

(defmulti execute (fn [_gb-state opcode] opcode))

(defmethod execute 0x00 NOP
  [gb-state _]
  (-> (util/inc-pc gb-state)
      (util/tick 4)))

(defmethod execute 0x01 LD_BC_NN
  [gb-state _]
  (-> (util/load16-immediate gb-state :b :c)
      (util/tick 12)))

(defmethod execute 0x02 LD_ADDR_BC_A
  [gb-state _]
  (util/write-r-to-addr16 gb-state :a :b :c))

(defmethod execute 0x03 INC_BC
  [gb-state _]
  (-> (util/inc16 gb-state :b :c)
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x04 INC_B
  [gb-state _]
  (-> (util/inc-r8 gb-state :b)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x05 DEC_B
  [gb-state _]
  (-> (util/dec-r8 gb-state :b)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x06 LD_B_N
  [gb-state _]
  (util/load8-immediate gb-state :b))

(defmethod execute 0x07 RLC_A
  [gb-state _]
  (-> (rotate-a gb-state util/rotate-left-circular)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x08 LD_ADDR_A16_SP
  [gb-state _]
  (let [addr (bus/read-word gb-state (inc (get-in gb-state [:cpu :pc])))]
    (-> gb-state
        (bus/write-word addr (get-in gb-state [:cpu :sp]))
        (util/inc-pc 3)
        (util/tick 20))))

(defmethod execute 0x09 ADD_HL_BC
  [gb-state _]
  (-> (util/add-hl gb-state (util/get16 gb-state :b :c))
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x0A LD_A_ADDR_BC
  [gb-state _]
  (util/load-r-from-addr16 gb-state :a :b :c))

(defmethod execute 0x0B DEC_BC
  [gb-state _]
  (-> (util/dec-r16 gb-state :b :c)
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x0C INC_C
  [gb-state _]
  (-> (util/inc-r8 gb-state :c)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x0D DEC_C
  [gb-state _]
  (-> (util/dec-r8 gb-state :c)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x0E LD_C_N
  [gb-state _]
  (util/load8-immediate gb-state :c))

(defmethod execute 0x0F RRC_A
  [gb-state _]
  (-> (rotate-a gb-state util/rotate-right-circular)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x11 LD_DE_NN
  [gb-state _]
  (-> (util/load16-immediate gb-state :d :e)
      (util/tick 12)))

(defmethod execute 0x12 LD_ADDR_DE_A
  [gb-state _]
  (util/write-r-to-addr16 gb-state :a :d :e))

(defmethod execute 0x13 INC_DE
  [gb-state _]
  (-> (util/inc16 gb-state :d :e)
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x14 INC_D
  [gb-state _]
  (-> (util/inc-r8 gb-state :d)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x15 DEC_D
  [gb-state _]
  (-> (util/dec-r8 gb-state :d)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x16 LD_D_N
  [gb-state _]
  (util/load8-immediate gb-state :d))

(defmethod execute 0x17 RL_A
  [gb-state _]
  (-> (rotate-a gb-state util/rotate-thru-carry-left)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x18 JR_r8
  [gb-state _]
  (let [offset (util/as-signed-8 (bus/read-byte gb-state (inc (get-in gb-state [:cpu :pc]))))]
    (-> gb-state
        (util/inc-pc (+ 2 offset))
        (util/tick 12))))

(defmethod execute 0x19 ADD_HL_DE
  [gb-state _]
  (-> (util/add-hl gb-state (util/get16 gb-state :d :e))
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x1A LD_A_ADDR_DE
  [gb-state _]
  (util/load-r-from-addr16 gb-state :a :d :e))

(defmethod execute 0x1B DEC_DE
  [gb-state _]
  (-> (util/dec-r16 gb-state :d :e)
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x1C INC_E
  [gb-state _]
  (-> (util/inc-r8 gb-state :e)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x1D DEC_E
  [gb-state _]
  (-> (util/dec-r8 gb-state :e)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x1E LD_E_N
  [gb-state _]
  (util/load8-immediate gb-state :e))

(defmethod execute 0x1F RR_A
  [gb-state _]
  (-> (rotate-a gb-state util/rotate-thru-carry-right)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x20 JR_NZ_r8
  [gb-state _]
  (util/jump-relative-pred-r8 gb-state #(not (util/flag-set? % util/Z-mask))))

(defmethod execute 0x21 LD_HL_NN
  [gb-state _]
  (-> (util/load16-immediate gb-state :h :l)
      (util/tick 12)))

(defmethod execute 0x22 LD_HL_INC_ADDR_A
  [gb-state _]
  (let [addr (util/get16 gb-state :h :l)]
    (-> gb-state
        (bus/write-byte addr (get-in gb-state [:cpu :a]))
        (util/inc16 :h :l)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0x23 INC_HL
  [gb-state _]
  (-> (util/inc16 gb-state :h :l)
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x24 INC_H
  [gb-state _]
  (-> (util/inc-r8 gb-state :h)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x25 DEC_H
  [gb-state _]
  (-> (util/dec-r8 gb-state :h)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x26 LD_H_N
  [gb-state _]
  (util/load8-immediate gb-state :h))

(defmethod execute 0x27 DAA
  [gb-state _]
  (let [a (get-in gb-state [:cpu :a])
        n? (util/flag-set? gb-state util/N-mask)
        h? (util/flag-set? gb-state util/H-mask)
        c? (util/flag-set? gb-state util/C-mask)
        correct-low? (or h? (and (not n?) (low-nibble-overflow? a)))
        correct-high? (or c? (and (not n?) (high-nibble-overflow? a)))
        adj (+ (if correct-high? 0x60 0) (if correct-low? 0x06 0))
        new-a (bit-and 0xFF (if n? (- a adj) (+ a adj)))]
    (-> gb-state
        (assoc-in [:cpu :a] new-a)
        (util/update-flag util/Z-mask (zero? new-a))
        (util/unset-flag util/H-mask)
        (util/update-flag util/C-mask correct-high?)
        (util/inc-pc)
        (util/tick 4))))

(defmethod execute 0x28 JR_Z_r8
  [gb-state _]
  (util/jump-relative-pred-r8 gb-state #(util/flag-set? % util/Z-mask)))

(defmethod execute 0x29 ADD_HL_HL
  [gb-state _]
  (-> (util/add-hl gb-state (util/get16 gb-state :h :l))
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x2A LD_A_ADDR_HLI
  [gb-state _]
  (let [addr (util/get16 gb-state :h :l)
        val (bus/read-byte gb-state addr)]
    (-> gb-state
        (assoc-in [:cpu :a] val)
        (util/inc16 :h :l)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0x2B DEC_HL
  [gb-state _]
  (-> (util/dec-r16 gb-state :h :l)
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x2C INC_L
  [gb-state _]
  (-> (util/inc-r8 gb-state :l)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x2D DEC_L
  [gb-state _]
  (-> (util/dec-r8 gb-state :l)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x2E LD_L_N
  [gb-state _]
  (util/load8-immediate gb-state :l))

(defmethod execute 0x30 JR_NC_N
  [gb-state _]
  (let [pc (get-in gb-state [:cpu :pc])
        offset (util/as-signed-8 (bus/read-byte gb-state (inc pc)))
        jump? (not (util/flag-set? gb-state util/C-mask))]
    (cond-> (-> gb-state
                (util/inc-pc 2)
                (util/tick 8))
      jump? (-> (update-in [:cpu :pc] #(bit-and 0xFFFF (+ % offset)))
                (util/tick 4)))))

(defmethod execute 0x31 LD_SP_NN
  [gb-state _]
  (let [pc (get-in gb-state [:cpu :pc])
        nn (bus/read-word gb-state (inc pc))]
    (-> gb-state
        (assoc-in [:cpu :sp] nn)
        (util/inc-pc 3)
        (util/tick 12))))

(defmethod execute 0x32 LD_HL_DEC_ADDR_A
  [gb-state _]
  (let [addr (util/get16 gb-state :h :l)]
    (-> gb-state
        (bus/write-byte addr (get-in gb-state [:cpu :a]))
        (util/dec-r16 :h :l)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0x33 INC_SP
  [gb-state _]
  (-> gb-state
      (util/inc16 :sp)
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x35 DEC_ADDR_HL
  [gb-state _]
  (let [addr (util/get16 gb-state :h :l)
        prev (bus/read-byte gb-state addr)
        [val gb-state] (util/dec8 gb-state prev)]
    (-> (bus/write-byte gb-state addr val)
        (util/inc-pc)
        (util/tick 12))))

(defmethod execute 0x36 LD_ADDR_HL_N
  [gb-state _]
  (let [addr (util/get16 gb-state :h :l)]
    (util/load8-immediate gb-state addr)))

(defmethod execute 0x38 JR_C_r8
  [gb-state _]
  (util/jump-relative-pred-r8 gb-state #(util/flag-set? % util/C-mask)))

(defmethod execute 0x39 ADD_HL_SP
  [gb-state _]
  (-> (util/add-hl gb-state (get-in gb-state [:cpu :sp]))
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x3A LD_A_ADDR_HL_DEC
  [gb-state _]
  (let [addr (util/get16 gb-state :h :l)
        val (bus/read-byte gb-state addr)]
    (-> gb-state
        (assoc-in [:cpu :a] val)
        (util/dec-r16 :h :l)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0x3B DEC_SP
  [gb-state _]
  (-> (util/dec-r16 gb-state :sp)
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x3C INC_A
  [gb-state _]
  (-> (util/inc-r8 gb-state :a)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x3D DEC_A
  [gb-state _]
  (-> (util/dec-r8 gb-state :a)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x3E LD_A_N
  [gb-state _]
  (util/load8-immediate gb-state :a))

(defmethod execute 0x40 LD_B_B
  [gb-state _]
  (-> gb-state
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x41 LD_B_C
  [gb-state _]
  (-> gb-state
      (util/copy-register :c :b)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x42 LD_B_D
  [gb-state _]
  (-> gb-state
      (util/copy-register :d :b)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x43 LD_B_E
  [gb-state _]
  (-> gb-state
      (util/copy-register :e :b)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x44 LD_B_H
  [gb-state _]
  (-> gb-state
      (util/copy-register :h :b)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x45 LD_B_L
  [gb-state _]
  (-> gb-state
      (util/copy-register :l :b)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x46 LD_B_ADDR_HL
  [gb-state _]
  (util/load-r-from-addr16 gb-state :b :h :l))

(defmethod execute 0x47 LD_B_A
  [gb-state _]
  (-> (util/copy-register gb-state :a :b)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x48 LD_C_B
  [gb-state _]
  (-> (util/copy-register gb-state :b :c)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x49 LD_C_C
  [gb-state _]
  (-> gb-state
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x4A LD_C_D
  [gb-state _]
  (-> (util/copy-register gb-state :d :c)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x4B LD_C_E
  [gb-state _]
  (-> (util/copy-register gb-state :e :c)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x4C LD_C_H
  [gb-state _]
  (-> (util/copy-register gb-state :h :c)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x4D LD_C_L
  [gb-state _]
  (-> (util/copy-register gb-state :l :c)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x4E LD_C_ADDR_HL
  [gb-state _]
  (util/load-r-from-addr16 gb-state :c :h :l))

(defmethod execute 0x4F LD_C_A
  [gb-state _]
  (-> (util/copy-register gb-state :a :c)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x50 LD_D_B
  [gb-state _]
  (-> (util/copy-register gb-state :b :d)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x51 LD_D_C
  [gb-state _]
  (-> (util/copy-register gb-state :c :d)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x52 LD_D_D
  [gb-state _]
  (-> gb-state
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x53 LD_D_E
  [gb-state _]
  (-> (util/copy-register gb-state :e :d)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x54 LD_D_H
  [gb-state _]
  (-> (util/copy-register gb-state :h :d)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x55 LD_D_L
  [gb-state _]
  (-> (util/copy-register gb-state :l :d)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x56 LD_D_ADDR_HL
  [gb-state _]
  (util/load-r-from-addr16 gb-state :d :h :l))

(defmethod execute 0x57 LD_D_A
  [gb-state _]
  (-> (util/copy-register gb-state :a :d)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x58 LD_E_B
  [gb-state _]
  (-> (util/copy-register gb-state :b :e)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x59 LD_E_C
  [gb-state _]
  (-> (util/copy-register gb-state :c :e)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x5A LD_E_D
  [gb-state _]
  (-> (util/copy-register gb-state :d :e)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x5B LD_E_E
  [gb-state _]
  (-> gb-state
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x5C LD_E_H
  [gb-state _]
  (-> (util/copy-register gb-state :h :e)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x5D LD_E_L
  [gb-state _]
  (-> (util/copy-register gb-state :l :e)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x5E LD_E_ADDR_HL
  [gb-state _]
  (util/load-r-from-addr16 gb-state :e :h :l))

(defmethod execute 0x5F LD_E_A
  [gb-state _]
  (-> (util/copy-register gb-state :a :e)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x60 LD_H_B
  [gb-state _]
  (-> (util/copy-register gb-state :b :h)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x61 LD_H_C
  [gb-state _]
  (-> (util/copy-register gb-state :c :h)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x62 LD_H_D
  [gb-state _]
  (-> (util/copy-register gb-state :d :h)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x63 LD_H_E
  [gb-state _]
  (-> (util/copy-register gb-state :e :h)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x64 LD_H_H
  [gb-state _]
  (-> (util/copy-register gb-state :h :h)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x65 LD_H_L
  [gb-state _]
  (-> (util/copy-register gb-state :l :h)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x66 LD_H_ADDR_HL
  [gb-state _]
  (util/load-r-from-addr16 gb-state :h :h :l))

(defmethod execute 0x67 LD_H_A
  [gb-state _]
  (-> (util/copy-register gb-state :a :h)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x68 LD_L_B
  [gb-state _]
  (-> (util/copy-register gb-state :b :l)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x69 LD_L_C
  [gb-state _]
  (-> (util/copy-register gb-state :c :l)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x6A LD_L_D
  [gb-state _]
  (-> (util/copy-register gb-state :d :l)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x6B LD_L_E
  [gb-state _]
  (-> (util/copy-register gb-state :e :l)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x6C LD_L_H
  [gb-state _]
  (-> (util/copy-register gb-state :h :l)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x6D LD_L_L
  [gb-state _]
  (-> (util/copy-register gb-state :l :l)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x6E LD_L_ADDR_HL
  [gb-state _]
  (util/load-r-from-addr16 gb-state :l :h :l))

(defmethod execute 0x6F LD_L_A
  [gb-state _]
  (-> (util/copy-register gb-state :a :l)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x70 LD_ADDR_HL_B
  [gb-state _]
  (util/write-r-to-addr16 gb-state :b :h :l))

(defmethod execute 0x71 LD_ADDR_HL_C
  [gb-state _]
  (util/write-r-to-addr16 gb-state :c :h :l))

(defmethod execute 0x72 LD_ADDR_HL_D
  [gb-state _]
  (util/write-r-to-addr16 gb-state :d :h :l))

(defmethod execute 0x73 LD_ADDR_HL_E
  [gb-state _]
  (util/write-r-to-addr16 gb-state :e :h :l))

(defmethod execute 0x74 LD_ADDR_HL_H
  [gb-state _]
  (util/write-r-to-addr16 gb-state :h :h :l))

(defmethod execute 0x75 LD_ADDR_HL_L
  [gb-state _]
  (util/write-r-to-addr16 gb-state :l :h :l))

(defmethod execute 0x76 HALT
  [gb-state _]
  (if (and (not (get-in gb-state [:cpu :interrupts-enabled?]))
           (util/interrupt-pending? gb-state))
    (-> gb-state
        (assoc-in [:cpu :halt-bug?] true)
        (util/tick 4))
    (-> gb-state
        (assoc-in [:cpu :halted?] true)
        (util/inc-pc)
        (util/tick 4))))

(defmethod execute 0x77 LD_ADDR_HL_A
  [gb-state _]
  (let [addr (util/get16 gb-state :h :l)]
    (-> gb-state
        (bus/write-byte addr (get-in gb-state [:cpu :a]))
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0x78 LD_A_B
  [gb-state _]
  (-> (util/copy-register gb-state :b :a)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x79 LD_A_C
  [gb-state _]
  (-> (util/copy-register gb-state :c :a)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x7A LD_A_D
  [gb-state _]
  (-> (util/copy-register gb-state :d :a)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x7B LD_A_E
  [gb-state _]
  (-> (util/copy-register gb-state :e :a)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x7C LD_A_H
  [gb-state _]
  (-> (util/copy-register gb-state :h :a)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x7D LD_A_L
  [gb-state _]
  (-> (util/copy-register gb-state :l :a)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x7E LD_A_ADDR_HL
  [gb-state _]
  (util/load-r-from-addr16 gb-state :a :h :l))

(defmethod execute 0x88 ADC_A_B
  [gb-state _]
  (let [b (get-in gb-state [:cpu :b])]
    (-> (util/add-with-carry gb-state b)
        (util/inc-pc)
        (util/tick 4))))

(defmethod execute 0x89 ADC_A_C
  [gb-state _]
  (let [c (get-in gb-state [:cpu :c])]
    (-> (util/add-with-carry gb-state c)
        (util/inc-pc)
        (util/tick 4))))

(defmethod execute 0x8A ADC_A_D
  [gb-state _]
  (let [d (get-in gb-state [:cpu :d])]
    (-> (util/add-with-carry gb-state d)
        (util/inc-pc)
        (util/tick 4))))

(defmethod execute 0x8B ADC_A_E
  [gb-state _]
  (let [e (get-in gb-state [:cpu :e])]
    (-> (util/add-with-carry gb-state e)
        (util/inc-pc)
        (util/tick 4))))

(defmethod execute 0x8C ADC_A_H
  [gb-state _]
  (let [h (get-in gb-state [:cpu :h])]
    (-> (util/add-with-carry gb-state h)
        (util/inc-pc)
        (util/tick 4))))

(defmethod execute 0x8D ADC_A_L
  [gb-state _]
  (let [l (get-in gb-state [:cpu :l])]
    (-> (util/add-with-carry gb-state l)
        (util/inc-pc)
        (util/tick 4))))

(defmethod execute 0x8E ADC_A_ADDR_HL
  [gb-state _]
  (let [addr (util/get16 gb-state :h :l)
        val (bus/read-byte gb-state addr)]
    (-> (util/add-with-carry gb-state val)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0x8F ADC_A_A
  [gb-state _]
  (let [a (get-in gb-state [:cpu :a])]
    (-> (util/add-with-carry gb-state a)
        (util/inc-pc)
        (util/tick 4))))

(defn xor-val [gb-state v]
  (let [a (get-in gb-state [:cpu :a])
        val (bit-xor a v)]
    (-> gb-state
        (assoc-in [:cpu :a] val)
        (util/update-flag util/Z-mask (zero? val))
        (util/unset-flag util/N-mask)
        (util/unset-flag util/H-mask)
        (util/unset-flag util/C-mask))))

(defmethod execute 0x90 SUB_B
  [gb-state _]
  (-> (util/sub-val gb-state (get-in gb-state [:cpu :b]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x91 SUB_C
  [gb-state _]
  (-> (util/sub-val gb-state (get-in gb-state [:cpu :c]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x92 SUB_D
  [gb-state _]
  (-> (util/sub-val gb-state (get-in gb-state [:cpu :d]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x93 SUB_E
  [gb-state _]
  (-> (util/sub-val gb-state (get-in gb-state [:cpu :e]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x94 SUB_H
  [gb-state _]
  (-> (util/sub-val gb-state (get-in gb-state [:cpu :h]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x95 SUB_L
  [gb-state _]
  (-> (util/sub-val gb-state (get-in gb-state [:cpu :l]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x96 SUB_ADDR_HL
  [gb-state _]
  (let [addr (util/get16 gb-state :h :l)
        val (bus/read-byte gb-state addr)]
    (-> (util/sub-val gb-state val)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0x97 SUB_A
  [gb-state _]
  (-> (util/sub-val gb-state (get-in gb-state [:cpu :a]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x98 SBC_A_B
  [gb-state _]
  (-> (util/sub-with-carry gb-state (get-in gb-state [:cpu :b]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x99 SBC_A_C
  [gb-state _]
  (-> (util/sub-with-carry gb-state (get-in gb-state [:cpu :c]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x9A SBC_A_D
  [gb-state _]
  (-> (util/sub-with-carry gb-state (get-in gb-state [:cpu :d]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x9B SBC_A_E
  [gb-state _]
  (-> (util/sub-with-carry gb-state (get-in gb-state [:cpu :e]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x9C SBC_A_H
  [gb-state _]
  (-> (util/sub-with-carry gb-state (get-in gb-state [:cpu :h]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x9D SBC_A_L
  [gb-state _]
  (-> (util/sub-with-carry gb-state (get-in gb-state [:cpu :l]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x9E SBC_A_ADDR_HL
  [gb-state _]
  (let [addr (util/get16 gb-state :h :l)
        val (bus/read-byte gb-state addr)]
    (-> (util/sub-with-carry gb-state val)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0x9F SBC_A_A
  [gb-state _]
  (-> (util/sub-with-carry gb-state (get-in gb-state [:cpu :a]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xA8 XOR_B
  [gb-state _]
  (-> (xor-val gb-state (get-in gb-state [:cpu :b]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xA9 XOR_C
  [gb-state _]
  (-> (xor-val gb-state (get-in gb-state [:cpu :c]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xAA XOR_D
  [gb-state _]
  (-> (xor-val gb-state (get-in gb-state [:cpu :d]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xAB XOR_E
  [gb-state _]
  (-> (xor-val gb-state (get-in gb-state [:cpu :e]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xAC XOR_H
  [gb-state _]
  (-> (xor-val gb-state (get-in gb-state [:cpu :h]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xAD XOR_L
  [gb-state _]
  (-> (xor-val gb-state (get-in gb-state [:cpu :l]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xAE XOR_ADDR_HL
  [gb-state _]
  (let [addr (util/get16 gb-state :h :l)
        val (bus/read-byte gb-state addr)]
    (-> (xor-val gb-state val)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0xAF XOR_A
  [gb-state _]
  (-> (xor-val gb-state (get-in gb-state [:cpu :a]))
      (util/inc-pc)
      (util/tick 4)))

(defn or-val [gb-state v]
  (let [a (get-in gb-state [:cpu :a])
        val (bit-or a v)]
    (-> gb-state
        (assoc-in [:cpu :a] val)
        (util/update-flag util/Z-mask (zero? val))
        (util/unset-flag util/N-mask)
        (util/unset-flag util/H-mask)
        (util/unset-flag util/C-mask))))

(defmethod execute 0xB0 OR_B
  [gb-state _]
  (-> (or-val gb-state (get-in gb-state [:cpu :b]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xB1 OR_C
  [gb-state _]
  (-> (or-val gb-state (get-in gb-state [:cpu :c]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xB2 OR_D
  [gb-state _]
  (-> (or-val gb-state (get-in gb-state [:cpu :d]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xB3 OR_E
  [gb-state _]
  (-> (or-val gb-state (get-in gb-state [:cpu :e]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xB4 OR_H
  [gb-state _]
  (-> (or-val gb-state (get-in gb-state [:cpu :h]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xB5 OR_L
  [gb-state _]
  (-> (or-val gb-state (get-in gb-state [:cpu :l]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xB6 OR_ADDR_HL
  [gb-state _]
  (let [addr (util/get16 gb-state :h :l)
        val (bus/read-byte gb-state addr)]
    (-> (or-val gb-state val)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0xB7 OR_A
  [gb-state _]
  (-> (or-val gb-state (get-in gb-state [:cpu :a]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xB8 CP_B
  [gb-state _]
  (-> (compare-val gb-state (get-in gb-state [:cpu :b]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xB9 CP_C
  [gb-state _]
  (-> (compare-val gb-state (get-in gb-state [:cpu :c]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xBA CP_D
  [gb-state _]
  (-> (compare-val gb-state (get-in gb-state [:cpu :d]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xBB CP_E
  [gb-state _]
  (-> (compare-val gb-state (get-in gb-state [:cpu :e]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xBC CP_H
  [gb-state _]
  (-> (compare-val gb-state (get-in gb-state [:cpu :h]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xBD CP_L
  [gb-state _]
  (-> (compare-val gb-state (get-in gb-state [:cpu :l]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xBE CP_ADDR_HL
  [gb-state _]
  (-> (compare-val gb-state (bus/read-byte gb-state (util/get16 gb-state :h :l)))
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0xBF CP_A
  [gb-state _]
  (-> (compare-val gb-state (get-in gb-state [:cpu :a]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xC0 RET_NZ
  [gb-state _]
  (util/maybe-ret gb-state #(not (util/flag-set? % util/Z-mask))))

(defmethod execute 0xC1 POP_BC
  [gb-state _]
  (-> gb-state
      (util/pop-r-16 :b :c)
      (util/inc-pc)
      (util/tick 12)))

(defmethod execute 0xC2 JP_NZ_NN
  [gb-state _]
  (util/jump-relative-pred-a16 gb-state (complement #(util/flag-set? % util/Z-mask))))

(defmethod execute 0xC3 JP_NN
  [gb-state _]
  (let [addr (bus/read-word gb-state (inc (get-in gb-state [:cpu :pc])))]
    (-> (assoc-in gb-state [:cpu :pc] addr)
        (util/tick 16))))

(defmethod execute 0xC4 CALL_NZ_NN
  [gb-state _]
  (let [pc (get-in gb-state [:cpu :pc])
        return-addr (bit-and 0xFFFF (+ pc 3))
        target-addr (bus/read-word gb-state (inc pc))]
    (if (util/flag-set? gb-state util/Z-mask)
      (-> gb-state
          (util/inc-pc 3)
          (util/tick 12))
      (-> gb-state
          (util/push-val-16 return-addr)
          (assoc-in [:cpu :pc] target-addr)
          (util/tick 24)))))

(defmethod execute 0xC5 PUSH_BC
  [gb-state _]
  (-> gb-state
      (util/push-r-16 :b :c)
      (util/inc-pc)
      (util/tick 16)))

(defmethod execute 0xC6 ADD_A_N
  [gb-state _]
  (let [a (get-in gb-state [:cpu :a])
        n (bus/read-byte gb-state (inc (get-in gb-state [:cpu :pc])))
        val (bit-and 0xFF (+ a n))]
    (-> gb-state
        (assoc-in [:cpu :a] val)
        (util/update-flag util/Z-mask (zero? val))
        (util/unset-flag util/N-mask)
        (util/update-flag util/H-mask (util/half-carry? a n val))
        (util/update-flag util/C-mask (> (+ a n) 0xFF))
        (util/inc-pc 2)
        (util/tick 8))))

(defmethod execute 0xC8 RET_Z
  [gb-state _]
  (util/maybe-ret gb-state #(util/flag-set? % util/Z-mask)))

(defmethod execute 0xC9 RET
  [gb-state _]
  (let [[[high low] gb-state] (util/pop-val-16 gb-state)]
    (-> gb-state
        (assoc-in [:cpu :pc] (bits/combine high low))
        (util/tick 16))))

(defmethod execute 0xCA JP_Z_NN
  [gb-state _]
  (util/jump-relative-pred-a16 gb-state #(util/flag-set? % util/Z-mask)))

(defmethod execute 0xCB PREFIX_CB
  [gb-state _]
  (let [pc (get-in gb-state [:cpu :pc])
        sub-opcode (bus/read-byte gb-state (inc pc))]
    (-> gb-state
        (util/inc-pc 2)
        (util/tick 8)
        (prefix/execute-prefix sub-opcode))))

(defmethod execute 0xCD CALL_NN
  [gb-state _]
  (let [pc (get-in gb-state [:cpu :pc])
        return-addr (bit-and 0xFFFF (+ pc 3))
        target-addr (bus/read-word gb-state (inc pc))]
    (-> gb-state
        (util/push-val-16 return-addr)
        (assoc-in [:cpu :pc] target-addr)
        (util/tick 24))))

(defmethod execute 0xCE ADC_A_N
  [gb-state _]
  (let [n (bus/read-byte gb-state (inc (get-in gb-state [:cpu :pc])))]
    (-> (util/add-with-carry gb-state n)
        (util/inc-pc 2)
        (util/tick 8))))

(defmethod execute 0xD0 RET_NC
  [gb-state _]
  (util/maybe-ret gb-state #(not (util/flag-set? % util/C-mask))))

(defmethod execute 0xD1 POP_DE
  [gb-state _]
  (-> gb-state
      (util/pop-r-16 :d :e)
      (util/inc-pc)
      (util/tick 12)))

(defmethod execute 0xD2 JP_NC_NN
  [gb-state _]
  (util/jump-relative-pred-a16 gb-state (complement #(util/flag-set? % util/C-mask))))

(defmethod execute 0xD5 PUSH_DE
  [gb-state _]
  (-> gb-state
      (util/push-r-16 :d :e)
      (util/inc-pc)
      (util/tick 16)))

(defmethod execute 0xD6 SUB_A_N
  [gb-state _]
  (let [a (get-in gb-state [:cpu :a])
        n (bus/read-byte gb-state (inc (get-in gb-state [:cpu :pc])))
        val (bit-and 0xFF (- a n))]
    (-> gb-state
        (assoc-in [:cpu :a] val)
        (util/update-flag util/Z-mask (zero? val))
        (util/set-flag util/N-mask)
        (util/update-flag util/H-mask (util/half-carry? a n val))
        (util/update-flag util/C-mask (> n a))
        (util/inc-pc 2)
        (util/tick 8))))

(defmethod execute 0xD8 RET_C
  [gb-state _]
  (util/maybe-ret gb-state #(util/flag-set? % util/C-mask)))

(defmethod execute 0xD9 RETI
  [gb-state _]
  (-> (util/maybe-ret gb-state (constantly true))
      (assoc-in [:cpu :interrupts-enabled?] true)))

(defmethod execute 0xDE SBC_A_N
  [gb-state _]
  (let [n (bus/read-byte gb-state (inc (get-in gb-state [:cpu :pc])))]
    (-> (util/sub-with-carry gb-state n)
        (util/inc-pc 2)
        (util/tick 8))))

(defmethod execute 0xDA JP_C_NN
  [gb-state _]
  (util/jump-relative-pred-a16 gb-state #(util/flag-set? % util/C-mask)))

(defmethod execute 0xE0 LDH_ADDR_A8_A
  [gb-state _]
  (let [addr (-> (bus/read-byte gb-state (inc (get-in gb-state [:cpu :pc])))
                 (+ 0xFF00))]
    (-> gb-state
        (bus/write-byte addr (get-in gb-state [:cpu :a]))
        (util/inc-pc 2)
        (util/tick 12))))

(defmethod execute 0xE1 POP_HL
  [gb-state _]
  (-> gb-state
      (util/pop-r-16 :h :l)
      (util/inc-pc)
      (util/tick 12)))

(defmethod execute 0xE2 LD_ADDR_C_A
  [gb-state _]
  (let [addr (+ 0xFF00 (get-in gb-state [:cpu :c]))]
    (-> gb-state
        (bus/write-byte addr (get-in gb-state [:cpu :a]))
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0xE5 PUSH_HL
  [gb-state _]
  (-> gb-state
      (util/push-r-16 :h :l)
      (util/inc-pc)
      (util/tick 16)))

(defmethod execute 0xE6 AND_N
  [gb-state _]
  (let [a (get-in gb-state [:cpu :a])
        n (bus/read-byte gb-state (inc (get-in gb-state [:cpu :pc])))
        val (bit-and a n)]
    (-> gb-state
        (assoc-in [:cpu :a] val)
        (util/update-flag util/Z-mask (zero? val))
        (util/unset-flag util/N-mask)
        (util/set-flag util/H-mask)
        (util/unset-flag util/C-mask)
        (util/inc-pc 2)
        (util/tick 8))))

(defmethod execute 0xE8 ADD_SP_r8
  [gb-state _]
  (let [sp (get-in gb-state [:cpu :sp])
        raw (bus/read-byte gb-state (inc (get-in gb-state [:cpu :pc])))
        offset (util/as-signed-8 raw)
        result (bit-and 0xFFFF (+ sp offset))
        h? (> (+ (bit-and sp 0xF) (bit-and raw 0xF)) 0xF)
        c? (> (+ (bit-and sp 0xFF) (bit-and raw 0xFF)) 0xFF)]
    (-> gb-state
        (assoc-in [:cpu :sp] result)
        (util/unset-flag util/Z-mask)
        (util/unset-flag util/N-mask)
        (util/update-flag util/H-mask h?)
        (util/update-flag util/C-mask c?)
        (util/inc-pc 2)
        (util/tick 16))))

(defmethod execute 0xE9 JP_HL
  [gb-state _]
  (-> gb-state
      (assoc-in [:cpu :pc] (util/get16 gb-state :h :l))
      (util/tick 4)))

(defmethod execute 0xEA LD_ADDR_A16_A
  [gb-state _]
  (let [addr (bus/read-word gb-state (inc (get-in gb-state [:cpu :pc])))]
    (-> gb-state
        (bus/write-byte addr (get-in gb-state [:cpu :a]))
        (util/inc-pc 3)
        (util/tick 16))))

(defmethod execute 0xEE XOR_N
  [gb-state _]
  (let [val (bus/read-byte gb-state (inc (get-in gb-state [:cpu :pc])))]
    (-> (xor-val gb-state val)
        (util/inc-pc 2)
        (util/tick 8))))

(defmethod execute 0xF0 LDH_A_ADDR_A8
  [gb-state _]
  (let [addr (-> (bus/read-byte gb-state (inc (get-in gb-state [:cpu :pc])))
                 (+ 0xFF00))
        val (bus/read-byte gb-state addr)]
    (-> gb-state
        (assoc-in [:cpu :a] val)
        (util/inc-pc 2)
        (util/tick 12))))

(defmethod execute 0xF1 POP_AF
  [gb-state _]
  (-> gb-state
      (util/pop-r-16 :a :f)
      (util/inc-pc)
      (util/tick 12)))

(defmethod execute 0xF2 LD_A_ADDR_C
  [gb-state _]
  (let [addr (-> (get-in gb-state [:cpu :c])
                 (+ 0xFF00))
        val (bus/read-byte gb-state addr)]
    (-> gb-state
        (assoc-in [:cpu :a] val)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0xF3 DI
  [gb-state _]
  (-> gb-state
      (assoc-in [:cpu :interrupts-enabled?] false)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xF5 PUSH_AF
  [gb-state _]
  (-> gb-state
      (util/push-r-16 :a :f)
      (util/inc-pc)
      (util/tick 16)))

(defmethod execute 0xF6 OR_d8
  [gb-state _]
  (let [val (bus/read-byte gb-state (inc (get-in gb-state [:cpu :pc])))]
    (-> (or-val gb-state val)
        (util/inc-pc 2)
        (util/tick 8))))

(defmethod execute 0xF8 LD_HL_SP_r8
  [gb-state _]
  (let [sp (get-in gb-state [:cpu :sp])
        raw (bus/read-byte gb-state (inc (get-in gb-state [:cpu :pc])))
        offset (util/as-signed-8 raw)
        result (bit-and 0xFFFF (+ sp offset))
        h? (> (+ (bit-and sp 0xF) (bit-and raw 0xF)) 0xF)
        c? (> (+ (bit-and sp 0xFF) (bit-and raw 0xFF)) 0xFF)]
    (-> gb-state
        (util/set16 :h :l result)
        (util/unset-flag util/Z-mask)
        (util/unset-flag util/N-mask)
        (util/update-flag util/H-mask h?)
        (util/update-flag util/C-mask c?)
        (util/inc-pc 2)
        (util/tick 12))))

(defmethod execute 0xF9 LD_SP_HL
  [gb-state _]
  (-> gb-state
      (assoc-in [:cpu :sp] (util/get16 gb-state :h :l))
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0xFA LD_A_ADDR_A16
  [gb-state _]
  (let [addr (bus/read-word gb-state (inc (get-in gb-state [:cpu :pc])))
        val (bus/read-byte gb-state addr)]
    (-> gb-state
        (assoc-in [:cpu :a] val)
        (util/inc-pc 3)
        (util/tick 16))))

(defmethod execute 0xFB EI
  [gb-state _]
  (-> gb-state
      (assoc-in [:cpu :ime-pending?] true)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xFE CP_N
  [gb-state _]
  (let [val (bus/read-byte gb-state (inc (get-in gb-state [:cpu :pc])))]
    (-> (compare-val gb-state val)
        (util/inc-pc 2)
        (util/tick 8))))

(defmethod execute :default
  [gb-state opcode]
  (throw (Exception. (format "Opcode 0x%02X not implemented at PC: 0x%04X"
                             opcode (get-in gb-state [:cpu :pc])))))


