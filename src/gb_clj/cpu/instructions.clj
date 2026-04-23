(ns gb-clj.cpu.instructions
  (:require
   [clojure.tools.logging :as log]
   [gb-clj.bus :as bus]
   [gb-clj.cpu.prefix-instructions :as prefix]
   [gb-clj.cpu.util :as util]))

(defn rotate-a [state rotate-fn]
  (let [old-a (get-in state [:cpu :a])
        old-c (if (util/flag-set? state util/C-mask) 1 0)
        [new-a new-c] (rotate-fn old-a old-c)]
    (-> state
        (assoc-in [:cpu :a] new-a)
        (util/unset-flag util/Z-mask)
        (util/unset-flag util/N-mask)
        (util/unset-flag util/H-mask)
        (util/update-flag util/C-mask (pos? new-c)))))

(defn compare-val [state val]
  (let [acc (get-in state [:cpu :a])
        result (- acc val)
        half-carry? (< (bit-and 0x0F acc) (bit-and 0x0F val))]
    (-> state
        (util/update-flag util/Z-mask (zero? result))
        (util/set-flag util/N-mask)
        (util/update-flag util/H-mask half-carry?)
        (util/update-flag util/C-mask (< acc val)))))

(defmulti execute (fn [_state opcode] opcode))

(defmethod execute 0x00 NOP
  [state _]
  (-> (util/inc-pc state)
      (util/tick 4)))

(defmethod execute 0x01 LD_BC_NN
  [state _]
  (-> (util/load16-immediate state :b :c)
      (util/tick 12)))

(defmethod execute 0x02 LD_ADDR_BC_A
  [state _]
  (util/write-r-to-addr16 state :a :b :c))

(defmethod execute 0x03 INC_BC
  [state _]
  (-> (util/inc16 state :b :c)
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x04 INC_B
  [state _]
  (-> (util/inc-r8 state :b)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x05 DEC_B
  [state _]
  (-> (util/dec-r8 state :b)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x06 LD_B_N
  [state _]
  (util/load8-immediate state :b))

(defmethod execute 0x07 RLC_A
  [state _]
  (-> (rotate-a state util/rotate-left-circular)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x09 ADD_HL_BC
  [state _]
  (-> (util/add-hl state (util/get16 state :b :c))
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x0A LD_A_ADDR_BC
  [state _]
  (util/load-r-from-addr16 state :a :b :c))

(defmethod execute 0x0B DEC_BC
  [state _]
  (-> (util/dec-r16 state :b :c)
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x0C INC_C
  [state _]
  (-> (util/inc-r8 state :c)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x0D DEC_C
  [state _]
  (-> (util/dec-r8 state :c)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x0E LD_C_N
  [state _]
  (util/load8-immediate state :c))

(defmethod execute 0x0F RRC_A
  [state _]
  (-> (rotate-a state util/rotate-right-circular)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x11 LD_DE_NN
  [state _]
  (-> (util/load16-immediate state :d :e)
      (util/tick 12)))

(defmethod execute 0x12 LD_ADDR_DE_A
  [state _]
  (util/write-r-to-addr16 state :a :d :e))

(defmethod execute 0x13 INC_DE
  [state _]
  (-> (util/inc16 state :d :e)
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x14 INC_D
  [state _]
  (-> (util/inc-r8 state :d)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x15 DEC_D
  [state _]
  (-> (util/dec-r8 state :d)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x16 LD_D_N
  [state _]
  (util/load8-immediate state :d))

(defmethod execute 0x17 RL_A
  [state _]
  (-> (rotate-a state util/rotate-thru-carry-left)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x18 JR_r8
  [state _]
  (let [offset (util/as-signed-8 (bus/read-byte state (inc (get-in state [:cpu :pc]))))]
    (-> state
        (util/inc-pc (+ 2 offset))
        (util/tick 12))))

(defmethod execute 0x19 ADD_HL_DE
  [state _]
  (-> (util/add-hl state (util/get16 state :d :e))
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x1A LD_A_ADDR_DE
  [state _]
  (util/load-r-from-addr16 state :a :d :e))

(defmethod execute 0x1B DEC_DE
  [state _]
  (-> (util/dec-r16 state :d :e)
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x1C INC_E
  [state _]
  (-> (util/inc-r8 state :e)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x1D DEC_E
  [state _]
  (-> (util/dec-r8 state :e)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x1E LD_E_N
  [state _]
  (util/load8-immediate state :e))

(defmethod execute 0x1F RR_A
  [state _]
  (-> (rotate-a state util/rotate-thru-carry-right)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x20 JR_NZ_r8
  [state _]
  (util/jump-relative-pred-r8 state #(not (util/flag-set? % util/Z-mask))))

(defmethod execute 0x21 LD_HL_NN
  [state _]
  (-> (util/load16-immediate state :h :l)
      (util/tick 12)))

(defmethod execute 0x22 LD_HL_INC_ADDR_A
  [state _]
  (let [addr (util/get16 state :h :l)]
    (-> state
        (bus/write-byte addr (get-in state [:cpu :a]))
        (util/inc16 :h :l)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0x23 INC_HL
  [state _]
  (-> (util/inc16 state :h :l)
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x24 INC_H
  [state _]
  (-> (util/inc-r8 state :h)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x25 DEC_H
  [state _]
  (-> (util/dec-r8 state :h)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x26 LD_H_N
  [state _]
  (util/load8-immediate state :h))

(defmethod execute 0x28 JR_Z_r8
  [state _]
  (util/jump-relative-pred-r8 state #(util/flag-set? % util/Z-mask)))

(defmethod execute 0x29 ADD_HL_HL
  [state _]
  (-> (util/add-hl state (util/get16 state :h :l))
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x2A LD_A_ADDR_HLI
  [state _]
  (let [addr (util/get16 state :h :l)
        val (bus/read-byte state addr)]
    (-> state
        (assoc-in [:cpu :a] val)
        (util/inc16 :h :l)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0x2B DEC_HL
  [state _]
  (-> (util/dec-r16 state :h :l)
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x2C INC_L
  [state _]
  (-> (util/inc-r8 state :l)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x2D DEC_L
  [state _]
  (-> (util/dec-r8 state :l)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x2E LD_L_N
  [state _]
  (util/load8-immediate state :l))

(defmethod execute 0x30 JR_NC_N
  [state _]
  (let [pc (get-in state [:cpu :pc])
        offset (util/as-signed-8 (bus/read-byte state (inc pc)))
        jump? (not (util/flag-set? state util/C-mask))]
    (cond-> (-> state
                (util/inc-pc 2)
                (util/tick 8))
      jump? (-> (update-in [:cpu :pc] #(bit-and 0xFFFF (+ % offset)))
                (util/tick 4)))))

(defmethod execute 0x31 LD_SP_NN
  [state _]
  (let [pc (get-in state [:cpu :pc])
        nn (bus/read-word state (inc pc))]
    (-> state
        (assoc-in [:cpu :sp] nn)
        (util/inc-pc 3)
        (util/tick 12))))

(defmethod execute 0x32  LD_HL_DEC_ADDR_A
  [state _]
  (let [addr (util/get16 state :h :l)]
    (-> state
        (bus/write-byte addr (get-in state [:cpu :a]))
        (util/dec-r16 :h :l)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0x35 DEC_ADDR_HL
  [state _]
  (let [addr (util/get16 state :h :l)
        prev (bus/read-byte state addr)
        [val state] (util/dec8 state prev)]
    (-> (bus/write-byte state addr val)
        (util/inc-pc)
        (util/tick 12))))

(defmethod execute 0x36 LD_ADDR_HL_N
  [state _]
  (let [addr (util/get16 state :h :l)]
    (util/load8-immediate state addr)))

(defmethod execute 0x39 ADD_HL_SP
  [state _]
  (-> (util/add-hl state (get-in state [:cpu :sp]))
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x3A LD_A_ADDR_HL_DEC
  [state _]
  (let [addr (util/get16 state :h :l)
        val (bus/read-byte state addr)]
    (-> state
        (assoc-in [:cpu :a] val)
        (util/dec-r16 :h :l)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0x3B DEC_SP
  [state _]
  (-> (util/dec-r16 state :sp)
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0x3C INC_A
  [state _]
  (-> (util/inc-r8 state :a)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x3D DEC_A
  [state _]
  (-> (util/dec-r8 state :a)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x3E LD_A_N
  [state _]
  (util/load8-immediate state :a))

(defmethod execute 0x40 LD_B_B
  [state _]
  (-> state
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x41 LD_B_C
  [state _]
  (-> state
      (util/copy-register :c :b)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x42 LD_B_D
  [state _]
  (-> state
      (util/copy-register :d :b)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x43 LD_B_E
  [state _]
  (-> state
      (util/copy-register :e :b)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x44 LD_B_H
  [state _]
  (-> state
      (util/copy-register :h :b)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x45 LD_B_L
  [state _]
  (-> state
      (util/copy-register :l :b)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x46 LD_B_ADDR_HL
  [state _]
  (util/load-r-from-addr16 state :b :h :l))

(defmethod execute 0x47 LD_B_A
  [state _]
  (-> (util/copy-register state :a :b)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x48 LD_C_B
  [state _]
  (-> (util/copy-register state :b :c)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x49 LD_C_C
  [state _]
  (-> state
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x4A LD_C_D
  [state _]
  (-> (util/copy-register state :d :c)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x4B LD_C_E
  [state _]
  (-> (util/copy-register state :e :c)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x4C LD_C_H
  [state _]
  (-> (util/copy-register state :h :c)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x4D LD_C_L
  [state _]
  (-> (util/copy-register state :l :c)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x4E LD_C_ADDR_HL
  [state _]
  (util/load-r-from-addr16 state :c :h :l))

(defmethod execute 0x4F LD_C_A
  [state _]
  (-> (util/copy-register state :a :c)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x50 LD_D_B
  [state _]
  (-> (util/copy-register state :b :d)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x51 LD_D_C
  [state _]
  (-> (util/copy-register state :c :d)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x52 LD_D_D
  [state _]
  (-> state
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x53 LD_D_E
  [state _]
  (-> (util/copy-register state :e :d)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x54 LD_D_H
  [state _]
  (-> (util/copy-register state :h :d)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x55 LD_D_L
  [state _]
  (-> (util/copy-register state :l :d)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x56 LD_D_ADDR_HL
  [state _]
  (util/load-r-from-addr16 state :d :h :l))

(defmethod execute 0x57 LD_D_A
  [state _]
  (-> (util/copy-register state :a :d)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x58 LD_E_B
  [state _]
  (-> (util/copy-register state :b :e)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x59 LD_E_C
  [state _]
  (-> (util/copy-register state :c :e)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x5A LD_E_D
  [state _]
  (-> (util/copy-register state :d :e)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x5B LD_E_E
  [state _]
  (-> state
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x5C LD_E_H
  [state _]
  (-> (util/copy-register state :h :e)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x5D LD_E_L
  [state _]
  (-> (util/copy-register state :l :e)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x5E LD_E_ADDR_HL
  [state _]
  (util/load-r-from-addr16 state :e :h :l))

(defmethod execute 0x5F LD_E_A
  [state _]
  (-> (util/copy-register state :a :e)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x66 LD_H_ADDR_HL
  [state _]
  (util/load-r-from-addr16 state :h :h :l))

(defmethod execute 0x6E LD_L_ADDR_HL
  [state _]
  (util/load-r-from-addr16 state :l :h :l))

(defmethod execute 0x6F LD_L_A
  [state _]
  (-> (util/copy-register state :a :l)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x70 LD_ADDR_HL_B
  [state _]
  (util/write-r-to-addr16 state :b :h :l))

(defmethod execute 0x71 LD_ADDR_HL_C
  [state _]
  (util/write-r-to-addr16 state :c :h :l))

(defmethod execute 0x72 LD_ADDR_HL_D
  [state _]
  (util/write-r-to-addr16 state :d :h :l))

(defmethod execute 0x73 LD_ADDR_HL_E
  [state _]
  (util/write-r-to-addr16 state :e :h :l))

(defmethod execute 0x74 LD_ADDR_HL_H
  [state _]
  (util/write-r-to-addr16 state :h :h :l))

(defmethod execute 0x75 LD_ADDR_HL_L
  [state _]
  (util/write-r-to-addr16 state :l :h :l))

(defmethod execute 0x77 LD_ADDR_HL_A
  [state _]
  (let [addr (util/get16 state :h :l)]
    (-> state
        (bus/write-byte addr (get-in state [:cpu :a]))
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0x78 LD_A_B
  [state _]
  (-> (util/copy-register state :b :a)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x79 LD_A_C
  [state _]
  (-> (util/copy-register state :c :a)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x7A LD_A_D
  [state _]
  (-> (util/copy-register state :d :a)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x7B LD_A_E
  [state _]
  (-> (util/copy-register state :e :a)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x7C LD_A_H
  [state _]
  (-> (util/copy-register state :h :a)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x7D LD_A_L
  [state _]
  (-> (util/copy-register state :l :a)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0x7E LD_A_ADDR_HL
  [state _]
  (util/load-r-from-addr16 state :a :h :l))

(defmethod execute 0x88 ADC_A_B
  [state _]
  (let [b (get-in state [:cpu :b])]
    (-> (util/add-with-carry state b)
        (util/inc-pc)
        (util/tick 4))))

(defmethod execute 0x89 ADC_A_C
  [state _]
  (let [c (get-in state [:cpu :c])]
    (-> (util/add-with-carry state c)
        (util/inc-pc)
        (util/tick 4))))

(defmethod execute 0x8A ADC_A_D
  [state _]
  (let [d (get-in state [:cpu :d])]
    (-> (util/add-with-carry state d)
        (util/inc-pc)
        (util/tick 4))))

(defmethod execute 0x8B ADC_A_E
  [state _]
  (let [e (get-in state [:cpu :e])]
    (-> (util/add-with-carry state e)
        (util/inc-pc)
        (util/tick 4))))

(defmethod execute 0x8C ADC_A_H
  [state _]
  (let [h (get-in state [:cpu :h])]
    (-> (util/add-with-carry state h)
        (util/inc-pc)
        (util/tick 4))))

(defmethod execute 0x8D ADC_A_L
  [state _]
  (let [l (get-in state [:cpu :l])]
    (-> (util/add-with-carry state l)
        (util/inc-pc)
        (util/tick 4))))

(defmethod execute 0x8E ADC_A_ADDR_HL
  [state _]
  (let [addr (util/get16 state :h :l)
        val (bus/read-byte state addr)]
    (-> (util/add-with-carry state val)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0x8F ADC_A_A
  [state _]
  (let [a (get-in state [:cpu :a])]
    (-> (util/add-with-carry state a)
        (util/inc-pc)
        (util/tick 4))))

(defn xor-val [state v]
  (let [a (get-in state [:cpu :a])
        val (bit-xor a v)]
    (-> state
        (assoc-in [:cpu :a] val)
        (util/update-flag util/Z-mask (zero? val))
        (util/unset-flag util/N-mask)
        (util/unset-flag util/H-mask)
        (util/unset-flag util/C-mask))))

(defmethod execute 0xA8 XOR_B
  [state _]
  (-> (xor-val state (get-in state [:cpu :b]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xA9 XOR_C
  [state _]
  (-> (xor-val state (get-in state [:cpu :c]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xAA XOR_D
  [state _]
  (-> (xor-val state (get-in state [:cpu :d]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xAB XOR_E
  [state _]
  (-> (xor-val state (get-in state [:cpu :e]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xAC XOR_H
  [state _]
  (-> (xor-val state (get-in state [:cpu :h]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xAD XOR_L
  [state _]
  (-> (xor-val state (get-in state [:cpu :l]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xAE XOR_ADDR_HL
  [state _]
  (let [addr (util/get16 state :h :l)
        val (bus/read-byte state addr)]
    (-> (xor-val state val)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0xAF XOR_A
  [state _]
  (-> (xor-val state (get-in state [:cpu :a]))
      (util/inc-pc)
      (util/tick 4)))

(defn or-val [state v]
  (let [a (get-in state [:cpu :a])
        val (bit-or a v)]
    (-> state
        (assoc-in [:cpu :a] val)
        (util/update-flag util/Z-mask (zero? val))
        (util/unset-flag util/N-mask)
        (util/unset-flag util/H-mask)
        (util/unset-flag util/C-mask))))

(defmethod execute 0xB1 OR_C
  [state _]
  (-> (or-val state (get-in state [:cpu :c]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xB6 OR_ADDR_HL
  [state _]
  (let [addr (util/get16 state :h :l)
        val (bus/read-byte state addr)]
    (-> (or-val state val)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0xB7 OR_A
  [state _]
  (-> (or-val state (get-in state [:cpu :a]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xB8 CP_B
  [state _]
  (-> (compare-val state (get-in state [:cpu :b]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xB9 CP_C
  [state _]
  (-> (compare-val state (get-in state [:cpu :c]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xBA CP_D
  [state _]
  (-> (compare-val state (get-in state [:cpu :d]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xBB CP_E
  [state _]
  (-> (compare-val state (get-in state [:cpu :e]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xBC CP_H
  [state _]
  (-> (compare-val state (get-in state [:cpu :h]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xBD CP_L
  [state _]
  (-> (compare-val state (get-in state [:cpu :l]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xBE CP_ADDR_HL
  [state _]
  (-> (compare-val state (bus/read-byte state (util/get16 state :h :l)))
      (util/inc-pc)
      (util/tick 8)))

(defmethod execute 0xBF CP_A
  [state _]
  (-> (compare-val state (get-in state [:cpu :a]))
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xC0 RET_NZ
  [state _]
  (util/maybe-ret state #(not (util/flag-set? % util/Z-mask))))

(defmethod execute 0xC1 POP_BC
  [state _]
  (-> state
      (util/pop-r-16 :b :c)
      (util/inc-pc)
      (util/tick 12)))

(defmethod execute 0xC2 JP_NZ_NN
  [state _]
  (util/jump-relative-pred-a16 state (complement #(util/flag-set? % util/Z-mask))))

(defmethod execute 0xC3 JP_NN
  [state _]
  (let [addr (bus/read-word state (inc (get-in state [:cpu :pc])))]
    (-> (assoc-in state [:cpu :pc] addr)
        (util/tick 16))))

(defmethod execute 0xC4 CALL_NZ_NN
  [state _]
  (let [pc (get-in state [:cpu :pc])
        return-addr (bit-and 0xFFFF (+ pc 3))
        target-addr (bus/read-word state (inc pc))]
    (if (util/flag-set? state util/Z-mask)
      (-> state
          (util/inc-pc 3)
          (util/tick 12))
      (-> state
          (util/push-val-16 return-addr)
          (assoc-in [:cpu :pc] target-addr)
          (util/tick 24)))))

(defmethod execute 0xC5 PUSH_BC
  [state _]
  (-> state
      (util/push-r-16 :b :c)
      (util/inc-pc)
      (util/tick 16)))

(defmethod execute 0xC6 ADD_A_N
  [state _]
  (let [a (get-in state [:cpu :a])
        n (bus/read-byte state (inc (get-in state [:cpu :pc])))
        val (bit-and 0xFF (+ a n))]
    (-> state
        (assoc-in [:cpu :a] val)
        (util/update-flag util/Z-mask (zero? val))
        (util/unset-flag util/N-mask)
        (util/update-flag util/H-mask (util/half-carry? a n val))
        (util/update-flag util/C-mask (> (+ a n) 0xFF))
        (util/inc-pc 2)
        (util/tick 8))))

(defmethod execute 0xC8 RET_Z
  [state _]
  (util/maybe-ret state #(util/flag-set? % util/Z-mask)))

(defmethod execute 0xC9 RET
  [state _]
  (let [[[high low] state] (util/pop-val-16 state)]
    (-> state
        (assoc-in [:cpu :pc] (util/combine high low))
        (util/tick 16))))

(defmethod execute 0xCA JP_Z_NN
  [state _]
  (util/jump-relative-pred-a16 state #(util/flag-set? % util/Z-mask)))

(defmethod execute 0xCB PREFIX_CB
  [state _]
  (let [pc (get-in state [:cpu :pc])
        sub-opcode (bus/read-byte state (inc pc))]
    (-> state
        (util/inc-pc 2)
        (util/tick 8)
        (prefix/execute-prefix sub-opcode))))

(defmethod execute 0xCD CALL_NN
  [state _]
  (let [pc (get-in state [:cpu :pc])
        return-addr (bit-and 0xFFFF (+ pc 3))
        target-addr (bus/read-word state (inc pc))]
    (-> state
        (util/push-val-16 return-addr)
        (assoc-in [:cpu :pc] target-addr)
        (util/tick 24))))

(defmethod execute 0xCE ADC_A_N
  [state _]
  (let [n (bus/read-byte state (inc (get-in state [:cpu :pc])))]
    (-> (util/add-with-carry state n)
        (util/inc-pc 2)
        (util/tick 8))))

(defmethod execute 0xD0 RET_NC
  [state _]
  (util/maybe-ret state #(not (util/flag-set? % util/C-mask))))

(defmethod execute 0xD1 POP_DE
  [state _]
  (-> state
      (util/pop-r-16 :d :e)
      (util/inc-pc)
      (util/tick 12)))

(defmethod execute 0xD2 JP_NC_NN
  [state _]
  (util/jump-relative-pred-a16 state (complement #(util/flag-set? % util/C-mask))))

(defmethod execute 0xD5 PUSH_DE
  [state _]
  (-> state
      (util/push-r-16 :d :e)
      (util/inc-pc)
      (util/tick 16)))

(defmethod execute 0xD6 SUB_A_N
  [state _]
  (let [a (get-in state [:cpu :a])
        n (bus/read-byte state (inc (get-in state [:cpu :pc])))
        val (bit-and 0xFF (- a n))]
    (-> state
        (assoc-in [:cpu :a] val)
        (util/update-flag util/Z-mask (zero? val))
        (util/set-flag util/N-mask)
        (util/update-flag util/H-mask (util/half-carry? a n val))
        (util/update-flag util/C-mask (> n a))
        (util/inc-pc 2)
        (util/tick 8))))

(defmethod execute 0xD8 RET_C
  [state _]
  (util/maybe-ret state #(util/flag-set? % util/C-mask)))

(defmethod execute 0xDA JP_C_NN
  [state _]
  (util/jump-relative-pred-a16 state #(util/flag-set? % util/C-mask)))

(defmethod execute 0xE0 LDH_ADDR_A8_A
  [state _]
  (let [addr (-> (bus/read-byte state (inc (get-in state [:cpu :pc])))
                 (+ 0xFF00))]
    (-> state
        (bus/write-byte addr (get-in state [:cpu :a]))
        (util/inc-pc 2)
        (util/tick 12))))

(defmethod execute 0xE1 POP_HL
  [state _]
  (-> state
      (util/pop-r-16 :h :l)
      (util/inc-pc)
      (util/tick 12)))

(defmethod execute 0xE2 LD_ADDR_C_A
  [state _]
  (let [addr (+ 0xFF00 (get-in state [:cpu :c]))]
    (-> state
        (bus/write-byte addr (get-in state [:cpu :a]))
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0xE5 PUSH_HL
  [state _]
  (-> state
      (util/push-r-16 :h :l)
      (util/inc-pc)
      (util/tick 16)))

(defmethod execute 0xE6 AND_N
  [state _]
  (let [a (get-in state [:cpu :a])
        n (bus/read-byte state (inc (get-in state [:cpu :pc])))
        val (bit-and a n)]
    (-> state
        (assoc-in [:cpu :a] val)
        (util/update-flag util/Z-mask (zero? val))
        (util/unset-flag util/N-mask)
        (util/set-flag util/H-mask)
        (util/unset-flag util/C-mask)
        (util/inc-pc 2)
        (util/tick 8))))

(defmethod execute 0xE9 JP_HL
  [state _]
  (-> state
      (assoc-in [:cpu :pc] (util/get16 state :h :l))
      (util/tick 4)))

(defmethod execute 0xEA LD_ADDR_A16_A
  [state _]
  (let [addr (bus/read-word state (inc (get-in state [:cpu :pc])))]
    (-> state
        (bus/write-byte addr (get-in state [:cpu :a]))
        (util/inc-pc 3)
        (util/tick 16))))

(defmethod execute 0xEE XOR_N
  [state _]
  (let [val (bus/read-byte state (inc (get-in state [:cpu :pc])))]
    (-> (xor-val state val)
        (util/inc-pc 2)
        (util/tick 8))))

(defmethod execute 0xF0 LDH_A_ADDR_A8
  [state _]
  (let [addr (-> (bus/read-byte state (inc (get-in state [:cpu :pc])))
                 (+ 0xFF00))
        val (bus/read-byte state addr)]
    (-> state
        (assoc-in [:cpu :a] val)
        (util/inc-pc 2)
        (util/tick 12))))

(defmethod execute 0xF1 POP_AF
  [state _]
  (-> state
      (util/pop-r-16 :a :f)
      (util/inc-pc)
      (util/tick 12)))

(defmethod execute 0xF2 LD_A_ADDR_C
  [state _]
  (let [addr (-> (get-in state [:cpu :c])
                 (+ 0xFF00))
        val (bus/read-byte state addr)]
    (-> state
        (assoc-in [:cpu :a] val)
        (util/inc-pc)
        (util/tick 8))))

(defmethod execute 0xF3 DI
  [state _]
  (-> state
      (assoc-in [:cpu :interrupts-enabled?] false)
      (util/inc-pc)
      (util/tick 4)))

(defmethod execute 0xF5 PUSH_AF
  [state _]
  (-> state
      (util/push-r-16 :a :f)
      (util/inc-pc)
      (util/tick 16)))

(defmethod execute 0xFA LD_A_ADDR_A16
  [state _]
  (let [addr (bus/read-word state (inc (get-in state [:cpu :pc])))
        val (bus/read-byte state addr)]
    (-> state
        (assoc-in [:cpu :a] val)
        (util/inc-pc 3)
        (util/tick 16))))

(defmethod execute 0xFE CP_N
  [state _]
  (let [val (bus/read-byte state (inc (get-in state [:cpu :pc])))]
    (compare-val state val)))

(defmethod execute :default
  [state opcode]
  (throw (Exception. (format "Opcode 0x%02X not implemented at PC: 0x%04X"
                             opcode (get-in state [:cpu :pc])))))


