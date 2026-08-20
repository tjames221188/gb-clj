(ns gb-clj.cpu.prefix-instructions
  (:require
   [gb-clj.cpu.util :as util]
   [gb-clj.bus :as bus]))

(defmulti execute-prefix (fn [_gb-state sub-opcode]
                           (cond
                             (<= 0x40 sub-opcode 0x7F) :bit
                             :else sub-opcode)))

(defmethod execute-prefix :default
  [gb-state opcode]
  (throw (Exception. (format "PREFIX Opcode 0x%02X not implemented at PC: 0x%04X"
                             opcode (get-in gb-state [:cpu :pc])))))

(defn rotate
  [gb-state rotate-fn register-or-addr]
  (let [old-val (if (keyword? register-or-addr)
                  (get-in gb-state [:cpu register-or-addr])
                  (bus/read-byte gb-state register-or-addr))
        old-c (if (util/flag-set? gb-state util/C-mask) 1 0)
        [new-val new-c] (rotate-fn old-val old-c)]
    (cond-> gb-state
      (keyword? register-or-addr) (assoc-in [:cpu register-or-addr] new-val)
      (not (keyword? register-or-addr)) (bus/write-byte register-or-addr new-val)
      :then
      (-> (util/update-flag util/Z-mask (zero? new-val))
          (util/unset-flag util/N-mask)
          (util/unset-flag util/H-mask)
          (util/update-flag util/C-mask (pos? new-c))))))

(defmethod execute-prefix 0x00 RLC_B
  [gb-state _]
  (rotate gb-state util/rotate-left-circular :b))

(defmethod execute-prefix 0x01 RLC_C
  [gb-state _]
  (rotate gb-state util/rotate-left-circular :c))

(defmethod execute-prefix 0x02 RLC_D
  [gb-state _]
  (rotate gb-state util/rotate-left-circular :d))

(defmethod execute-prefix 0x03 RLC_E
  [gb-state _]
  (rotate gb-state util/rotate-left-circular :e))

(defmethod execute-prefix 0x04 RLC_H
  [gb-state _]
  (rotate gb-state util/rotate-left-circular :h))

(defmethod execute-prefix 0x05 RLC_L
  [gb-state _]
  (rotate gb-state util/rotate-left-circular :l))

(defmethod execute-prefix 0x06 RLC_ADDR_HL
  [gb-state _]
  (let [address (util/get16 gb-state :h :l)]
    (-> (rotate gb-state util/rotate-left-circular address)
        ;; extra cycles because of read + write from/to memory
        (util/tick 8))))

(defmethod execute-prefix 0x07 RLC_A
  [gb-state _]
  (rotate gb-state util/rotate-left-circular :a))

(defmethod execute-prefix 0x08 RRC_B
  [gb-state _]
  (rotate gb-state util/rotate-right-circular :b))

(defmethod execute-prefix 0x09 RRC_C
  [gb-state _]
  (rotate gb-state util/rotate-right-circular :c))

(defmethod execute-prefix 0x0A RRC_D
  [gb-state _]
  (rotate gb-state util/rotate-right-circular :d))

(defmethod execute-prefix 0x0B RRC_E
  [gb-state _]
  (rotate gb-state util/rotate-right-circular :e))

(defmethod execute-prefix 0x0C RRC_H
  [gb-state _]
  (rotate gb-state util/rotate-right-circular :h))

(defmethod execute-prefix 0x0D RRC_L
  [gb-state _]
  (rotate gb-state util/rotate-right-circular :l))

(defmethod execute-prefix 0x0E RRC_ADDR_HL
  [gb-state _]
  (let [address (util/get16 gb-state :h :l)]
    (-> (rotate gb-state util/rotate-right-circular address)
        ;; extra cycles because of read + write from/to memory
        (util/tick 8))))

(defmethod execute-prefix 0x0F RRC_A
  [gb-state _]
  (rotate gb-state util/rotate-right-circular :a))

(defmethod execute-prefix 0x10 RL_B
  [gb-state _]
  (rotate gb-state util/rotate-thru-carry-left :b))

(defmethod execute-prefix 0x11 RL_C
  [gb-state _]
  (rotate gb-state util/rotate-thru-carry-left :c))

(defmethod execute-prefix 0x12 RL_D
  [gb-state _]
  (rotate gb-state util/rotate-thru-carry-left :d))

(defmethod execute-prefix 0x13 RL_E
  [gb-state _]
  (rotate gb-state util/rotate-thru-carry-left :e))

(defmethod execute-prefix 0x14 RL_H
  [gb-state _]
  (rotate gb-state util/rotate-thru-carry-left :h))

(defmethod execute-prefix 0x15 RL_L
  [gb-state _]
  (rotate gb-state util/rotate-thru-carry-left :l))

(defmethod execute-prefix 0x16 RL_ADDR_HL
  [gb-state _]
  (let [address (util/get16 gb-state :h :l)]
    (-> (rotate gb-state util/rotate-thru-carry-left address)
        ;; extra cycles because of read + write from/to memory
        (util/tick 8))))

(defmethod execute-prefix 0x17 RL_A
  [gb-state _]
  (rotate gb-state util/rotate-thru-carry-left :a))

(defmethod execute-prefix 0x18 RR_B
  [gb-state _]
  (rotate gb-state util/rotate-thru-carry-right :b))

(defmethod execute-prefix 0x19 RR_C
  [gb-state _]
  (rotate gb-state util/rotate-thru-carry-right :c))

(defmethod execute-prefix 0x1A RR_D
  [gb-state _]
  (rotate gb-state util/rotate-thru-carry-right :d))

(defmethod execute-prefix 0x1B RR_E
  [gb-state _]
  (rotate gb-state util/rotate-thru-carry-right :e))

(defmethod execute-prefix 0x1C RR_H
  [gb-state _]
  (rotate gb-state util/rotate-thru-carry-right :h))

(defmethod execute-prefix 0x1D RR_L
  [gb-state _]
  (rotate gb-state util/rotate-thru-carry-right :l))

(defmethod execute-prefix 0x1E RR_ADDR_HL
  [gb-state _]
  (let [address (util/get16 gb-state :h :l)]
    (-> (rotate gb-state util/rotate-thru-carry-right address)
        ;; extra cycles because of read + write from/to memory
        (util/tick 8))))

(defmethod execute-prefix 0x1F RR_A
  [gb-state _]
  (rotate gb-state util/rotate-thru-carry-right :a))

(defmethod execute-prefix 0x20 SLA_B
  [gb-state _]
  (rotate gb-state util/shift-left-arithmetic :b))

(defmethod execute-prefix 0x21 SLA_C
  [gb-state _]
  (rotate gb-state util/shift-left-arithmetic :c))

(defmethod execute-prefix 0x22 SLA_D
  [gb-state _]
  (rotate gb-state util/shift-left-arithmetic :d))

(defmethod execute-prefix 0x23 SLA_E
  [gb-state _]
  (rotate gb-state util/shift-left-arithmetic :e))

(defmethod execute-prefix 0x24 SLA_H
  [gb-state _]
  (rotate gb-state util/shift-left-arithmetic :h))

(defmethod execute-prefix 0x25 SLA_L
  [gb-state _]
  (rotate gb-state util/shift-left-arithmetic :l))

(defmethod execute-prefix 0x26 SLA_ADDR_HL
  [gb-state _]
  (let [address (util/get16 gb-state :h :l)]
    (-> (rotate gb-state util/shift-left-arithmetic address)
        ;; extra cycles because of read + write from/to memory
        (util/tick 8))))

(defmethod execute-prefix 0x27 SLA_A
  [gb-state _]
  (rotate gb-state util/shift-left-arithmetic :a))

(defmethod execute-prefix 0x28 SRA_B
  [gb-state _]
  (rotate gb-state util/shift-right-arithmetic :b))

(defmethod execute-prefix 0x29 SRA_C
  [gb-state _]
  (rotate gb-state util/shift-right-arithmetic :c))

(defmethod execute-prefix 0x2A SRA_D
  [gb-state _]
  (rotate gb-state util/shift-right-arithmetic :d))

(defmethod execute-prefix 0x2B SRA_E
  [gb-state _]
  (rotate gb-state util/shift-right-arithmetic :e))

(defmethod execute-prefix 0x2C SRA_H
  [gb-state _]
  (rotate gb-state util/shift-right-arithmetic :h))

(defmethod execute-prefix 0x2D SRA_L
  [gb-state _]
  (rotate gb-state util/shift-right-arithmetic :l))

(defmethod execute-prefix 0x2E SRA_ADDR_HL
  [gb-state _]
  (let [address (util/get16 gb-state :h :l)]
    (-> (rotate gb-state util/shift-right-arithmetic address)
        ;; extra cycles because of read + write from/to memory
        (util/tick 8))))

(defmethod execute-prefix 0x2F SRA_A
  [gb-state _]
  (rotate gb-state util/shift-right-arithmetic :a))

(defmethod execute-prefix 0x30 SWAP_B
  [gb-state _]
  (rotate gb-state util/swap-nibbles :b))

(defmethod execute-prefix 0x31 SWAP_C
  [gb-state _]
  (rotate gb-state util/swap-nibbles :c))

(defmethod execute-prefix 0x32 SWAP_D
  [gb-state _]
  (rotate gb-state util/swap-nibbles :d))

(defmethod execute-prefix 0x33 SWAP_E
  [gb-state _]
  (rotate gb-state util/swap-nibbles :e))

(defmethod execute-prefix 0x34 SWAP_H
  [gb-state _]
  (rotate gb-state util/swap-nibbles :h))

(defmethod execute-prefix 0x35 SWAP_L
  [gb-state _]
  (rotate gb-state util/swap-nibbles :l))

(defmethod execute-prefix 0x36 SWAP_ADDR_HL
  [gb-state _]
  (let [address (util/get16 gb-state :h :l)]
    (-> (rotate gb-state util/swap-nibbles address)
        ;; extra cycles because of read + write from/to memory
        (util/tick 8))))

(defmethod execute-prefix 0x37 SWAP_A
  [gb-state _]
  (rotate gb-state util/swap-nibbles :a))

(defmethod execute-prefix 0x38 SRL_B
  [gb-state _]
  (rotate gb-state util/shift-right-logical :b))

(defmethod execute-prefix 0x39 SRL_C
  [gb-state _]
  (rotate gb-state util/shift-right-logical :c))

(defmethod execute-prefix 0x3A SRL_D
  [gb-state _]
  (rotate gb-state util/shift-right-logical :d))

(defmethod execute-prefix 0x3B SRL_E
  [gb-state _]
  (rotate gb-state util/shift-right-logical :e))

(defmethod execute-prefix 0x3C SRL_H
  [gb-state _]
  (rotate gb-state util/shift-right-logical :h))

(defmethod execute-prefix 0x3D SRL_L
  [gb-state _]
  (rotate gb-state util/shift-right-logical :l))

(defmethod execute-prefix 0x3E SRL_ADDR_HL
  [gb-state _]
  (let [address (util/get16 gb-state :h :l)]
    (-> (rotate gb-state util/shift-right-logical address)
        ;; extra cycles because of read + write from/to memory
        (util/tick 8))))

(defmethod execute-prefix 0x3F SRL_A
  [gb-state _]
  (rotate gb-state util/shift-right-logical :a))

(def registers [:b :c :d :e :h :l :hl :a])

(defmethod execute-prefix :bit
  [gb-state opcode]
  (let [bit-idx (-> (bit-shift-right opcode 3)
                    (bit-and 0x07))
        r-idx (-> (bit-and 0x07 opcode))
        r (nth registers r-idx)
        val (if (= :hl r)
              (bus/read-byte gb-state (util/get16 gb-state :h :l))
              (get-in gb-state [:cpu r]))]
    (cond-> (-> gb-state
                (util/update-flag util/Z-mask (not (bit-test val bit-idx)))
                (util/unset-flag util/N-mask)
                (util/set-flag util/H-mask))
      (= :hl r) ; 4 additional cycles because of read from memory
      (util/tick 4))))
