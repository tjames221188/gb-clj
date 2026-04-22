(ns gb-clj.cpu.prefix-instructions
  (:require
   [gb-clj.cpu.util :as util]
   [gb-clj.bus :as bus]))

(defmulti execute-prefix (fn [_state sub-opcode] sub-opcode))

(defmethod execute-prefix :default
  [state opcode]
  (throw (Exception. (format "PREFIX Opcode 0x%02X not implemented at PC: 0x%04X"
                             opcode (get-in state [:cpu :pc])))))

(defn rotate
  [state rotate-fn register-or-addr]
  (let [old-val (if (keyword? register-or-addr)
                  (get-in state [:cpu register-or-addr])
                  (bus/read-byte state register-or-addr))
        old-c (if (util/flag-set? state util/C-mask) 1 0)
        [new-val new-c] (rotate-fn old-val old-c)]
    (cond-> state
      (keyword? register-or-addr) (assoc-in [:cpu register-or-addr] new-val)
      (not (keyword? register-or-addr)) (bus/write-byte register-or-addr new-val)
      :then
      (-> (util/update-flag util/Z-mask (zero? new-val))
          (util/unset-flag util/N-mask)
          (util/unset-flag util/H-mask)
          (util/update-flag util/C-mask (pos? new-c))))))

(defmethod execute-prefix 0x00 RLC_B
  [state _]
  (rotate state util/rotate-left-circular :b))

(defmethod execute-prefix 0x01 RLC_C
  [state _]
  (rotate state util/rotate-left-circular :c))

(defmethod execute-prefix 0x02 RLC_D
  [state _]
  (rotate state util/rotate-left-circular :d))

(defmethod execute-prefix 0x03 RLC_E
  [state _]
  (rotate state util/rotate-left-circular :e))

(defmethod execute-prefix 0x04 RLC_H
  [state _]
  (rotate state util/rotate-left-circular :h))

(defmethod execute-prefix 0x05 RLC_L
  [state _]
  (rotate state util/rotate-left-circular :l))

(defmethod execute-prefix 0x06 RLC_ADDR_HL
  [state _]
  (let [address (util/get16 state :h :l)]
    (-> (rotate state util/rotate-left-circular address)
        ;; extra cycles because of read + write from/to memory
        (util/tick 8))))

(defmethod execute-prefix 0x07 RLC_A
  [state _]
  (rotate state util/rotate-left-circular :a))

(defmethod execute-prefix 0x08 RRC_B
  [state _]
  (rotate state util/rotate-right-circular :b))

(defmethod execute-prefix 0x09 RRC_C
  [state _]
  (rotate state util/rotate-right-circular :c))

(defmethod execute-prefix 0x0A RRC_D
  [state _]
  (rotate state util/rotate-right-circular :d))

(defmethod execute-prefix 0x0B RRC_E
  [state _]
  (rotate state util/rotate-right-circular :e))

(defmethod execute-prefix 0x0C RRC_H
  [state _]
  (rotate state util/rotate-right-circular :h))

(defmethod execute-prefix 0x0D RRC_L
  [state _]
  (rotate state util/rotate-right-circular :l))

(defmethod execute-prefix 0x0E RRC_ADDR_HL
  [state _]
  (let [address (util/get16 state :h :l)]
    (-> (rotate state util/rotate-right-circular address)
        ;; extra cycles because of read + write from/to memory
        (util/tick 8))))

(defmethod execute-prefix 0x0F RRC_A
  [state _]
  (rotate state util/rotate-right-circular :a))

(defmethod execute-prefix 0x10 RL_B
  [state _]
  (rotate state util/rotate-thru-carry-left :b))

(defmethod execute-prefix 0x11 RL_C
  [state _]
  (rotate state util/rotate-thru-carry-left :c))

(defmethod execute-prefix 0x12 RL_D
  [state _]
  (rotate state util/rotate-thru-carry-left :d))

(defmethod execute-prefix 0x13 RL_E
  [state _]
  (rotate state util/rotate-thru-carry-left :e))

(defmethod execute-prefix 0x14 RL_H
  [state _]
  (rotate state util/rotate-thru-carry-left :h))

(defmethod execute-prefix 0x15 RL_L
  [state _]
  (rotate state util/rotate-thru-carry-left :l))

(defmethod execute-prefix 0x16 RL_ADDR_HL
  [state _]
  (let [address (util/get16 state :h :l)]
    (-> (rotate state util/rotate-thru-carry-left address)
        ;; extra cycles because of read + write from/to memory
        (util/tick 8))))

(defmethod execute-prefix 0x17 RL_A
  [state _]
  (rotate state util/rotate-thru-carry-left :a))

(defmethod execute-prefix 0x18 RR_B
  [state _]
  (rotate state util/rotate-thru-carry-right :b))

(defmethod execute-prefix 0x19 RR_C
  [state _]
  (rotate state util/rotate-thru-carry-right :c))

(defmethod execute-prefix 0x1A RR_D
  [state _]
  (rotate state util/rotate-thru-carry-right :d))

(defmethod execute-prefix 0x1B RR_E
  [state _]
  (rotate state util/rotate-thru-carry-right :e))

(defmethod execute-prefix 0x1C RR_H
  [state _]
  (rotate state util/rotate-thru-carry-right :h))

(defmethod execute-prefix 0x1D RR_L
  [state _]
  (rotate state util/rotate-thru-carry-right :l))

(defmethod execute-prefix 0x1E RR_ADDR_HL
  [state _]
  (let [address (util/get16 state :h :l)]
    (-> (rotate state util/rotate-thru-carry-right address)
        ;; extra cycles because of read + write from/to memory
        (util/tick 8))))

(defmethod execute-prefix 0x1F RR_A
  [state _]
  (rotate state util/rotate-thru-carry-right :a))

(defmethod execute-prefix 0x38 SRL_B ;; Shift Right Logial (not shift register left!)
  [state _]
  (let [old-b (get-in state [:cpu :b])
        new-b (bit-shift-right old-b 1)
        c? (bit-test old-b 0)]
    (-> state
        (assoc-in [:cpu :b] new-b)
        (util/update-flag util/Z-mask (zero? new-b))
        (util/unset-flag util/N-mask)
        (util/unset-flag util/H-mask)
        (util/update-flag util/C-mask c?))))
