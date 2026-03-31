(ns gb-clj.cpu.prefix-instructions
  (:require
   [gb-clj.cpu.util :as util]))

(defmulti execute-prefix (fn [_state sub-opcode] sub-opcode))

(defmethod execute-prefix :default
  [state opcode]
  (throw (Exception. (format "PREFIX Opcode 0x%02X not implemented at PC: 0x%04X"
                             opcode (get-in state [:cpu :pc])))))

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

