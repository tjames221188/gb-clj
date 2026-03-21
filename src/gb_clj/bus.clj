(ns gb-clj.bus)

;; TODO:
;;  - Echo RAM? 0xE000 - 0xFDFF

(defn read-byte [state addr]
  (nth (:memory state) (bit-and 0xFFFF addr)))

(defn read-word [state addr]
  (let [low  (read-byte state addr)
        high (read-byte state (inc addr))]
    (bit-or (bit-shift-left high 8) low)))

(defn write-byte [state addr val]
  ;; Use bit-and to ensure we only ever store 0-255
  (assoc-in state [:memory (bit-and 0xFFFF addr)] (bit-and val 0xFF)))

(defn map-rom [state rom-data]
  (let [blank-mem (vec (repeat 0x10000 0))
        ;; Overwrite the beginning of blank memory with the ROM bytes
        new-mem (vec (concat rom-data (drop (count rom-data) blank-mem)))]
    (assoc state :memory new-mem)))
