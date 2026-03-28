(ns gb-clj.bus
  (:require
   [clojure.tools.logging :as log]))

;; TODO:
;;  - Echo RAM? 0xE000 - 0xFDFF

(defn read-byte [state addr]
  (nth (:memory state) (bit-and 0xFFFF addr)))

(defn read-word [state addr]
  (let [low  (read-byte state addr)
        high (read-byte state (inc addr))]
    (bit-or (bit-shift-left high 8) low)))

(defn write-byte [state addr val]
  (let [addr (bit-and 0xFFFF addr)
        val (bit-and 0xFF val)]
    (cond
      (<= 0x0000 addr 0x7FFF)
      ;; TODO: there are some valid use cases for this apparently,
      (do (log/error "Attempted write to ROM" {:addr addr :val val})
          state)

      (< addr 0xE000)
      (assoc-in state [:memory addr] val)

      (< addr 0xFE00)
      (assoc-in state [:memory (- addr 0x2000)] val)

      ;; OAM: 0xFE00 - 0xFE9F
      (< addr 0xFEA0)
      (assoc-in state [:memory addr] val)

      ;; The "Forbidden" Hole: 0xFEA0 - 0xFEFF
      (< addr 0xFF00)
      (do (log/warnf "Illegal write to prohibited area: 0x%04X" addr)
          state)

      ;; Serial control hook
      (and (= addr 0xFF02) (= val 0x81))
      (let [c (char (get-in state [:memory 0xFF01]))]
        (-> state
            (update :serial-out (fnil str "") c)
            ;; clear bit 7 to signal transfer complete, for now
            (assoc-in [:memory 0xFF02] (bit-and val 0x7F))))

;; IO Registers, HRAM, and IE Register: 0xFF00 - 0xFFFF
      :else
      (assoc-in state [:memory addr] val))))

(defn map-rom [state rom-data]
  (let [blank-mem (vec (repeat 0x10000 0))
        ;; Overwrite the beginning of blank memory with the ROM bytes
        new-mem (vec (concat rom-data (drop (count rom-data) blank-mem)))]
    (assoc state :memory new-mem)))
