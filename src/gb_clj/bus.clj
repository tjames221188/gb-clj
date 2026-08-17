(ns gb-clj.bus
  (:require
   [clojure.tools.logging :as log]
   [gb-clj.timer :as timer]))

;; TODO:
;;  - Echo RAM? 0xE000 - 0xFDFF

(defn read-byte [gb-state addr]
  (cond
    (= timer/DIV_ADDR addr)
    (timer/div-read gb-state)

    :else
    (nth (:memory gb-state) (bit-and 0xFFFF addr))))

(defn read-word [gb-state addr]
  (let [low (read-byte gb-state addr)
        high (read-byte gb-state (inc addr))]
    (bit-or (bit-shift-left high 8) low)))

(defn write-byte [gb-state addr val]
  (let [addr (bit-and 0xFFFF addr)
        val (bit-and 0xFF val)]
    (cond
      (<= 0x0000 addr 0x7FFF)
      ;; TODO: there are some valid use cases for this apparently,
      (do (log/error "Attempted write to ROM" {:addr addr :val val})
          gb-state)

      (< addr 0xE000)
      (assoc-in gb-state [:memory addr] val)

      (< addr 0xFE00)
      (assoc-in gb-state [:memory (- addr 0x2000)] val)

      ;; OAM: 0xFE00 - 0xFE9F
      (< addr 0xFEA0)
      (assoc-in gb-state [:memory addr] val)

      ;; The "Forbidden" Hole: 0xFEA0 - 0xFEFF
      (< addr 0xFF00)
      (do (log/warnf "Illegal write to prohibited area: 0x%04X" addr)
          gb-state)

      (= addr timer/DIV_ADDR)
      (timer/div-write gb-state)

      ;; Serial control hook
      (and (= addr 0xFF02) (= val 0x81))
      (let [c (char (get-in gb-state [:memory 0xFF01]))]
        (-> gb-state
            (update :serial-out (fnil str "") c)
            ;; clear bit 7 to signal transfer complete, for now
            (assoc-in [:memory 0xFF02] (bit-and val 0x7F))))

;; IO Registers, HRAM, and IE Register: 0xFF00 - 0xFFFF
      :else
      (assoc-in gb-state [:memory addr] val))))

(defn write-word [gb-state addr val]
  (-> gb-state
      (write-byte addr (bit-and val 0xFF))
      (write-byte (inc addr) (bit-and (bit-shift-right val 8) 0xFF))))

(defn map-rom [gb-state rom-data]
  (let [blank-mem (vec (repeat 0x10000 0))
        ;; Overwrite the beginning of blank memory with the ROM bytes
        new-mem (vec (concat rom-data (drop (count rom-data) blank-mem)))]
    (assoc gb-state :memory new-mem)))
