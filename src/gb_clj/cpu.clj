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

(defn combine [high low]
  (bit-or (bit-shift-left high 8) low))

(defn split [word]
  [(bit-shift-right (bit-and word 0xFF00) 8) ; High byte
   (bit-and word 0x00FF)])                   ; Low byte

(defn get16 [state r1 r2]
  (->> (:cpu state)
       ((juxt r1 r2))
       (apply combine)))

(defmulti execute (fn [_state opcode] opcode))

(defmethod execute 0x00 NOP
  [state _]
  (update-in state [:cpu :pc] inc))

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

(defmethod execute 0x13 INC_DE
  [state _]
  (-> (inc16 state :d :e)
      (update-in [:cpu :pc] inc)))

(defmethod execute 0xC3
  [state _]
  (let [addr (bus/read-word state (inc (get-in state [:cpu :pc])))]
    (assoc-in state [:cpu :pc] addr)))

(defmethod execute :default
  [state opcode]
  (throw (Exception. (format "Opcode 0x%02X not implemented at PC: 0x%04X"
                             opcode (dec (get-in state [:cpu :pc]))))))

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

