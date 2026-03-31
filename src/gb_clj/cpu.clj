(ns gb-clj.cpu
  (:require
   [clojure.tools.logging :as log]
   [gb-clj.bus :as bus]
   [gb-clj.cpu.instructions :as instructions]
   [gb-clj.cpu.util :as util]))

(defn initial-state []
  {:a 0x01   ; Initial Accumulator
   :f 0xB0   ; Flags: Z=1, N=0, H=1, C=1 (on original DMG)
   :b 0x00 :c 0x13
   :d 0x00 :e 0xD8
   :h 0x01 :l 0x4D
   :pc 0x0100 ; The entry point of every GB ROM
   :sp 0xFFFE ; Top of stack
   :halted? false
   :t-cycles 0
   :interrupts-enabled? true})

(defn format-trace [state opcode]
  (let [{:keys [pc a f sp t-cycles]} (:cpu state)]
    (if (= opcode 0xCB)
      (let [sub-op (bus/read-byte state (inc pc))]
        (format "PC: 0x%04X | Op: 0xCB 0x%02X | A: %02X F: %02X | BC: %04X | DE: %04X | HL: %04X | SP: %04X (t-cycles: %d)" pc sub-op a f
                (util/get16 state :b :c) (util/get16 state :d :e) (util/get16 state :h :l)
                sp t-cycles))
      (format "PC: 0x%04X | Op: 0x%02X | A: %02X F: %02X | BC: %04X | DE: %04X | HL: %04X | SP: %04X (t-cycles: %d)"
              pc opcode a f
              (util/get16 state :b :c) (util/get16 state :d :e) (util/get16 state :h :l)
              sp t-cycles))))

(defn step [state]
  (if (:halted? state)
    state
    (let [pc (get-in state [:cpu :pc])
          opcode (bus/read-byte state pc)]
      (try
        (log/info (format-trace state opcode))
        (instructions/execute state opcode)
        (catch Exception e
          (log/error e (format "CPU error! Trace: \n%s\n" (format-trace state opcode)))
          (throw e))))))

