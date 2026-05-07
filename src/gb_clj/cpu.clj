(ns gb-clj.cpu
  (:require
   [clojure.tools.logging :as log]
   [gb-clj.bus :as bus]
   [gb-clj.cpu.instructions :as instructions]
   [gb-clj.cpu.util :as util]))

(def trace-buf-size 200)
(def ^:private trace-buf (atom []))

(defn clear-trace [] (reset! trace-buf []))

(defn dump-trace []
  (doseq [line @trace-buf]
    (log/info line)))

(defn- push-trace [line]
  (swap! trace-buf (fn [v]
                     (let [v (conj v line)]
                       (if (> (count v) trace-buf-size)
                         (subvec v (- (count v) trace-buf-size))
                         v)))))

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

(defn- dispatch-interrupt [state]
  (let [IE (bus/read-byte state 0xFFFF)
        IF (bus/read-byte state 0xFF0F)
        lowest-set-bit (Integer/numberOfTrailingZeros (bit-and IE IF 0x1F))
        new-IF (bit-clear IF lowest-set-bit)
        pc (get-in state [:cpu :pc])
        addr (nth [0x0040 0x0048 0x0050 0x0058 0x0060] lowest-set-bit)]
    (-> state
        (assoc-in [:cpu :interrupts-enabled?] false)
        (bus/write-byte 0xFF0F new-IF)
        (util/push-val-16 pc)
        (assoc-in [:cpu :pc] addr)
        (util/tick 20))))

(defn halty-walty [state]
  (cond (util/ime-and-interrupt-pending? state)
        (-> (assoc-in state [:cpu :halted?] false)
            (dispatch-interrupt))

        (util/interrupt-pending? state)
        (assoc-in state [:cpu :halted?] false)

        :else state))

(defn do-halt-bug [state opcode]
  (let [pc (get-in state [:cpu :pc])]
    (-> state
        (assoc-in [:cpu :halt-bug?] false)
        (instructions/execute opcode)
        (assoc-in [:cpu :pc] pc))))

(defn step [state]
  (if (get-in state [:cpu :halted?])
    (halty-walty state)
    (let [pc (get-in state [:cpu :pc])
          opcode (bus/read-byte state pc)]
      (try
        (push-trace (format-trace state opcode))
        (let [state (cond-> state
                      (get-in state [:cpu :ime-pending?])
                      (-> (assoc-in [:cpu :ime-pending?] false)
                          (assoc-in [:cpu :interrupts-enabled?] true)))]
          (cond (get-in state [:cpu :halt-bug?])
                (do-halt-bug state opcode)

                (util/ime-and-interrupt-pending? state)
                (dispatch-interrupt state)

                :else (instructions/execute state opcode)))
        (catch Exception e
          (dump-trace)
          (log/error e (format "CPU error at: %s" (format-trace state opcode)))
          (throw e))))))

