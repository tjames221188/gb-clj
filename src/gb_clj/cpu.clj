(ns gb-clj.cpu
  (:require
   [clojure.tools.logging :as log]
   [gb-clj.bus :as bus]
   [gb-clj.cpu.instructions :as instructions]
   [gb-clj.cpu.util :as util]
   [gb-clj.timer :as timer]))

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

(defn initial-cpu-state []
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

(defn format-trace [gb-state opcode]
  (let [{:keys [pc a f sp t-cycles]} (:cpu gb-state)]
    (if (= opcode 0xCB)
      (let [sub-op (bus/read-byte gb-state (inc pc))]
        (format "PC: 0x%04X | Op: 0xCB 0x%02X | A: %02X F: %02X | BC: %04X | DE: %04X | HL: %04X | SP: %04X (t-cycles: %d)" pc sub-op a f
                (util/get16 gb-state :b :c) (util/get16 gb-state :d :e) (util/get16 gb-state :h :l)
                sp t-cycles))
      (format "PC: 0x%04X | Op: 0x%02X | A: %02X F: %02X | BC: %04X | DE: %04X | HL: %04X | SP: %04X (t-cycles: %d)"
              pc opcode a f
              (util/get16 gb-state :b :c) (util/get16 gb-state :d :e) (util/get16 gb-state :h :l)
              sp t-cycles))))

(defn- dispatch-interrupt [gb-state]
  (let [IE (bus/read-byte gb-state 0xFFFF)
        IF (bus/read-byte gb-state 0xFF0F)
        lowest-set-bit (Integer/numberOfTrailingZeros (bit-and IE IF 0x1F))
        new-IF (bit-clear IF lowest-set-bit)
        pc (get-in gb-state [:cpu :pc])
        addr (nth [0x0040 0x0048 0x0050 0x0058 0x0060] lowest-set-bit)]
    (-> gb-state
        (assoc-in [:cpu :interrupts-enabled?] false)
        (bus/write-byte 0xFF0F new-IF)
        (util/push-val-16 pc)
        (assoc-in [:cpu :pc] addr)
        (util/tick 20))))

(defn halty-walty [gb-state]
  (cond (util/ime-and-interrupt-pending? gb-state)
        (-> (assoc-in gb-state [:cpu :halted?] false)
            (dispatch-interrupt))

        (util/interrupt-pending? gb-state)
        (assoc-in gb-state [:cpu :halted?] false)

        :else gb-state))

(defn do-halt-bug [gb-state opcode]
  (let [pc (get-in gb-state [:cpu :pc])]
    (-> gb-state
        (assoc-in [:cpu :halt-bug?] false)
        (instructions/execute opcode)
        (assoc-in [:cpu :pc] pc))))

(defn- step-cpu [gb-state]
  (if (get-in gb-state [:cpu :halted?])
    (halty-walty gb-state)
    (let [pc (get-in gb-state [:cpu :pc])
          opcode (bus/read-byte gb-state pc)]
      (try
        (push-trace (format-trace gb-state opcode))
        (let [gb-state (cond-> gb-state
                         (get-in gb-state [:cpu :ime-pending?])
                         (-> (assoc-in [:cpu :ime-pending?] false)
                             (assoc-in [:cpu :interrupts-enabled?] true)))]
          (cond (get-in gb-state [:cpu :halt-bug?])
                (do-halt-bug gb-state opcode)

                (util/ime-and-interrupt-pending? gb-state)
                (dispatch-interrupt gb-state)

                :else (instructions/execute gb-state opcode)))
        (catch Exception e
          (dump-trace)
          (log/error e (format "CPU error at: %s" (format-trace gb-state opcode)))
          (throw e))))))

(defn step [state]
  (let [cycles-before (get-in state [:cpu :t-cycles])
        next-state (step-cpu state)
        cycles-after (get-in next-state [:cpu :t-cycles])]
    (timer/tick next-state (- cycles-after cycles-before))))
