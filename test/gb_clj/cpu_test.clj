(ns gb-clj.cpu-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer :all]
   [clojure.tools.logging :as log]
   [gb-clj.bus :as bus]
   [gb-clj.core :as core]
   [gb-clj.cpu :as cpu]
   [gb-clj.cpu.util :as util]))

(defn make-state [& {:keys [pc sp ime halted ie if-reg]
                     :or {pc 0x0200 sp 0xFFFE ime true halted false ie 0x00 if-reg 0x00}}]
  (-> {:cpu (assoc (cpu/initial-cpu-state)
                   :pc pc
                   :sp sp
                   :interrupts-enabled? ime
                   :halted? halted)
       :memory (vec (repeat 0x10000 0))
       :serial-out ""
       :timer {:div-counter 0 :tima-counter 0}}
      (bus/write-byte 0xFFFF ie)
      (bus/write-byte 0xFF0F if-reg)))

(defn get-test-rom [filename]
  (let [res (io/resource (str "cpu_instrs/individual/" filename))]
    (if res
      (io/as-file res)
      (throw (Exception. (str "ROM not found: " filename))))))

(defn run-test-rom [filename expected-serial max-steps]
  (let [rom-path (get-test-rom filename)
        init-state (-> (core/initial-state)
                       (core/load-rom rom-path))]
    (loop [state init-state
           steps 0]
      (if (or (>= steps max-steps)
              (= (:serial-out state) expected-serial))
        state
        (let [next-state (try
                           (cpu/step state)
                           (catch Exception e
                             (log/error e "It's fucked mate")
                             (throw (Exception. (str "CPU Crash at step " steps ": " (.getMessage e))))))]
          (recur next-state (inc steps)))))))

(deftest blargg-instr-test
  (testing "01-special"
    (cpu/clear-trace)
    (let [expected "01-special\n\n\nPassed\n"
          final-state (run-test-rom "01-special.gb" expected 2000000)]
      (cpu/dump-trace)
      (is (= expected (:serial-out final-state)))))
  (testing "02-interrupts"
    (cpu/clear-trace)
    (let [expected "02-interrupts\n\n\nPassed\n"
          final-state (run-test-rom "02-interrupts.gb" expected 20000000)]
      (cpu/dump-trace)
      (println "\nSerial output:\n" (:serial-out final-state))
      (is (= expected (:serial-out final-state)))))
  (testing "03-op sp,hl"
    (cpu/clear-trace)
    (let [expected "03-op sp,hl\n\n\nPassed\n"
          final-state (run-test-rom "03-op sp,hl.gb" expected 20000000)]
      (cpu/dump-trace)
      (is (= expected (:serial-out final-state))))))

(deftest interrupt-dispatch
  (testing "jumps to correct vector and clears IF bit"
    (let [state (make-state :pc 0x0200 :sp 0xFFFE :ime true :ie 0x01 :if-reg 0x01)
          result (cpu/step state)]
      (is (= 0x0040 (get-in result [:cpu :pc])))
      (is (= 0 (bus/read-byte result 0xFF0F)))
      (is (false? (get-in result [:cpu :interrupts-enabled?])))))

  (testing "services lowest priority bit when multiple pending"
    (let [state (make-state :pc 0x0200 :ime true :ie 0x05 :if-reg 0x05)
          result (cpu/step state)]
      (is (= 0x0040 (get-in result [:cpu :pc])))
      (is (= 0x04 (bus/read-byte result 0xFF0F)))))

  (testing "pushes PC to stack"
    (let [state (make-state :pc 0x1234 :sp 0xFFFE :ime true :ie 0x01 :if-reg 0x01)
          result (cpu/step state)]
      (is (= 0x12 (bus/read-byte result 0xFFFD)))
      (is (= 0x34 (bus/read-byte result 0xFFFC)))))

  (testing "does not dispatch when IME is false"
    (let [state (-> (make-state :pc 0x0200 :ime false :ie 0x01 :if-reg 0x01)
                    (assoc-in [:memory 0x0200] 0x00))
          result (cpu/step state)]
      (is (= 0x0201 (get-in result [:cpu :pc])))
      (is (= 0x01 (bus/read-byte result 0xFF0F)))))

  (testing "EI delay: does not dispatch on same step as EI"
    (let [state (-> (make-state :pc 0x0200 :ime false :ie 0x01 :if-reg 0x01)
                    (assoc-in [:memory 0x0200] 0xFB))
          result (cpu/step state)]
      (is (not= 0x0040 (get-in result [:cpu :pc])))))

  (testing "dispatches on step after EI"
    (let [state (-> (make-state :pc 0x0200 :ime false :ie 0x01 :if-reg 0x01)
                    (assoc-in [:cpu :ime-pending?] true)
                    (assoc-in [:memory 0x0200] 0x00))
          result (cpu/step state)]
      (is (= 0x0040 (get-in result [:cpu :pc]))))))

(deftest halt-behaviour
  (testing "wakes and dispatches when IME=true and interrupt pending"
    (let [state (make-state :pc 0x0200 :ime true :halted true :ie 0x01 :if-reg 0x01)
          result (cpu/step state)]
      (is (false? (get-in result [:cpu :halted?])))
      (is (= 0x0040 (get-in result [:cpu :pc])))))

  (testing "wakes but does not dispatch when IME=false and interrupt pending"
    (let [state (make-state :pc 0x0200 :ime false :halted true :ie 0x01 :if-reg 0x01)
          result (cpu/step state)]
      (is (false? (get-in result [:cpu :halted?])))
      (is (= 0x0200 (get-in result [:cpu :pc])))
      (is (= 0x01 (bus/read-byte result 0xFF0F)))))

  (testing "stays halted when no interrupt pending"
    (let [state (make-state :pc 0x0200 :ime true :halted true :ie 0x00 :if-reg 0x00)
          result (cpu/step state)]
      (is (true? (get-in result [:cpu :halted?]))))))

(deftest flags
  (let [mask 2r0100]
    (testing "Set flag..."
      (testing "when flag is unset"
        (is (= {:cpu {:f 2r0100}} (util/set-flag {:cpu {:f 2r0000}} mask))))
      (testing "when flag is already set"
        (is (= {:cpu {:f 2r0100}} (util/set-flag {:cpu {:f 2r0100}} mask)))))
    (testing "Unset flag..."
      (testing "When flag is set"
        (is (= {:cpu {:f 2r1011}} (util/unset-flag {:cpu {:f 2r1111}} mask))))
      (testing "When flag is already unset"
        (is (= {:cpu {:f 2r1011}} (util/unset-flag {:cpu {:f 2r1011}} mask)))))))

