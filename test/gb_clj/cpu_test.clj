(ns gb-clj.cpu-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer :all]
   [clojure.tools.logging :as log]
   [gb-clj.core :as core]
   [gb-clj.cpu :as cpu]
   [gb-clj.cpu.util :as util]))

(defn get-test-rom [filename]
  (let [res (io/resource (str "cpu_instrs/individual/" filename))]
    (if res
      (.getPath res)
      (throw (Exception. (str "ROM not found: " filename))))))

(defn run-test-rom [filename max-steps]
  (let [rom-path (get-test-rom filename)
        ;; Start with the Power-Up state (Post-BIOS)
        init-state (-> (core/initial-state)
                       (core/load-rom rom-path))]
    (loop [state init-state
           steps 0]
      ;; Check for an exit condition (like a specific pass/fail memory address)
      ;; or just stop when we hit our manual limit.
      (if (>= steps max-steps)
        state
        (let [next-state (try
                           (cpu/step state)
                           (catch Exception e
                             (log/error e "It's fucked mate")
                             ;; Re-throw with context so you know where it died
                             (throw (Exception. (str "CPU Crash at step " steps ": " (.getMessage e))))))]
          (recur next-state (inc steps)))))))

(deftest blargg-instr-test
  (testing "Running the first Blargg test rom"
    ;; We start with a small number of steps (e.g., 100) 
    ;; to find the very first unimplemented opcode.
    (let [final-state (run-test-rom "01-special.gb" 200000)]
      (is (= "foo" (:serial-out final-state)))
      (is (not (nil? final-state)) "Emulator should return a valid state"))
    (is (= 0 1))))

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

