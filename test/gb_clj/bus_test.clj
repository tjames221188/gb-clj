(ns gb-clj.bus-test
  (:require [clojure.test :refer :all]
            [gb-clj.bus :as bus]
            [gb-clj.cpu :as cpu]))

(deftest test-bus-boundaries
  (let [state (assoc (cpu/initial-state) :memory (vec (repeat 65536 0)))]
    (testing "ROM Boundary (0x0000 - 0x7FFF)"
      (is (true? (= state (bus/write-byte state 0x0000 0xFF))) "Start of ROM")
      (is (true? (= state (bus/write-byte state 0x7FFF 0xFF))) "End of ROM")
      ;; The very next byte (0x8000) SHOULD be writable (VRAM)
      (let [vram-state (bus/write-byte state 0x8000 0x42)]
        (is (= 0x42 (bus/read-byte vram-state 0x8000)) "Start of VRAM should work")))
    (testing "Echo RAM Mapping (0xE000 - 0xFDFF -> 0xC000 - 0xDFFF)"
      ;; Test the start of the mirror
      (let [s1 (bus/write-byte state 0xE000 0x11)]
        (is (= 0x11 (bus/read-byte s1 0xC000)) "0xE000 should map to 0xC000"))
      ;; Test the end of the mirror
      (let [s2 (bus/write-byte state 0xFDFF 0x22)]
        (is (= 0x22 (bus/read-byte s2 0xDDFF)) "0xFDFF should map to 0xDDFF")))
    (testing "Prohibited Area (0xFEA0 - 0xFEFF)"
      ;; The Game Boy manual says this area is unusable. 
      ;; Usually, emulators either throw an error or log a warning and return state.
      (is (true? (= state (bus/write-byte state 0xFEA0 0xFF))) "Writing to unusable hole"))
    (testing "High RAM / IE Register (Top of memory)"
      (let [s3 (bus/write-byte state 0xFF80 0xAA) ; Start of HRAM
            s4 (bus/write-byte state 0xFFFF 0x55)] ; Interrupt Enable Register
        (is (= 0xAA (bus/read-byte s3 0xFF80)))
        (is (= 0x55 (bus/read-byte s4 0xFFFF)))))))
