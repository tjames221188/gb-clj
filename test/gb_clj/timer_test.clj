(ns gb-clj.timer-test
  (:require [clojure.test :refer :all]
            [gb-clj.bus :as bus]
            [gb-clj.core :as core]
            [gb-clj.timer :as timer]))

(defn- base-state []
  (core/initial-state))

;; ---------------------------------------------------------------------------
;; DIV (0xFF04)
;; ---------------------------------------------------------------------------

(deftest div-increments-with-cycles
  (testing "DIV starts at 0"
    (let [s (base-state)]
      (is (= 0 (bus/read-byte s 0xFF04)))))

  (testing "DIV increments after 256 T-cycles"
    (let [s (timer/tick (base-state) 256)]
      (is (= 1 (bus/read-byte s 0xFF04)))))

  (testing "DIV does not increment before 256 T-cycles"
    (let [s (timer/tick (base-state) 255)]
      (is (= 0 (bus/read-byte s 0xFF04)))))

  (testing "DIV wraps from 255 back to 0"
    (let [s (timer/tick (base-state) (* 256 256))]
      (is (= 0 (bus/read-byte s 0xFF04))))))

(deftest div-write-resets-to-zero
  (testing "Writing any value to 0xFF04 resets DIV to 0"
    (let [s (-> (base-state)
                (timer/tick 512))]
      (is (= 2 (bus/read-byte s 0xFF04)))
      (let [s' (bus/write-byte s 0xFF04 0xFF)]
        (is (= 0 (bus/read-byte s' 0xFF04)))))))

(deftest div-write-resets-internal-counter
  (testing "After a DIV reset, the counter restarts from 0 (no partial tick carry-over)"
    (let [s (-> (base-state)
                (timer/tick 255)   ; one tick shy of a DIV increment
                (bus/write-byte 0xFF04 0x00)  ; reset
                (timer/tick 255))] ; another 255 cycles
      (is (= 0 (bus/read-byte s 0xFF04))))))

;; ---------------------------------------------------------------------------
;; TIMA (0xFF05) — disabled timer
;; ---------------------------------------------------------------------------

(deftest tima-does-not-increment-when-timer-disabled
  (testing "TIMA stays 0 when TAC timer-enable bit is clear"
    (let [s (-> (base-state)
                (bus/write-byte 0xFF07 0x00) ; TAC: disabled, 4096 Hz clock
                (timer/tick 10000))]
      (is (= 0 (bus/read-byte s 0xFF05))))))

;; ---------------------------------------------------------------------------
;; TIMA (0xFF05) — clock select
;; ---------------------------------------------------------------------------

(deftest tima-increments-at-correct-rate
  (testing "TAC clock 00 — 1024 T-cycles per TIMA tick"
    (let [s (-> (base-state)
                (bus/write-byte 0xFF07 0x04) ; enable, clock 00
                (timer/tick 1024))]
      (is (= 1 (bus/read-byte s 0xFF05)))))

  (testing "TAC clock 00 — no tick before 1024 cycles"
    (let [s (-> (base-state)
                (bus/write-byte 0xFF07 0x04)
                (timer/tick 1023))]
      (is (= 0 (bus/read-byte s 0xFF05)))))

  (testing "TAC clock 01 — 16 T-cycles per TIMA tick"
    (let [s (-> (base-state)
                (bus/write-byte 0xFF07 0x05) ; enable, clock 01
                (timer/tick 16))]
      (is (= 1 (bus/read-byte s 0xFF05)))))

  (testing "TAC clock 10 — 64 T-cycles per TIMA tick"
    (let [s (-> (base-state)
                (bus/write-byte 0xFF07 0x06) ; enable, clock 10
                (timer/tick 64))]
      (is (= 1 (bus/read-byte s 0xFF05)))))

  (testing "TAC clock 11 — 256 T-cycles per TIMA tick"
    (let [s (-> (base-state)
                (bus/write-byte 0xFF07 0x07) ; enable, clock 11
                (timer/tick 256))]
      (is (= 1 (bus/read-byte s 0xFF05))))))

;; ---------------------------------------------------------------------------
;; TIMA overflow — reload from TMA and set IF
;; ---------------------------------------------------------------------------

(deftest tima-overflow-reloads-from-tma
  (testing "TIMA reloads from TMA on overflow"
    (let [s (-> (base-state)
                (bus/write-byte 0xFF05 0xFF) ; TIMA at max
                (bus/write-byte 0xFF06 0x42) ; TMA = 0x42
                (bus/write-byte 0xFF07 0x04) ; enable, clock 00
                (timer/tick 1024))]      ; one tick -> overflow
      (is (= 0x42 (bus/read-byte s 0xFF05))))))

(deftest tima-overflow-sets-timer-interrupt-flag
  (testing "TIMA overflow sets bit 2 of IF (0xFF0F)"
    (let [s (-> (base-state)
                (bus/write-byte 0xFF05 0xFF)
                (bus/write-byte 0xFF06 0x00)
                (bus/write-byte 0xFF07 0x04)
                (timer/tick 1024))]
      (is (= 0x04 (bit-and 0x04 (bus/read-byte s 0xFF0F)))))))

(deftest tima-overflow-does-not-set-flag-without-overflow
  (testing "IF timer bit stays clear when TIMA has not overflowed"
    (let [s (-> (base-state)
                (bus/write-byte 0xFF05 0xFE) ; one below max
                (bus/write-byte 0xFF07 0x04)
                (timer/tick 1024))]      ; one tick -> 0xFF, no overflow yet
      (is (= 0x00 (bit-and 0x04 (bus/read-byte s 0xFF0F)))))))
