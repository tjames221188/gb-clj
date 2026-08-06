(ns gb-clj.timer)

;; timer addresses
(def DIV_ADDR 0xFF04)
(def TIMA_ADDR 0xFF05)
(def TMA_ADDR 0xFF06)
(def TAC_ADDR 0xFF07)
(def IF_ADDR 0xFF0F)

(defn div-read [state]
  (-> (get-in state [:timer :div-counter])
      (bit-shift-right 8)))

(defn div-write [state]
  (assoc-in state [:timer :div-counter] 0))

(defn div-inc [state t-cycles]
  (update-in state [:timer :div-counter] #(bit-and 0xFFFF (+ % t-cycles))))

(defn- tima-divisor [tac]
  (case (bit-and tac 0x03)
    0x00 1024
    0x01 16
    0x02 64
    0x03 256))

(defn- tima-inc [state]
  (let [tima (inc (get-in state [:memory TIMA_ADDR]))
        tma (get-in state [:memory TMA_ADDR])]
    (if (> tima 0xFF)
      (-> (assoc-in state [:memory TIMA_ADDR] tma)
          (update-in [:memory IF_ADDR] bit-set 2))
      (assoc-in state [:memory TIMA_ADDR] tima))))

(defn- tima-catch-up [state t-cycles tac]
  (let [divisor (tima-divisor tac)]
    (loop [new-state (update-in state [:timer :tima-counter] + t-cycles)]
      (if (>= (get-in new-state [:timer :tima-counter]) divisor)
        (recur (-> (update-in new-state [:timer :tima-counter] - divisor)
                   (tima-inc)))
        new-state))))

(defn tick [state t-cycles]
  (let [tac (get-in state [:memory TAC_ADDR])]
    (cond-> (div-inc state t-cycles)
      (bit-test tac 2)
      (tima-catch-up t-cycles tac))))

