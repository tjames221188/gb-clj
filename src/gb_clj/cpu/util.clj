(ns gb-clj.cpu.util
  (:require [gb-clj.bus :as bus]))

(defn tick [state n]
  (update-in state [:cpu :t-cycles] + n))

(defn inc-pc
  ([state]
   (inc-pc state 1))
  ([state n]
   (update-in state [:cpu :pc] #(bit-and 0xFFFF (+ % n)))))

(def Z-mask 2r10000000)
(def N-mask 2r01000000)
(def H-mask 2r00100000)
(def C-mask 2r00010000)

(defn flag-set? [state mask]
  (pos? (bit-and (get-in state [:cpu :f]) mask)))

(defn set-flag [state mask]
  (update-in state [:cpu :f] bit-or mask))

(defn unset-flag [state mask]
  (update-in state [:cpu :f] bit-and-not mask))

(defn update-flag [state mask ?]
  (if ?
    (set-flag state mask)
    (unset-flag state mask)))

(defn combine [high low]
  (bit-or (bit-shift-left high 8) low))

(defn split [word]
  [(bit-shift-right (bit-and word 0xFF00) 8) ; High byte
   (bit-and word 0x00FF)])                   ; Low byte

(defn get16 [state r1 r2]
  (->> (:cpu state)
       ((juxt r1 r2))
       (apply combine)))

(defn set16 [state r1 r2 v]
  (let [[high low] (split v)]
    (-> state
        (assoc-in [:cpu r1] high)
        (assoc-in [:cpu r2] low))))

(defn dec8 [state r]
  (let [prev (get-in state [:cpu r])
        val (bit-and 0xFF (dec prev))
        h? (zero? (bit-and 0xF prev)) ;; will always "borrow" from upper nibble if lower nibble is 0000
        ]
    (-> state
        (assoc-in [:cpu r] val)
        (set-flag N-mask)
        (update-flag Z-mask (zero? val))
        (update-flag H-mask h?))))

(defn dec16 [state r1 r2]
  (let [[r1-v r2-v] (->> (:cpu state)
                         ((juxt r1 r2))
                         (apply combine)
                         (dec)
                         (bit-and 0xFFFF)
                         (split))]
    (-> state
        (assoc-in [:cpu r1] r1-v)
        (assoc-in [:cpu r2] r2-v))))

(defn inc8 [state r]
  (let [prev (get-in state [:cpu r])
        val (bit-and 0xFF (inc prev))
        z? (zero? val)
        h? (= 0x0F (bit-and prev 0x0F))]
    (-> state
        (assoc-in [:cpu r] val)
        (unset-flag N-mask)
        (update-flag Z-mask z?)
        (update-flag H-mask h?))))

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

(defn load8-immediate
  [state r1]
  (let [pc (get-in state [:cpu :pc])
        n (bus/read-byte state (inc pc))]
    (-> state
        (assoc-in [:cpu r1] n)
        (inc-pc 2)
        (tick 8))))

(defn load16-immediate
  [state r1 r2]
  (let [pc (get-in state [:cpu :pc])
        nn (bus/read-word state (inc pc))]
    (-> state
        (set16 r1 r2 nn)
        (inc-pc 3))))

(defn pop16 [state r1 r2]
  (let [sp (get-in state [:cpu :sp])
        low (cond-> (bus/read-byte state sp)
              ; lower nibble of f register must always be zero
              (= :f r2) (bit-and 0xF0))
        high (bus/read-byte state (inc sp))]
    (-> state
        (assoc-in [:cpu r1] high)
        (assoc-in [:cpu r2] low)
        (update-in [:cpu :sp] #(bit-and 0xFFFF (+ % 2))))))

(defn push16 [state r1 r2]
  (let [sp (get-in state [:cpu :sp])
        h-addr (bit-and 0xFFFF (dec sp))
        l-addr (bit-and 0xFFFF (dec h-addr))]
    (-> state
        (bus/write-byte h-addr (get-in state [:cpu r1]))
        (bus/write-byte l-addr (get-in state [:cpu r2]))
        (assoc-in [:cpu :sp] l-addr))))

(defn copy-register [state r-src r-dest]
  (let [v (get-in state [:cpu r-src])]
    (assoc-in state [:cpu r-dest] v)))

(defn as-signed-8 [n]
  (if (>= n 128)
    (- n 256)
    n))

(defn jump-relative-pred-r8
  [state pred?]
  (let [offset (as-signed-8 (bus/read-byte state (inc (get-in state [:cpu :pc]))))]
    (if (pred? state)
      (-> state
          (inc-pc (+ 2 offset))
          (tick 12))
      (-> state
          (inc-pc 2)
          (tick 8)))))

(defn half-carry?
  "works for addition and subtraction"
  [a b result]
  (bit-test (bit-xor a b result) 4))

(defn load-r-from-addr16 [state r r-addr-1 r-addr-2]
  (let [addr (get16 state r-addr-1 r-addr-2)
        val (bus/read-byte state addr)]
    (-> state
        (assoc-in [:cpu r] val)
        (inc-pc)
        (tick 8))))

