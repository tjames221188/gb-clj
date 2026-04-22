(ns gb-clj.cpu.util
  (:require
   [clojure.tools.logging :as log]
   [gb-clj.bus :as bus]))

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

(defn dec8 [state prev]
  (let [val (bit-and 0xFF (dec prev))
        h? (zero? (bit-and 0xF prev)) ;; will always "borrow" from upper nibble if lower nibble is 0000
        ]
    [val (-> state
             (set-flag N-mask)
             (update-flag Z-mask (zero? val))
             (update-flag H-mask h?))]))

(defn dec-r8 [state r]
  (let [prev (get-in state [:cpu r])
        [val state] (dec8 state prev)]
    (assoc-in state  [:cpu r] val)))

(defn dec-r16
  ([state r]
   (when (not= :sp r)
     (log/warn "single register dec16, but not stack pointer! ( " r " )"))
   (update-in state [:cpu r] (comp #(bit-and 0xFFFF %) dec)))
  ([state r1 r2]
   (->> (:cpu state)
        ((juxt r1 r2))
        (apply combine)
        (dec)
        (bit-and 0xFFFF)
        (set16 state r1 r2))))

(defn inc8 [state prev]
  (let [val (bit-and 0xFF (inc prev))
        z? (zero? val)
        h? (= 0x0F (bit-and prev 0x0F))]
    [val (-> state
             (unset-flag N-mask)
             (update-flag Z-mask z?)
             (update-flag H-mask h?))]))

(defn inc-r8 [state r]
  (let [prev (get-in state [:cpu r])
        [val state] (inc8 state prev)]
    (assoc-in state [:cpu r] val)))

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

(defn add-hl [state val]
  (let [hl (get16 state :h :l)
        result (+ hl val)
        new-hl (bit-and 0xFFFF result)
        h? (> (+ (bit-and hl 0xFFF) (bit-and val 0xFFF)) 0xFFF)
        c? (> result 0xFFFF)]
    (-> state
        (set16 :h :l new-hl)
        (unset-flag N-mask)
        (update-flag H-mask h?)
        (update-flag C-mask c?))))

(defn load8-immediate
  [state register-or-addr]
  (let [pc (get-in state [:cpu :pc])
        n (bus/read-byte state (inc pc))]
    (if (keyword? register-or-addr)
      (-> state
          (assoc-in [:cpu register-or-addr] n)
          (inc-pc 2)
          (tick 8))
      (-> state
          (bus/write-byte register-or-addr n)
          (inc-pc 2)
          (tick 12)))))

(defn load16-immediate
  [state r1 r2]
  (let [pc (get-in state [:cpu :pc])
        nn (bus/read-word state (inc pc))]
    (-> state
        (set16 r1 r2 nn)
        (inc-pc 3))))

(defn pop-val-16 [state]
  (let [sp (get-in state [:cpu :sp])
        low (bus/read-byte state sp)
        high (bus/read-byte state (inc sp))]
    [[high low]
     (update-in state [:cpu :sp] #(bit-and 0xFFFF (+ % 2)))]))

(defn push-val-16 [state val]
  (let [sp (get-in state [:cpu :sp])
        h-addr (bit-and 0xFFFF (dec sp))
        l-addr (bit-and 0xFFFF (dec h-addr))
        [h-val l-val] (split val)]
    (-> state
        (bus/write-byte h-addr h-val)
        (bus/write-byte l-addr l-val)
        (assoc-in [:cpu :sp] l-addr))))

(defn pop-r-16 [state r1 r2]
  (let [[[high low] state] (pop-val-16 state)]
    (-> state
        (assoc-in [:cpu r1] high)
        (assoc-in [:cpu r2] (cond-> low (= :f r2) (bit-and 0xF0))))))

(defn push-r-16 [state r1 r2]
  (let [val (->> (:cpu state)
                 ((juxt r1 r2))
                 (apply combine))]
    (push-val-16 state val)))

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

(defn jump-relative-pred-a16
  [state pred?]
  (let [addr (bus/read-word state (inc (get-in state [:cpu :pc])))]
    (if (pred? state)
      (-> state
          (assoc-in [:cpu :pc] addr)
          (tick 16))
      (-> state
          (inc-pc 3)
          (tick 12)))))

(defn half-carry?
  "works for addition and subtraction"
  [a b result]
  (bit-test (bit-xor a b result) 4))

(defn write-r-to-addr16 [state r r-addr-1 r-addr-2]
  (let [addr (get16 state r-addr-1 r-addr-2)
        val (get-in state [:cpu r])]
    (-> state
        (bus/write-byte addr val)
        (inc-pc)
        (tick 8))))

(defn load-r-from-addr16 [state r r-addr-1 r-addr-2]
  (let [addr (get16 state r-addr-1 r-addr-2)
        val (bus/read-byte state addr)]
    (-> state
        (assoc-in [:cpu r] val)
        (inc-pc)
        (tick 8))))

;; Rotating
(defn rotate-left-circular
  "Rotate left circular. Returns `[new-val new-c]`"
  [old-val _]
  (let [bit-7 (bit-shift-right (bit-and old-val 0x80) 7)]
    [(bit-and 0xFF (bit-or (bit-shift-left old-val 1)
                           bit-7))
     bit-7]))

(defn rotate-right-circular
  "Rotate right circular. Returns `[new-val new-c]`"
  [old-val _]
  (let [bit-0 (bit-and old-val 0x1)]
    [(bit-or (bit-shift-right old-val 1)
             (bit-shift-left bit-0 7))
     bit-0]))

(defn rotate-thru-carry-left
  "Rotate left the bits at `register-or-addr` using the carry flag as the 9th bit.
  Returns `[new-val new-c]`"
  [old-val old-c]
  [(bit-and 0xFF (bit-or (bit-shift-left old-val 1)
                         old-c))
   (bit-shift-right (bit-and old-val 0x80) 7)])

(defn rotate-thru-carry-right
  "Rotate right the bits at `register-or-addr` using the carry flag as the 9th bit.
   Returns `[new-val new-c]`"
  [old-val old-c]
  [(bit-or (bit-shift-right old-val 1)
           (bit-shift-left old-c 7))
   (bit-and old-val 0x01)])

(defn add-with-carry
  [state val]
  (let [a (get-in state [:cpu :a])
        old-c (if (flag-set? state C-mask) 1 0)
        result (+ a val old-c)
        new-a (bit-and 0xFF result)
        h? (> (+ (bit-and  a 0xf) (bit-and val 0xF) old-c) 0xF)
        c? (> result 0xFF)]
    (-> state
        (assoc-in [:cpu :a] new-a)
        (update-flag Z-mask (zero? new-a))
        (unset-flag N-mask)
        (update-flag H-mask h?)
        (update-flag C-mask c?))))

(defn maybe-ret
  [state pred]
  (if (pred state)
    (let [[[high low] state] (pop-val-16 state)]
      (-> state
          (assoc-in [:cpu :pc] (combine high low))
          (tick 20)))
    (-> (inc-pc state)
        (tick 8))))
