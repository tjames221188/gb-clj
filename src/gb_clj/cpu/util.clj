(ns gb-clj.cpu.util
  (:require
   [clojure.tools.logging :as log]
   [gb-clj.bus :as bus]
   [gb-clj.cpu.bits :as bits]))

(defn interrupt-pending? [gb-state]
  (let [IE (bus/read-byte gb-state 0xFFFF)
        IF (bus/read-byte gb-state 0xFF0F)]
    (pos? (bit-and IE IF 0x1F))))

(defn ime-and-interrupt-pending? [gb-state]
  (and (get-in gb-state [:cpu :interrupts-enabled?])
       (interrupt-pending? gb-state)))

(defn tick [gb-state n]
  (update-in gb-state [:cpu :t-cycles] + n))

(defn inc-pc
  ([gb-state]
   (inc-pc gb-state 1))
  ([gb-state n]
   (update-in gb-state [:cpu :pc] #(bit-and 0xFFFF (+ % n)))))

(def Z-mask 2r10000000)
(def N-mask 2r01000000)
(def H-mask 2r00100000)
(def C-mask 2r00010000)

(defn flag-set? [gb-state mask]
  (pos? (bit-and (get-in gb-state [:cpu :f]) mask)))

(defn set-flag [gb-state mask]
  (update-in gb-state [:cpu :f] bit-or mask))

(defn unset-flag [gb-state mask]
  (update-in gb-state [:cpu :f] bit-and-not mask))

(defn update-flag [gb-state mask ?]
  (if ?
    (set-flag gb-state mask)
    (unset-flag gb-state mask)))

(defn toggle-flag [gb-state mask]
  (update-in gb-state [:cpu :f] bit-xor mask))

(defn get16 [gb-state r1 r2]
  (->> (:cpu gb-state)
       ((juxt r1 r2))
       (apply bits/combine)))

(defn set16 [gb-state r1 r2 v]
  (let [[high low] (bits/split v)]
    (-> gb-state
        (assoc-in [:cpu r1] high)
        (assoc-in [:cpu r2] low))))

(defn dec8 [gb-state prev]
  (let [val (bit-and 0xFF (dec prev))
        h? (zero? (bit-and 0xF prev)) ;; will always "borrow" from upper nibble if lower nibble is 0000
        ]
    [val (-> gb-state
             (set-flag N-mask)
             (update-flag Z-mask (zero? val))
             (update-flag H-mask h?))]))

(defn dec-r8 [gb-state r]
  (let [prev (get-in gb-state [:cpu r])
        [val gb-state] (dec8 gb-state prev)]
    (assoc-in gb-state [:cpu r] val)))

(defn dec-r16
  ([gb-state r]
   (when (not= :sp r)
     (log/warn "single register dec16, but not stack pointer! ( " r " )"))
   (update-in gb-state [:cpu r] (comp #(bit-and 0xFFFF %) dec)))
  ([gb-state r1 r2]
   (->> (:cpu gb-state)
        ((juxt r1 r2))
        (apply bits/combine)
        (dec)
        (bit-and 0xFFFF)
        (set16 gb-state r1 r2))))

(defn inc8 [gb-state prev]
  (let [val (bit-and 0xFF (inc prev))
        z? (zero? val)
        h? (= 0x0F (bit-and prev 0x0F))]
    [val (-> gb-state
             (unset-flag N-mask)
             (update-flag Z-mask z?)
             (update-flag H-mask h?))]))

(defn inc-r8 [gb-state r]
  (let [prev (get-in gb-state [:cpu r])
        [val gb-state] (inc8 gb-state prev)]
    (assoc-in gb-state [:cpu r] val)))

(defn inc16
  ([gb-state r]
   (when (not= :sp r)
     (log/warn "single register inc16, but not stack pointer! ( " r " )"))
   (update-in gb-state [:cpu r] (comp #(bit-and 0xFFFF %) inc)))
  ([gb-state r1 r2]
   (let [[r1-v r2-v] (->> (:cpu gb-state)
                          ((juxt r1 r2))
                          (apply bits/combine)
                          (inc)
                          (bit-and 0xFFFF)
                          (bits/split))]
     (-> gb-state
         (assoc-in [:cpu r1] r1-v)
         (assoc-in [:cpu r2] r2-v)))))

(defn add-hl [gb-state val]
  (let [hl (get16 gb-state :h :l)
        result (+ hl val)
        new-hl (bit-and 0xFFFF result)
        h? (> (+ (bit-and hl 0xFFF) (bit-and val 0xFFF)) 0xFFF)
        c? (> result 0xFFFF)]
    (-> gb-state
        (set16 :h :l new-hl)
        (unset-flag N-mask)
        (update-flag H-mask h?)
        (update-flag C-mask c?))))

(defn load8-immediate
  [gb-state register-or-addr]
  (let [pc (get-in gb-state [:cpu :pc])
        n (bus/read-byte gb-state (inc pc))]
    (if (keyword? register-or-addr)
      (-> gb-state
          (assoc-in [:cpu register-or-addr] n)
          (inc-pc 2)
          (tick 8))
      (-> gb-state
          (bus/write-byte register-or-addr n)
          (inc-pc 2)
          (tick 12)))))

(defn load16-immediate
  [gb-state r1 r2]
  (let [pc (get-in gb-state [:cpu :pc])
        nn (bus/read-word gb-state (inc pc))]
    (-> gb-state
        (set16 r1 r2 nn)
        (inc-pc 3))))

(defn pop-val-16 [gb-state]
  (let [sp (get-in gb-state [:cpu :sp])
        low (bus/read-byte gb-state sp)
        high (bus/read-byte gb-state (inc sp))]
    [[high low]
     (update-in gb-state [:cpu :sp] #(bit-and 0xFFFF (+ % 2)))]))

(defn push-val-16 [gb-state val]
  (let [sp (get-in gb-state [:cpu :sp])
        h-addr (bit-and 0xFFFF (dec sp))
        l-addr (bit-and 0xFFFF (dec h-addr))
        [h-val l-val] (bits/split val)]
    (-> gb-state
        (bus/write-byte h-addr h-val)
        (bus/write-byte l-addr l-val)
        (assoc-in [:cpu :sp] l-addr))))

(defn pop-r-16 [gb-state r1 r2]
  (let [[[high low] gb-state] (pop-val-16 gb-state)]
    (-> gb-state
        (assoc-in [:cpu r1] high)
        (assoc-in [:cpu r2] (cond-> low (= :f r2) (bit-and 0xF0))))))

(defn push-r-16 [gb-state r1 r2]
  (let [val (->> (:cpu gb-state)
                 ((juxt r1 r2))
                 (apply bits/combine))]
    (push-val-16 gb-state val)))

(defn copy-register [gb-state r-src r-dest]
  (let [v (get-in gb-state [:cpu r-src])]
    (assoc-in gb-state [:cpu r-dest] v)))

(defn as-signed-8 [n]
  (if (>= n 128)
    (- n 256)
    n))

(defn jump-relative-pred-r8
  [gb-state pred?]
  (let [offset (as-signed-8 (bus/read-byte gb-state (inc (get-in gb-state [:cpu :pc]))))]
    (if (pred? gb-state)
      (-> gb-state
          (inc-pc (+ 2 offset))
          (tick 12))
      (-> gb-state
          (inc-pc 2)
          (tick 8)))))

(defn jump-relative-pred-a16
  [gb-state pred?]
  (let [addr (bus/read-word gb-state (inc (get-in gb-state [:cpu :pc])))]
    (if (pred? gb-state)
      (-> gb-state
          (assoc-in [:cpu :pc] addr)
          (tick 16))
      (-> gb-state
          (inc-pc 3)
          (tick 12)))))

(defn half-carry?
  "works for addition and subtraction"
  [a b result]
  (bit-test (bit-xor a b result) 4))

(defn write-r-to-addr16 [gb-state r r-addr-1 r-addr-2]
  (let [addr (get16 gb-state r-addr-1 r-addr-2)
        val (get-in gb-state [:cpu r])]
    (-> gb-state
        (bus/write-byte addr val)
        (inc-pc)
        (tick 8))))

(defn load-r-from-addr16 [gb-state r r-addr-1 r-addr-2]
  (let [addr (get16 gb-state r-addr-1 r-addr-2)
        val (bus/read-byte gb-state addr)]
    (-> gb-state
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

(defn swap-nibbles
  [old-val _]
  (let [[high low] ((juxt #(bit-and % 0xF0)
                          #(bit-and % 0x0F)) old-val)]
    [(bit-or (bit-shift-left low 4) (bit-shift-right high 4))
     0]))

(defn shift-left-arithmetic
  [old-val _]
  [(bit-and 0xFF (bit-shift-left old-val 1))
   (if (bit-test old-val 7) 1 0)])

(defn shift-right-arithmetic
  [old-val _]
  [(bit-or (bit-shift-right old-val 1)
           (bit-and old-val 0x80)) ;; sign bit is preserved
   (if (bit-test old-val 0) 1 0)])

(defn add-with-carry
  [gb-state val]
  (let [a (get-in gb-state [:cpu :a])
        old-c (if (flag-set? gb-state C-mask) 1 0)
        result (+ a val old-c)
        new-a (bit-and 0xFF result)
        h? (> (+ (bit-and a 0xF) (bit-and val 0xF) old-c) 0xF)
        c? (> result 0xFF)]
    (-> gb-state
        (assoc-in [:cpu :a] new-a)
        (update-flag Z-mask (zero? new-a))
        (unset-flag N-mask)
        (update-flag H-mask h?)
        (update-flag C-mask c?))))

(defn add-val
  [gb-state val]
  (-> (unset-flag gb-state C-mask)
      (add-with-carry val)))

(defn sub-with-carry
  [gb-state val]
  (let [a (get-in gb-state [:cpu :a])
        old-c (if (flag-set? gb-state C-mask) 1 0)
        result (- a val old-c)
        new-a (bit-and 0xFF result)
        h? (< (bit-and a 0xF) (+ (bit-and val 0xF) old-c))
        c? (< a (+ val old-c))]
    (-> gb-state
        (assoc-in [:cpu :a] new-a)
        (update-flag Z-mask (zero? new-a))
        (set-flag N-mask)
        (update-flag H-mask h?)
        (update-flag C-mask c?))))

(defn sub-val
  [gb-state val]
  (-> (unset-flag gb-state C-mask)
      (sub-with-carry val)))

(defn rst
  "(ReSTart)"
  [gb-state target-addr]
  (let [return-addr (bit-and 0xFFFF (inc (get-in gb-state [:cpu :pc])))]
    (-> gb-state
        (push-val-16 return-addr)
        (assoc-in [:cpu :pc] target-addr)
        (tick 16))))

(defn maybe-call
  [gb-state pred]
  (let [pc (get-in gb-state [:cpu :pc])
        return-addr (bit-and 0xFFFF (+ pc 3))
        target-addr (bus/read-word gb-state (inc pc))]
    (if (pred gb-state)
      (-> gb-state
          (push-val-16 return-addr)
          (assoc-in [:cpu :pc] target-addr)
          (tick 24))
      (-> gb-state
          (inc-pc 3)
          (tick 12)))))

(defn maybe-ret
  [gb-state pred]
  (if (pred gb-state)
    (let [[[high low] gb-state] (pop-val-16 gb-state)]
      (-> gb-state
          (assoc-in [:cpu :pc] (bits/combine high low))
          (tick 20)))
    (-> (inc-pc gb-state)
        (tick 8))))

(defn and-val [gb-state v]
  (let [a (get-in gb-state [:cpu :a])
        val (bit-and a v)]
    (-> gb-state
        (assoc-in [:cpu :a] val)
        (update-flag Z-mask (zero? val))
        (unset-flag N-mask)
        (set-flag H-mask)
        (unset-flag C-mask))))

(defn or-val [gb-state v]
  (let [a (get-in gb-state [:cpu :a])
        val (bit-or a v)]
    (-> gb-state
        (assoc-in [:cpu :a] val)
        (update-flag Z-mask (zero? val))
        (unset-flag N-mask)
        (unset-flag H-mask)
        (unset-flag C-mask))))

(defn xor-val [gb-state v]
  (let [a (get-in gb-state [:cpu :a])
        val (bit-xor a v)]
    (-> gb-state
        (assoc-in [:cpu :a] val)
        (update-flag Z-mask (zero? val))
        (unset-flag N-mask)
        (unset-flag H-mask)
        (unset-flag C-mask))))

