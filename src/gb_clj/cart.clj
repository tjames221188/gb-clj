(ns gb-clj.cart
  (:require [clojure.java.io :as io]))

#_{:clj-kondo/ignore [:redefined-var]}
(defn load-file [path]
  (let [file (io/file path)
        size (.length file)
        buffer (byte-array size)]
    (with-open [is (io/input-stream file)]
      (.read is buffer))
    ;; Convert signed Java bytes (-128 to 127) to unsigned ints (0 to 255)
    (mapv #(bit-and % 0xFF) buffer)))

(defn get-title [rom-data]
  ;; The title is stored at 0x0134 to 0x0143
  (let [title-bytes (subvec rom-data 0x0134 0x0143)]
    (apply str (map char (take-while #(> % 0) title-bytes)))))

