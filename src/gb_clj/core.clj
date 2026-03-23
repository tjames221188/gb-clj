(ns gb-clj.core
  (:require
   [clojure.tools.logging :as log]
   [gb-clj.bus :as bus]
   [gb-clj.cart :as cart]
   [gb-clj.cpu :as cpu]
   [taoensso.timbre :as timbre]))

(timbre/set-min-level! :info)

(defn initial-state []
  {:cpu (cpu/initial-state)
   :memory (vec (repeat 0x10000 0))
   :serial-output ""})

(defn load-rom [state path]
  (let [rom-data (cart/load-file path)]
    (log/info "Loaded ROM: " (cart/get-title rom-data))
    (bus/map-rom state rom-data)))

(defn -main [& args]
  (log/info "Booting GameBoy...")
  ;; Main loop logic goes here
  )
