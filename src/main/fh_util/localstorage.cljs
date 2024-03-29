(ns fh-util.localstorage
  (:require
   [clojure.edn :refer [read-string]])
  )

(defn set-item!
  "Set `key' in browser's localStorage to `val`."
  [key val]
  (.setItem (.-localStorage js/window) key (pr-str val)))

(defn get-item
  "Returns value of `key' from browser's localStorage."
  [key]
  (read-string (.getItem (.-localStorage js/window) key)))

(defn remove-item!
  "Remove the browser's localStorage value for the given `key`."
  [key]
  (.removeItem (.-localStorage js/window) key))
