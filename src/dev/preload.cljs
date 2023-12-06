(ns preload)

(ns preload
  {:dev/always true}
  (:require
   [uix.dev]
   [shadow.cljs.devtools.client.browser]))

(uix.dev/init-fast-refresh!)

(defn ^:dev/after-load refresh []
  ; (rf/clear-subscription-cache!)
  (uix.dev/refresh!))
