(ns dev
  (:require
   [shadow.cljs.devtools.api :as shadow]
   [portal.api :as p]))

(defn start
  {:shadow/requires-server true}
  []
  ;; Start compiling the app
  (shadow/watch :app)
  ;; ..and tests
  ;; (shadow/watch :browser-test)

  ;; Open a JVM portal instance that will host a port for remote portal clients to connect to
  (p/open {:theme :portal.colors/gruvbox
           :port 5678})

  ;; Wire taps (in clj code) to go to portal
  (add-tap p/submit)

  (prn "-------------------------------------------------------")
  (prn "Web App available at   :  http://localhost:8080")
  (prn "Tests available at     :  http://localhost:8290")
  (prn "Portal Remote API at   :  :5678")
  (prn "-------------------------------------------------------")

  ::started)

(defn stop []
  ::stopped)

(defn go []
  (stop)
  (start))
