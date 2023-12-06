(ns fh-util.main
  (:require
   [clojure.string :as str]
   [fh-util.button :refer [Button]]
   [fh-util.localstorage :as storage]
   [fh-util.ui :as ui :refer [Input]]
   [goog.string :as gstring]
   [goog.string.format]
   [lambdaisland.fetch :as fetch]
   [shadow.css :refer (css)]
   [uix.core :as uix :refer [$ defui]]
   [uix.dom :as uix.dom]))

(def pattern #"x = (\d+)\s+y\s=\s+(\d+)\s+z\s=\s+(\d+)\s+stellar type =\s+(.*)$")
(defn parse-system [system]
  (let [[_ x y z stellar-type] (re-find  pattern system)]
    {:coord (mapv parse-long [x y z])
     :stellar-type stellar-type}))

(defn parse-galaxy [data]
  (->> data
       (str/split-lines)
       (map parse-system)))

(defn cartesian-distance [[x1 y1 z1] [x2 y2 z2]]
  (Math/sqrt (+ (* (- x1 x2) (- x1 x2))
                (* (- y1 y2) (- y1 y2))
                (* (- z1 z2) (- z1 z2)))))

(defn distances
  "Calculate the distance between every star in the galaxy and source coord"
  [galaxy scoord]
  (for [system galaxy]
    (assoc system
           :distance (cartesian-distance scoord (:coord system)))))

(defn sort-nearest [galaxy]
  (sort-by :distance galaxy))

(defn sort-farthest [galaxy]
  (sort-by (comp - :distance) galaxy))

(defn fetch-galaxy []
  (fetch/get "/galaxy-dg.txt"))

#_(defn init-app [galaxy]
    (.log js/console
          (distances galaxy [6 40 25])))

(defn load-galaxy [after-fn]
  (-> (fetch-galaxy)
      (.then (fn [resp]
               (-> resp
                   :body
                   parse-galaxy
                   after-fn)))))

(defn fmt-float [v]
  (gstring/format "%.2f" v))

(defn calc-mishap-probability [gv distance]
  (if (== gv 0)
    100.0
    (max 0.0 (min 100.0 (/ (* distance distance) gv)))))

(defui System [{:keys [on-system-selected system has-distance? has-mishap?]}]
  (let [{:keys [coord stellar-type]} system
        [x y z] coord
        selected? (== (:distance system) 0.0)
        $col (css :px-2 :text-right)
        $selected (when selected?
                    (css :bg-gray-500 :text-white :font-bold))]
    ($ :tr
       ($ :td {:class (ui/cs $col $selected)} x)
       ($ :td {:class (ui/cs $col $selected)} y)
       ($ :td {:class (ui/cs $col $selected)} z)
       ($ :td {:class (ui/cs $col $selected)} stellar-type)
       (when has-distance?
         ($ :td {:class (ui/cs $col $selected)} (fmt-float (:distance system))))
       (when has-mishap?
         ($ :td {:class (ui/cs $col $selected)} (str (fmt-float (:mishap system)) "%")))
       ($ :td
          (when (not selected?)
            ($ Button {:on-click (fn []
                                   (on-system-selected system))} "choose"))))))

(defui Galaxy [{:keys [galaxy on-system-selected]}]
  (let [has-distance? (some? (:distance (first galaxy)))
        has-mishap? (some? (:mishap (first galaxy)))]
    ($ :table
       ($ :thead
          ($ :tr
             ($ :th {:class (css :px-2 :text-right)} "x")
             ($ :th {:class (css :px-2 :text-right)} "y")
             ($ :th {:class (css :px-2 :text-right)} "z")
             ($ :th {:class (css :px-2 :text-right)} "stellar type")
             (when has-distance?
               ($ :th {:class (css :px-2 :text-right)} "distance (parsecs)"))
             (when has-mishap?
               ($ :th {:class (css :px-2 :text-right)} "mishap chance"))
             ($ :th {:class (css :px-2)} "")))

       ($ :tbody
          (for [system galaxy]
            ($ System {:key (:coord system) :has-mishap? has-mishap? :has-distance? has-distance? :system system :on-system-selected on-system-selected}))))))

(defn find-system [galaxy coord]
  (first (filter (fn [system]
                   (= (:coord system) coord))
                 galaxy)))

(defui LookupSystem [{:keys [on-system-selected state]}]
  (let [[x y z] (-> state :selected :coord)
        [values set-values] (uix/use-state {:x x :y y :z z})
        setter (fn [k] #(set-values (assoc values k (parse-long %))))]
    ($ :div {:class (css :flex :gap-2)}
       ($ Input {:prefix "X" :attr {:type :number :defaultValue x} :on-change (setter :x)})
       ($ Input {:prefix "Y" :attr {:type :number :defaultValue y} :on-change (setter :y)})
       ($ Input {:prefix "Z" :attr {:type :number :defaultValue z} :on-change (setter :z)})
       ($ Button {:on-click (fn []
                              (let [coord [(:x values) (:y values) (:z values)]
                                    system (find-system (:galaxy state) coord)]
                                (on-system-selected system)))}
          "choose"))))

(defn system-str [{:keys [coord stellar-type]}]
  (let [[x y z] coord]
    (str "x = " x " y = " y " z = " z " stellar type = " stellar-type)))

(defui Distance [{:keys [state on-system-selected]}]
  (let [system (:selected state)
        d (->> (distances (:galaxy state) (:coord system))
               (map (fn [{:keys [coord distance] :as system}]
                      (let [gv (:GV (:species state))]
                        (if gv
                          (assoc system :mishap (calc-mishap-probability gv distance))
                          system))))

               (sort-nearest))]

    ($ :div
       ($ :h3 {:class (css :text-xl)}
          (str "Active System: " (system-str system)))
       ($ Galaxy {:galaxy d :on-system-selected on-system-selected}))))

(defui SpeciesInfo [{:keys [state on-species-change]}]
  (let [species (:species state)]
    ($ Input {:prefix "GV" :attr {:defaultValue (or (:GV species) js/undefined)
                                  :min 0
                                  :max 200
                                  :type :number} :on-change #(on-species-change
                                                              (assoc species :GV (parse-long %)))})))

(defui App [{:keys [galaxy]}]
  (let [[state set-state] (uix/use-state {:galaxy galaxy
                                          :selected (storage/get-item "active-system")
                                          :species (storage/get-item "species")})
        on-species-change (fn [species]
                            (when species
                              (storage/set-item! "species" species)
                              (set-state
                               (assoc-in state [:species] species))))
        on-system-selected (fn [system]
                             (when system
                               (storage/set-item! "active-system" system)
                               (set-state
                                (assoc-in state [:selected] system))))]
    ($ :.app {:class (css :p-2)}
       ($ :h1 {:class (css :text-3xl :mb-4)} "FH Web Utils")
       ($ :p "A collection of simple utilities for Far Horizons: Delta Galaxy. All data stays in your browser, nothing is sent to anyone else.")
       ($ :p "Current features:")
       ($ :ul {:class (css :mb-4 {:list-style-type "disc"})}
          ($ :li {:class (css :ml-8)} "Distance calculator - Choose a star system, then you will see the distances from that system to all the other systems. Optionally enter your Gravitics tech level to calculate the mishap probability. "))
       ($ :h2 {:class (css :text-xl :pt-4)} "Species Info")
       ($ SpeciesInfo {:state state :on-species-change on-species-change})
       ($ :h2 {:class (css :text-xl :pt-4)} "Lookup System")
       ($ LookupSystem {:state state :on-system-selected on-system-selected})
       ($ :div  {:class (css :pt-4)}
          (if (:selected state)
            ($ Distance {:state state :on-system-selected on-system-selected})
            ($ Galaxy {:galaxy (:galaxy state) :on-system-selected on-system-selected}))))))

(defn init [galaxy]
  (let [root (uix.dom/create-root (js/document.getElementById "root"))]
    (uix.dom/render-root ($ App {:galaxy galaxy}) root)
    nil))

(defn ^:export main []
  (load-galaxy init))
