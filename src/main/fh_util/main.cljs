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

(defui System [{:keys [scanned? on-system-selected on-mark-scanned system has-distance? has-mishap? selected?]}]
  (let [{:keys [coord stellar-type]} system
        [x y z] coord
        $col (css :px-2 :text-right)
        $scanned (when scanned? (css :bg-sycamore-500))
        $selected (when selected?
                    (css :bg-gray-500 :text-white :font-bold))]
    ($ :tr
       ($ :td {:class (ui/cs $scanned $col $selected)} x)
       ($ :td {:class (ui/cs $scanned $col $selected)} y)
       ($ :td {:class (ui/cs $scanned $col $selected)} z)
       ($ :td {:class (ui/cs $scanned $col $selected)} stellar-type)
       (when has-distance?
         ($ :td {:class (ui/cs $scanned $col $selected)} (fmt-float (:distance system))))
       (when has-mishap?
         ($ :td {:class (ui/cs $scanned $col $selected)} (str (fmt-float (:mishap system)) "%")))
       ($ :td
          ($ :div {:class (css :flex :gap-2)}
             ($ Button {:on-click (fn []
                                    (on-mark-scanned (not scanned?) system))}
                (if scanned?
                  "unscan"
                  "scan"))
             (when (not selected?)
               ($ Button {:on-click (fn []
                                      (on-system-selected system))} "choose")))))))

(defui Galaxy [{:keys [state on-system-selected on-mark-scanned]}]
  (let [{:keys [galaxy ui]} state
        {:keys [scanned-systems active-system]} ui
        has-distance? (some? (:distance (first galaxy)))
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
            ($ System {:key (:coord system)
                       :scanned? (contains? scanned-systems (:coord system))
                       :selected? (= (:coord system) (:coord active-system))
                       :has-mishap? has-mishap? :has-distance? has-distance? :system system :on-system-selected on-system-selected :on-mark-scanned on-mark-scanned}))))))

(defn find-system [galaxy coord]
  (first (filter (fn [system]
                   (= (:coord system) coord))
                 galaxy)))

(defui LookupSystem [{:keys [on-system-selected state]}]
  (let [[x y z] (-> state :ui :active-system :coord)
        [values set-values] (uix/use-state {:x x :y y :z z})
        setter (fn [k] #(set-values (assoc values k (parse-long %))))]
    ($ :div {:class (css :flex :gap-2) :key [x y z]}
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

(defui SpeciesInfo [{:keys [state on-species-change]}]
  (let [species (-> state :ui :species)]
    ($ Input {:prefix "GV" :attr {:value (or (:GV species) js/undefined)
                                  :min 0
                                  :max 200
                                  :type :number} :on-change #(on-species-change
                                                              (assoc species :GV (parse-long %)))})))

(defn update-galaxy* [species galaxy system]
  (if system
    (->> (distances galaxy (:coord system))
         (map (fn [{:keys [distance] :as system}]
                (let [gv (:GV species)]
                  (if gv
                    (assoc system :mishap (calc-mishap-probability gv distance))
                    system))))

         (sort-nearest))
    galaxy))

(defui App [{:keys [initial-state]}]
  (let [[state set-state*] (uix/use-state initial-state)
        set-state (fn [new-state]
                    (storage/set-item! "ui-state" (:ui new-state))
                    (set-state* new-state))
        update-galaxy (fn [state]
                        (assoc-in state [:galaxy] (update-galaxy* (-> state :ui :species) (:galaxy state) (-> state :ui :active-system))))
        on-species-change (fn [species]
                            (when species
                              (set-state
                               (-> state
                                   (assoc-in  [:ui :species] species)
                                   (update-galaxy)))))
        on-mark-scanned (fn [scanned? system]
                          (set-state
                           (update-in state [:ui :scanned-systems] (if scanned? conj disj) (:coord system))))
        on-system-selected (fn [system]
                             (when system
                               (set-state
                                (-> state
                                    (assoc-in [:ui :active-system] system)
                                    (update-galaxy)))))]

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
          ($ Galaxy {:state state :on-system-selected on-system-selected :on-mark-scanned on-mark-scanned})))))

(defn init [galaxy]
  (let [{:keys [species active-system] :as ui-state} (or (storage/get-item "ui-state") {:active-system nil :species :nil :scanned-systems #{}})
        galaxy (if active-system
                 (update-galaxy* species galaxy active-system)
                 galaxy)
        root (uix.dom/create-root (js/document.getElementById "root"))]
    (uix.dom/render-root ($ App {:initial-state {:galaxy galaxy
                                                 :ui ui-state}}) root)
    nil))

(defn ^:export main []
  (load-galaxy init))
