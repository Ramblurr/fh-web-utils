(ns fh-util.ui
(:require
 ;; [app.ui.tooltip :as tooltip]
 [shadow.css :refer (css)]
 [uix.core :as uix :refer [defui $]]
 [clojure.string :as string])
  )

(defn cs [& names]
  (string/join " " (filter identity names)))





(defui Input [{:keys [form attr valid? prefix suffix on-change]}]
  (let [$align (if (= (:type attr) :number)
                 (css :text-right)
                 (css :text-left))
        $base (css :border-b  :text-white :px-1 {:height "17px" :line-height "17px"})
        $input-base (css :w-10)
        $form-color (if valid?
                      (css :bg-form-valid-500 :border-form-valid-400)
                      (css :bg-form-invalid-500 :border-form-invalid-400))]
    ($ :div {:class (css :flex)}
       (when prefix
         ($ :div {:class (cs (css :inline-block :border-r-2 :font-bold) $align $base $form-color)} prefix))
       ($ :input (merge {:on-change #(on-change (.. % -target -value)) :class (cs $align $base $form-color $input-base) :type "text"} form attr))
       (when suffix
         ($ :div {:class (cs (css :inline-block) $align $base $form-color)} suffix)))))
