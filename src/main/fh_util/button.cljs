(ns fh-util.button
  (:require
   [fh-util.ui :refer [cs]]
   ;; [app.ui.tooltip :as tooltip]
   [shadow.css :refer (css)]
   [uix.core :as uix :refer [defui $]]))

(def sizes
  {:xs (css  :py-1 :px-2 :text-xs)
   :sm (css :py-1 :px-2 :text-sm)
   :md (css  :py-1 :px-2 :text-base)
   :info-panel (css  :py-0 {:padding-left "8px" :padding-right "8px" } :text-info-panel)
   :lg (css :py-2.5 :px-3.5 :text-sm)
   :xl (css :py-3 :px-4 :text-sm)})

(def variant-base {:solid (css :font-bold [:focus-visible :outline-2 :outline :outline-offset-2])
                   :soft ""
                   :ghost ""
                   :outline ""})

(def radii {:none ""
            :small (css :rounded-sm)
            :medium (css :rounded)
            :large (css :rounded)
            :full (css :rounded-full)})

(def colors {:primary {:solid (css  :bg-rust-500 [:hover :bg-rust-400 :text-black] [:focus-visible :outline-rust-500])}
             :secondary {:solid (css :bg-hillary-500 [:hover :bg-hillary-400 :text-black] [:focus-visible :outline-hillary-500])}
             :yellow {:solid (css :bg-yellow-600 [:hover :bg-yellow-400 :text-white] [:focus-visible :outline-yellow-500])}
             :destructive {:solid (css :bg-red-500 [:hover :bg-red-400] [:focus-visible :outline-red-500])}
             :create {:solid (css  :bg-sycamore-500 [:hover :bg-sycamore-400 :text-black] [:focus-visible :outline-sycamore-500])}})

(defui Button [{:keys [size variant  radius children color on-click disabled? type]
                :or {size :md
                     type :button
                     color :primary
                     variant :solid
                     radius :none
                     disabled? false
                     }}]
  (let [size-class (get sizes size)
        color-class (get-in colors [color variant])
        variant-base-class (get variant-base variant)
        radius-class (get radii radius)
        disabled-class (css :bg-mine-shaft-900 :text-mine-shaft-600 :cursor-not-allowed [:hover :bg-mine-shaft-900 :text-mine-shaft-600] [:focus-visible :outline-mine-shaft-900])
        ]
    ($ :button {:class (cs size-class variant-base-class color-class radius-class (when disabled? disabled-class))
                :on-click on-click
                :disabled disabled?
                :type type}
       children)))
