;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns app.main.ui.shapes.text
  (:require
   [cuerdas.core :as str]
   [rumext.alpha :as mf]
   [app.main.ui.context :as muc]
   [app.common.data :as d]
   [app.common.geom.shapes :as geom]
   [app.common.geom.matrix :as gmt]
   [app.util.object :as obj]
   [app.util.color :as uc]
   [app.main.ui.shapes.text.styles :as sts]
   [app.main.ui.shapes.text.embed :as ste]
   [app.util.perf :as perf]))


;; --- NEW RENDER IMPL

(defn get-sections
  [{:keys [text] :as block}]
  (let [ranges (map (fn [range]
                      (assoc range :type :entity))
                    (:entityRanges block))]
    ;; TODO: sort ranges
    (loop [ranges (seq ranges)
           result []
           offset 0]
      (if-let [item (first ranges)]
        (recur (rest ranges)
               (cond-> result
                 (> (:offset item) offset)
                 (as-> $ (let [start offset
                               end   (:offset item)]
                           (conj $ {:start start
                                    :end   end
                                    :text  (subs text start (+ start end))})))

                 :always
                 (as-> $ (let [start (:offset item)
                               end   (+ start (:length item))]
                           (conj $ {:start start
                                    :end   end
                                    :text  (subs text start (+ start end))
                                    :ekey  (:key item)
                                    :type  (:type item)}))))
               (+ (:offset item) (:length item)))

        (cond-> result
          (< offset (count text))
          (as-> $ (let [start offset
                        end   (count text)]
                    (conj $ {:start start
                             :end   end
                             :text  (subs text start (+ start end))}))))))))

(defn get-section-markup
  [section]
  (mf/create-element "span" #js {} #js [(:text section)]))

(defn get-entity-markup
  [section data]
  (prn "get-entity-markup" data)
  (mf/create-element "span" #js {} #js [(:text section)]))

(defn get-block-inline-markup
  [shape block entities]
  (let [sections (get-sections block)]
    (for [item sections]
      (if (= :entity (:type item))
        (get-entity-markup item (get entities (:ekey item)))
        (get-section-markup item)))))

(defn get-block-markup
  [shape block entities]
  (prn "get-block-markup" entities)
  (let [style (sts/generate-paragraph-styles* shape (:data block))]
    (mf/create-element "div" #js {:dir "auto" :style style}
                       (into-array (get-block-inline-markup shape block entities)))))

(mf/defc text-content
  {::mf/wrap-props false}
  [props]
  (let [shape    (obj/get props "shape")
        content  (:content2 shape)
        entities (:entityMap content)   ; TODO: rename?
        embed?   (obj/get props "embed-fonts?")
        style    (sts/generate-root-styles* shape)]

    [:div.rich-text
     {:style style
      :xmlns "http://www.w3.org/1999/xhtml"}
     [:*
      [:style ".gradient { background: var(--text-color); -webkit-text-fill-color: transparent; -webkit-background-clip: text;"]
      ;; TODO
      #_(when embed-fonts?
          [ste/embed-fontfaces-style {:content root}])
      (for [block (:blocks content)]
        (get-block-markup shape block entities))]]))

;; TODO
(defn- retrieve-colors
  [shape]
  (let [colors (->> shape
                    :content
                    (tree-seq map? :children)
                    (into #{} (comp (map :fill-color) (filter string?))))]
    (if (empty? colors)
      "#000000"
      (apply str (interpose "," colors)))))

(mf/defc text-shape
  {::mf/wrap-props false
   ::mf/forward-ref true}
  [props ref]
  (let [shape     (unchecked-get props "shape")
        grow-type (unchecked-get props "grow-type")
        embed-fonts? (mf/use-ctx muc/embed-ctx)
        {:keys [id x y width height content]} shape
        ;; We add 8px to add a padding for the exporter
        ;; width (+ width 8)
        ]
    [:foreignObject {:x x
                     :y y
                     :id (:id shape)
                     :data-colors (retrieve-colors shape)
                     :transform (geom/transform-matrix shape)
                     :width  (if (#{:auto-width} grow-type) 100000 width)
                     :height (if (#{:auto-height :auto-width} grow-type) 100000 height)
                     :ref ref}
     [:& text-content {:shape shape
                       :embed-fonts? embed-fonts?}]]))


;; --- OLD RENDER IMPL

;; (mf/defc render-text
;;   {::mf/wrap-props false}
;;   [props]
;;   (let [node (obj/get props "node")
;;         text (:text node)
;;         style (sts/generate-text-styles props)]
;;     [:span {:style style
;;             :className (when (:fill-color-gradient node) "gradient")}
;;      (if (= text "") "\u00A0" text)]))

;; (mf/defc render-root
;;   {::mf/wrap-props false}
;;   [props]
;;   (let [node (obj/get props "node")
;;         embed-fonts? (obj/get props "embed-fonts?")
;;         children (obj/get props "children")
;;         style (sts/generate-root-styles props)]
;;     [:div.root.rich-text
;;      {:style style
;;       :xmlns "http://www.w3.org/1999/xhtml"}
;;      [:*
;;       [:style ".gradient { background: var(--text-color); -webkit-text-fill-color: transparent; -webkit-background-clip: text;"]
;;       (when embed-fonts?
;;         [ste/embed-fontfaces-style {:node node}])]
;;      children]))

;; (mf/defc render-paragraph-set
;;   {::mf/wrap-props false}
;;   [props]
;;   (let [node (obj/get props "node")
;;         children (obj/get props "children")
;;         style (sts/generate-paragraph-set-styles props)]
;;     [:div.paragraph-set {:style style} children]))

;; (mf/defc render-paragraph
;;   {::mf/wrap-props false}
;;   [props]
;;   (let [node (obj/get props "node")
;;         children (obj/get props "children")
;;         style (sts/generate-paragraph-styles props)]
;;     [:p.paragraph {:style style} children]))

;; -- Text nodes

;; (mf/defc render-node
;;   {::mf/wrap-props false}
;;   [props]
;;   (let [node (obj/get props "node")
;;         index (obj/get props "index")
;;         {:keys [type text children]} node]
;;     (if (string? text)
;;       [:> render-text props]

;;       (let [component (case type
;;                         "root" render-root
;;                         "paragraph-set" render-paragraph-set
;;                         "paragraph" render-paragraph
;;                         nil)]
;;         (when component
;;           [:> component (obj/set! props "key" index)
;;            (for [[index child] (d/enumerate children)]
;;              (let [props (-> (obj/clone props)
;;                              (obj/set! "node" child)
;;                              (obj/set! "index" index)
;;                              (obj/set! "key" index))]
;;                [:> render-node props]))])))))


