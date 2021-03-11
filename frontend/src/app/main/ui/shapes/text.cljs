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
   [app.common.data :as d]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.shapes :as geom]
   [app.main.ui.context :as muc]
   [app.main.ui.shapes.text.embed :as ste]
   [app.main.ui.shapes.text.styles :as sts]
   [app.util.color :as uc]
   [app.util.object :as obj]
   [app.util.perf :as perf]
   [cuerdas.core :as str]
   [rumext.alpha :as mf]))


;; --- NEW RENDER IMPL

;; (defn get-sections
;;   [{:keys [text entityRanges] :as block}]
;;   ;; TODO: sort ranges
;;   ;; TODO: use transients
;;   (loop [ranges (->> entityRanges
;;                      (sort-by :offset)
;;                      (seq))
;;          result []
;;          offset 0]
;;     (if-let [item (first ranges)]
;;       (recur (rest ranges)
;;              (cond-> result
;;                (> (get item :offset) offset)
;;                (as-> $ (let [start offset
;;                              end   (get item :offset)]
;;                          (conj $ {:start start
;;                                   :end   end
;;                                   :text  (subs text start (+ end))})))

;;                :always
;;                (as-> $ (let [start (get item :offset)
;;                              end   (+ start (get item :length))]
;;                          (conj $ {:start  start
;;                                   :end    end
;;                                   :text   (subs text start (+  end))
;;                                   :entity (keyword (str (get item :key)))}))))

;;              (+ (get item :offset) (get item :length)))

;;       (cond-> result
;;         (< offset (count text))
;;         (as-> $ (let [start offset
;;                       end   (count text)]
;;                   (conj $ {:start start
;;                            :end   end
;;                            :text  (subs text start (+  end))})))))))


(defn parse-ranges
  [ranges]
  (map (fn [{:keys [style] :as item}]
         (if (str/starts-with? style "PENPOT$$")
           (let [[k v] (str/split (subs style 8) ":")]
             (assoc item
                    :key (keyword (str/lower k))
                    :val (str/lower v)))))
       ranges))

(defn get-style-index
  [text ranges]
  (loop [result (->> (range (count text))
                     (mapv (constantly {}))
                     (transient))
         ranges (seq ranges)]
    (if-let [{:keys [offset length] :as item} (first ranges)]
      (recur (reduce (fn [result index]
                       (let [prev (get result index)]
                         (assoc! result index (assoc prev (:key item) (:val item)))))
                     result
                     (range offset (+ offset length)))
             (rest ranges))
      (persistent! result))))

(defn get-sections*
  [{:keys [text inlineStyleRanges] :as block}]
  (let [ranges (parse-ranges inlineStyleRanges)]
    (->> (get-style-index text ranges)
         (d/enumerate)
         (partition-by second)
         (map (fn [part]
                (let [start (ffirst part)
                      end   (inc (first (last part)))]
                  {:text  (subs text start end)
                   :attrs (second (first part))}))))))

(defn get-section-markup
  [section]
  (mf/create-element "span" #js {} #js [(:text section)]))

(defn get-entity-markup
  [section]
  (let [style (sts/generate-text-styles* (:attrs section))]
    (mf/create-element "span" #js {:style style} #js [(:text section)])))

(defn get-block-inline-markup
  [shape block ]
  (let [sections (get-sections* block)]
    (for [item sections]
      (get-entity-markup item))))

(defn get-block-markup
  [shape block]
  (let [data  (get block :data)
        style (sts/generate-paragraph-styles* shape data)]
    (mf/create-element "div" #js {:dir "auto" :style style}
                       (into-array (get-block-inline-markup shape block)))))

(mf/defc text-content
  {::mf/wrap-props false}
  [props]
  (let [shape    (obj/get props "shape")
        content  (:content2 shape)
        entities (get content :entityMap)
        blocks   (get content :blocks)
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
      (for [block blocks]
        (get-block-markup shape block))]]))

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


