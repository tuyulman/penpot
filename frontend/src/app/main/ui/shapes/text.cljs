;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020-2021 UXBOX Labs SL

(ns app.main.ui.shapes.text
  (:require
   [app.common.data :as d]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.shapes :as geom]
   [app.main.ui.context :as muc]
   [app.main.ui.shapes.text.embed :as ste]
   [app.main.ui.shapes.text.styles :as sts]
   [app.util.color :as uc]
   [app.util.dom :as dom]
   [app.util.object :as obj]
   [app.util.perf :as perf]
   [app.util.text :as txt]
   [cuerdas.core :as str]
   [rumext.alpha :as mf]))

(defn- get-section-markup
  [section]
  (let [style (sts/generate-text-styles* (:attrs section))]
    (mf/create-element "span" #js {:style style} #js [(:text section)])))

(defn- get-block-inline-markup
  [shape block]
  (let [sections (txt/parse-sections block)]
    (for [item sections]
      (get-section-markup item))))

(defn- get-block-markup
  [shape block]
  (let [data  (get block :data)
        style (sts/generate-paragraph-styles* shape data)]
    (mf/create-element "div" #js {:dir "auto" :style style}
                       (into-array (get-block-inline-markup shape block)))))

(mf/defc text-content
  {::mf/wrap-props false}
  [props]
  (let [shape     (obj/get props "shape")
        grow-type (obj/get props "grow-type")
        content   (:content2 shape)
        entities  (get content :entityMap)
        blocks    (get content :blocks)
        embed?    (obj/get props "embed-fonts?")]

    [:div.root.rich-text
     {:style (sts/generate-root-styles* shape)
      :xmlns "http://www.w3.org/1999/xhtml"}
     [:style ".gradient { background: var(--text-color); -webkit-text-fill-color: transparent; -webkit-background-clip: text;"]
     (when embed?
       [ste/embed-fontfaces-style {:shape shape}])
     [:div.paragraph-set {:style (sts/generate-paragraph-set-styles* grow-type)}
      (for [block blocks]
        (get-block-markup shape block))]]))

(defn- retrieve-colors
  [{:keys [content2] :as shape}]
  (let [prefix (txt/encode-style-prefix :fill-color)
        styles (into #{}
                     (comp (mapcat :inlineStyleRanges)
                           (map :style)
                           (filter #(str/starts-with? % prefix)))
                     (:blocks content2))
        colors (txt/styles-to-values styles)
        colors (conj colors "#000000")]
    (apply str (interpose "," colors))))

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
                       :grow-type grow-type
                       :embed-fonts? embed-fonts?}]]))
