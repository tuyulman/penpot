;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020-2021 UXBOX Labs SL

(ns app.main.ui.shapes.text.embed
  (:require
   [app.common.data :as d]
   [app.main.data.fetch :as df]
   [app.main.fonts :as fonts]
   [app.util.object :as obj]
   [app.util.text :as txt]
   [clojure.set :as set]
   [cuerdas.core :as str]
   [promesa.core :as p]
   [rumext.alpha :as mf]))

(def font-face-template "
/* latin */
@font-face {
  font-family: '%(family)s';
  font-style: %(style)s;
  font-weight: %(weight)s;
  font-display: block;
  src: url(/fonts/%(family)s-%(style)s.woff) format('woff');
}
")

(def ^:private xf-filter-styles
  (let [prefix1 (txt/encode-style-prefix :font-id)
        prefix2 (txt/encode-style-prefix :font-variant-id)]
    (comp
     (map :style)
     (filter #(or (str/starts-with? % prefix1)
                  (str/starts-with? % prefix2))))))

(def ^:private xf-transform-styles-group
  (comp
   (map #(into #{} xf-filter-styles %))
   (filter (complement empty?))
   (map txt/styles-to-attrs)))

(defn get-fonts
  [{:keys [content2] :as shape}]
  (loop [blocks (seq (:blocks content2))
         result (transient #{})]
    (if-let [block (first blocks)]
      (recur (rest blocks)
             (->> (:inlineStyleRanges block)
                  (group-by (juxt :offset :start))
                  (vals)
                  (transduce xf-transform-styles-group conj! result)))
      (persistent! result))))

(defn get-local-font-css
  [font-id font-variant-id]
  (let [{:keys [family variants] :as font}      (get @fonts/fontsdb font-id)
        {:keys [name weight style] :as variant} (d/seek #(= (:id %) font-variant-id) variants)]
    (-> (str/format font-face-template {:family family :style style :width weight})
        (p/resolved))))

(defn get-text-font-data [text]
  (->> text
       (re-seq #"url\(([^)]+)\)")
       (map second)
       (map df/fetch-as-data-uri)
       (p/all)))

(defn embed-font
  [{:keys [font-id font-variant-id] :or {font-variant-id "regular"}}]
  (let [{:keys [backend]} (get @fonts/fontsdb font-id)]
    (p/let [font-text (case backend
                        :google (fonts/fetch-font font-id font-variant-id)
                        (get-local-font-css font-id font-variant-id))
            url-to-data (get-text-font-data font-text)
            replace-text (fn [text [url data]] (str/replace text url data))]
      (reduce replace-text font-text url-to-data))))

(mf/defc embed-fontfaces-style
  {::mf/wrap-props false}
  [props]
  (let [shape (obj/get props "shape")
        style (mf/use-state nil)]
    (mf/use-effect
     (mf/deps shape)
     (fn []
       (let [font-to-embed (get-fonts shape)
             font-to-embed (if (empty? font-to-embed) #{txt/default-text-attrs} font-to-embed)
             embeded       (map embed-font font-to-embed)]
         (-> (p/all embeded)
             (p/then (fn [result] (reset! style (str/join "\n" result))))))))

    (when (some? @style)
      [:style @style])))
