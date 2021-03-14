;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020-2021 UXBOX Labs SL

(ns app.util.text-editor
  "Draft related abstraction functions."
  (:require
   [cuerdas.core :as str]
   [app.util.transit :as t]
   [clojure.walk :as walk]
   [app.common.data :as d]
   [app.common.attrs :as attrs]))

(defn encode-style-value
  [v]
  (cond
    (string? v)  (str "s:" v)
    (number? v)  (str "n:" v)
    (keyword? v) (str "k:" (name v))
    :else (str "o:" v)))

(defn decode-style-value
  [v]
  (let [prefix (subs v 0 2)]
    (case prefix
      "s:" (subs v 2)
      "n:" (js/Number (subs v 2))
      "k:" (keyword (subs v 2))
      "o:" (subs v 2)
      v)))

(defn encode-style
  [key val]
  (let [k (d/name key)
        v (encode-style-value val)]
    (str "PENPOT$$$" k "$$$" v)))

(defn encode-style-prefix
  [key]
  (let [k (d/name key)]
    (str "PENPOT$$$" k "$$$")))

(defn attrs-to-styles
  [attrs]
  (reduce-kv (fn [res k v]
               (conj res (encode-style k v)))
             #{}
             attrs))

(defn styles-to-attrs
  [styles]
  (persistent!
   (reduce (fn [result style]
             (let [[_ k v] (str/split style "$$$" 3)]
               (assoc! result (keyword k) (decode-style-value v))))
           (transient {})
           (seq styles))))

(defn styles-to-values
  [styles]
  (persistent!
   (reduce (fn [result style]
             (let [[_ k v] (str/split style "$$$" 3)]
               (conj! result (decode-style-value v))))
           (transient #{})
           (seq styles))))

(defn parse-style-ranges
  "Parses draft-js style ranges, converting encoded style name into a
  key/val pair of data."
  [ranges]
  (map (fn [{:keys [style] :as item}]
         (if (str/starts-with? style "PENPOT$$$")
           (let [[_ k v] (str/split style "$$$" 3)]
             (assoc item
                    :key (keyword k)
                    :val (decode-style-value v)))))
       ranges))

(defn build-style-index
  "Generates a character based index with associated styles map."
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

;; NOTE: this function will become useles
(defn parse-sections
  "Parses the draft-js block in to contiguos sections based on inline
  styles associated with ranges of text."
  [{:keys [text inlineStyleRanges] :as block}]
  (let [ranges (parse-style-ranges inlineStyleRanges)]
    (->> (build-style-index text ranges)
         (d/enumerate)
         (partition-by second)
         (map (fn [part]
                (let [start (ffirst part)
                      end   (inc (first (last part)))]
                  {:start start
                   :end   end
                   :text  (subs text start end)
                   :attrs (second (first part))}))))))

;; TODO: expect JS data sturcture
(defn draft->penpot
  [{:keys [blocks]}]
  (letfn [(build-text [text part]
            (let [start (ffirst part)
                  end   (inc (first (last part)))]
              (-> (second (first part))
                  (assoc :text (subs text start end)))))

          (split-texts [text ranges]
            (->> (parse-style-ranges ranges)
                 (build-style-index text)
                 (d/enumerate)
                 (partition-by second)
                 (mapv (partial build-text text))))

          (build-paragraph [{:keys [key text inlineStyleRanges data]}]
            (-> data
                (assoc :key key)
                (assoc :type "paragraph")
                (assoc :children (split-texts text inlineStyleRanges))))]

    {:type "root"
     :children
     [{:type "paragraph-set"
       :children (mapv build-paragraph blocks)}]}))

;; TODO: convert directly to JS data structure
(defn penpot->draft
  [node]
  (letfn [(calc-attr-ranges [children [k v]]
            (loop [children (seq children)
                   start    nil
                   offset   0
                   ranges   []]
              (if-let [child (first children)]
                (if (= v (get child k ::novalue))
                  (recur (rest children)
                         (if (nil? start) offset start)
                         (+ offset (count (:text child)))
                         ranges)
                  (if (some? start)
                    (recur (rest children)
                           nil
                           (+ offset (count (:text child)))
                           (conj ranges {:offset start
                                         :length (- offset start)
                                         :style (encode-style k v)}))
                    (recur (rest children)
                           start
                           (+ offset (count (:text child)))
                           ranges)))
                (cond-> ranges
                  (some? start)
                  (conj {:offset start
                         :length (- offset start)
                         :style (encode-style k v)})))))

          (calc-ranges [{:keys [children]}]
            (let [xform (comp (map #(dissoc % :key :text))
                              (remove empty?)
                              (mapcat vec)
                              (distinct)
                              (map #(calc-attr-ranges children %))
                              (mapcat vec))]
              (into [] xform children)))

          (build-block [{:keys [key children] :as paragraph}]
            {:key key
             :depth 0
             :text (apply str (map :text children))
             :data (dissoc paragraph :key :children :type)
             :type "unstyled"
             :entityRanges []
             :inlineStyleRanges (calc-ranges paragraph)})]

    {:blocks (->> (tree-seq map? :children node)
                  (filter #(= (:type %) "paragraph"))
                  (mapv build-block))
     :entityMap {}}))

(defn immutable-map->map
  [obj]
  (into {} (map (fn [[k v]] [(keyword k) v])) (seq obj)))
