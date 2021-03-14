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
   ["draft-js" :as draft]
   [app.common.attrs :as attrs]
   [app.common.data :as d]
   [app.util.transit :as t]
   [clojure.walk :as walk]
   [cuerdas.core :as str]))

;; --- INLINE STYLES ENCODING

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

;; --- CONVERSION

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
    (clj->js
     {:blocks (->> (tree-seq map? :children node)
                   (filter #(= (:type %) "paragraph"))
                   (mapv build-block))
      :entityMap {}})))

(defn immutable-map->map
  [obj]
  (into {} (map (fn [[k v]] [(keyword k) v])) (seq obj)))


;; --- DRAFT-JS HELPERS

(defn create-editor-state
  ([]
   (.createEmpty ^js draft/EditorState))
  ([content]
   (.createWithContent ^js draft/EditorState content)))

(defn import-content
  [content]
  (-> content penpot->draft draft/convertFromRaw))

(defn export-content
  [content]
  (-> content
      (draft/convertToRaw)
      (js->clj :keywordize-keys true)
      (draft->penpot)))

(defn get-editor-current-content
  [state]
  (.getCurrentContent ^js state))

(defn ^boolean content-has-text?
  [content]
  (.hasText ^js content))

(defn editor-select-all
  [state]
  (let [content   (get-editor-current-content state)
        fblock    (.. ^js content getBlockMap first)
        lblock    (.. ^js content getBlockMap last)
        fbk       (.getKey ^js fblock)
        lbk       (.getKey ^js lblock)
        lbl       (.getLength ^js lblock)
        params    #js {:anchorKey fbk
                       :anchorOffset 0
                       :focusKey lbk
                       :focusOffset lbl}
        selection (draft/SelectionState. params)]
    (.forceSelection ^js draft/EditorState state selection)))

(defn get-editor-current-block-data
  [state]
  (let [content (.getCurrentContent ^js state)
        key     (.. ^js state getSelection getStartKey)
        block   (.getBlockForKey ^js content key)]
    (-> (.getData ^js block)
        (immutable-map->map))))

(defn get-editor-current-inline-styles
  [state]
  (-> (.getCurrentInlineStyle ^js state)
      (styles-to-attrs)))

(defn update-editor-current-block-data
  [state attrs]
  (loop [selection (.getSelection ^js state)
         start-key (.getStartKey ^js selection)
         end-key   (.getEndKey ^js selection)
         content   (.getCurrentContent ^js state)
         target    selection]
    (if (and (not= start-key end-key)
             (zero? (.getEndOffset ^js selection)))
      (let [before-block (.getBlockBefore ^js content end-key)]
        (recur selection
               start-key
               (.getKey ^js before-block)
               content
               (.merge ^js target
                       #js {:anchorKey start-key
                            :anchorOffset (.getStartOffset ^js selection)
                            :focusKey end-key
                            :focusOffset (.getLength ^js before-block)
                            :isBackward false})))
      (.push ^js draft/EditorState
             state
             (.mergeBlockData ^js draft/Modifier content target (clj->js attrs))
             "change-block-data"))))

(defn update-editor-current-inline-styles
  [state attrs]
  (let [selection (.getSelection ^js state)
        content   (.getCurrentContent ^js state)
        styles    (attrs-to-styles attrs)]
    (reduce (fn [state style]
              (let [modifier (.applyInlineStyle draft/Modifier
                                                (.getCurrentContent ^js state)
                                                selection
                                                style)]
                (.push draft/EditorState state modifier "change-inline-style")))
            state
            styles)))
