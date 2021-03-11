(ns app.util.text
  (:require
   [cuerdas.core :as str]
   [app.util.transit :as t]
   [app.common.data :as d]
   [app.common.attrs :refer [get-attrs-multi]]))

(def default-text-attrs
  {:typography-ref-file nil
   :typography-ref-id nil
   :font-id "sourcesanspro"
   :font-family "sourcesanspro"
   :font-variant-id "regular"
   :font-size "14"
   :font-weight "400"
   :font-style "normal"
   :line-height "1.2"
   :letter-spacing "0"
   :text-transform "none"
   :text-align "left"
   :text-decoration "none"
   :fill-color nil
   :fill-opacity 1})

(def typography-fields
  [:font-id
   :font-family
   :font-variant-id
   :font-size
   :font-weight
   :font-style
   :line-height
   :letter-spacing
   :text-transform])

(def default-typography
  (merge
   {:name "Source Sans Pro Regular"}
   (select-keys default-text-attrs typography-fields)))

(defn some-node
  [predicate node]
  (or (predicate node)
      (some #(some-node predicate %) (:children node))))

(defn map-node
  [map-fn node]
  (cond-> (map-fn node)
    (:children node) (update :children (fn [children] (mapv #(map-node map-fn %) children)))))

(defn content->text
  [node]
  (str
   (if (:children node)
     (str/join (if (= "paragraph-set" (:type node)) "\n" "") (map content->text (:children node)))
     (:text node ""))))

(defn parse-style-text-blocks
  [node attrs]
  (letfn
      [(rec-style-text-map [acc node style]
         (let [node-style (merge style (select-keys node attrs))
               head (or (-> acc first) [{} ""])
               [head-style head-text] head

               new-acc
               (cond
                 (:children node)
                 (reduce #(rec-style-text-map %1 %2 node-style) acc (:children node))

                 (not= head-style node-style)
                 (cons [node-style (:text node "")] acc)

                 :else
                 (cons [node-style (str head-text "" (:text node))] (rest acc)))

               ;; We add an end-of-line when finish a paragraph
               new-acc
               (if (= (:type node) "paragraph")
                 (let [[hs ht] (first new-acc)]
                   (cons [hs (str ht "\n")] (rest new-acc)))
                 new-acc)]
           new-acc))]

    (-> (rec-style-text-map [] node {})
        reverse)))

(defn search-text-attrs
  [node attrs]
  (let [rec-fn
        (fn rec-fn [current node]
          (let [current (reduce rec-fn current (:children node []))]
            (merge current
                   (select-keys node attrs))))]
    (rec-fn {} node)))


(defn content->nodes [node]
  (loop [result (transient [])
         curr node
         pending (transient [])]

    (let [result (conj! result curr)]
      ;; Adds children to the pending list
      (let [children (:children curr)
            pending (loop [child (first children)
                           children (rest children)
                           pending pending]
                      (if child
                        (recur (first children)
                               (rest children)
                               (conj! pending child))
                        pending))]

        (if (= 0 (count pending))
          (persistent! result)
          ;; Iterates with the next value in pending
          (let [next (get pending (dec (count pending)))]
            (recur result next (pop! pending))))))))

(defn get-text-attrs-multi
  [node attrs]
  (let [nodes (content->nodes node)]
    (get-attrs-multi nodes attrs)))


;; --- NEW IMPL

(defn- encode-style-value
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
                  {:text  (subs text start end)
                   :attrs (second (first part))}))))))

(defn encode-style
  [key val]
  (let [k (d/name key)
        v (encode-style-value val)]
    (str "PENPOT$$$" k "$$$" v)))

(defn encode-partial-style
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


