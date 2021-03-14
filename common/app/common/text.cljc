;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020-2021 UXBOX Labs SL

(ns app.common.text
  (:require
   [app.common.attrs :as attrs]
   [app.common.data :as d]
   [app.util.transit :as t]
   [clojure.walk :as walk]
   [cuerdas.core :as str]))

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

;; TODO:
(defn some-node
  [predicate node]
  (or (predicate node)
      (some #(some-node predicate %) (:children node))))

;; TODO: used in libraries_helpers, workspace text editor and util.svg (?)
(defn map-node
  [map-fn node]
  (cond-> (map-fn node)
    (:children node) (update :children (fn [children] (mapv #(map-node map-fn %) children)))))

(defn transform-nodes
  [pred transform data]
  (walk/postwalk
   (fn [item]
     (if (and (map? item) (pred item))
       (transform item)
       item))
   data))

(defn ^boolean is-text-node?
  [node]
  (and (map? node)
       (string? (:text node))))

(defn ^boolean is-paragraph-node?
  [node]
  (and (map? node)
       (= "paragraph" (:type node))))

(defn ^boolean is-root-node?
  [node]
  (and (map? node)
       (= "root" (:type node))))

(defn nodes-seq
  ([root] (nodes-seq nil root))
  ([match? root]
   (cond->> (tree-seq map? :children root)
     (fn? match?)
     (filter match?))))

;; TODO: rename node->text
(defn content->text
  [node]
  (str
   (if (:children node)
     (str/join (if (= "paragraph-set" (:type node)) "\n" "") (map content->text (:children node)))
     (:text node ""))))


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

;; TODO: used on multiple selection options, remove
(defn get-text-attrs-multi
  [node attrs]
  (let [nodes (content->nodes node)]
    (attrs/get-attrs-multi nodes attrs)))
