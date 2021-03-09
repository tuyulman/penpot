;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns app.main.data.workspace.texts
  (:require
   ["slate" :as slate :refer [Editor Node Transforms Text]]
   ["slate-react" :as rslate]

   ["draft-js" :as draft]
   [app.common.math :as mth]
   [app.common.attrs :as attrs]
   [app.common.geom.shapes :as gsh]
   [app.common.pages :as cp]
   [app.common.data :as d]
   [app.main.data.workspace.common :as dwc]
   [app.main.data.workspace.transforms :as dwt]
   [app.main.fonts :as fonts]
   [app.util.object :as obj]
   [app.util.text :as ut]
   [beicon.core :as rx]
   [cljs.spec.alpha :as s]
   [clojure.walk :as walk]
   [goog.object :as gobj]
   [potok.core :as ptk]))

(defn update-editor-state
  [estate]
  (ptk/reify ::update-editor-state
    ptk/UpdateEvent
    (update [_ state]
      (if (some? estate)
        (assoc state :workspace-editor estate)
        (dissoc state :workspace-editor)))))

(defn initialize-editor-state
  [{:keys [id content2] :as shape} decorator]
  (ptk/reify ::initialize-editor-state
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-editor
              (fn [_]
                (if content2
                  (as-> content2 $
                    (clj->js $)
                    (draft/convertFromRaw $)
                    (.createWithContent ^js draft/EditorState $ decorator))
                  (.createEmpty ^js draft/EditorState decorator)))))))

(defn finalize-editor-state
  [{:keys [id content2] :as shape}]
  (ptk/reify ::update-editor-state
    ptk/WatchEvent
    (watch [_ state stream]
      (let [estate  (:workspace-editor state)
            content (-> (.getCurrentContent estate)
                        (draft/convertToRaw)
                        (js->clj :keywordize-keys true))]
        (rx/merge
         (rx/of (update-editor-state nil))
         (when (not= content2 content)
           (rx/of (dwc/update-shapes [id] #(assoc % :content2 content)))))))))

(defn select-all
  "Select all content of the current editor. When not editor found this
  event is noop."
  []
  (letfn [(select-all [state]
            (let [content   (.getCurrentContent ^js state)
                  fblock    (.. ^js content getBlockMap first)
                  lblock    (.. ^js content getBlockMap last)
                  fbk       (.getKey ^js fblock)
                  lbk       (.getKey ^js lblock)
                  lbl       (.getLength ^js lblock)
                  selection (draft/SelectionState.
                             #js {:anchorKey fbk
                                  :anchorOffset 0
                                  :focusKey lbk
                                  :focusOffset lbl})]
              (.forceSelection ^js draft/EditorState state selection)))]
    (ptk/reify ::editor-select-all
      ptk/UpdateEvent
      (update [_ state]
        (d/update-when state :workspace-editor select-all)))))

;; --- Helpers

(defn get-editor-current-block
  [state]
  (let [content (.getCurrentContent ^js state)
        key     (.. ^js state getSelection getStartKey)]
    (.getBlockForKey ^js content key)))

(defn get-editor-current-block-data
  [state]
  (let [block (get-editor-current-block state)]
    (.getData ^js block)))

(defn get-editor-current-entity-key
  [state]
  (let [block   (get-editor-current-block ^js state)
        offset  (.. state getSelection getStartOffset)]
    (.getEntityAt ^js block offset)))

(defn get-editor-current-entity-data
  [state]
  (let [content (.getCurrentContent ^js state)]
    (when-let [key (get-editor-current-entity-key state)]
      (.. content (getEntity key) (getData)))))

(defn get-entity-data
  [{:keys [content2] :as shape}]
  (reduce (fn [res item]
            (d/merge res (:data item)))
          {}
          (vals (:entityMap content2))))

(defn get-block-data
  [{:keys [content2] :as shape}]
  (reduce (fn [res item]
            (d/merge res (:data item)))
          {}
          (:blocks content2)))

(defn current-text-values
  [{:keys [editor attrs shape]}]
  (if editor
    (-> (get-editor-current-entity-data editor)
        (js->clj :keywordize-keys true)
        (select-keys attrs))
    (-> (get-entity-data shape)
        (select-keys attrs))))

(defn immutable->clj
  [obj]
  (some-> obj
          (.toJS)
          (clj->js :keywordize-keys true)))

(defn current-paragraph-values
  [{:keys [editor attrs shape]}]
  (if editor
    (-> (get-editor-current-block-data editor)
        (immutable->clj)
        (select-keys attrs))
    (-> (get-block-data shape)
        (select-keys attrs))))

(defn- merge-attrs
  [node attrs]
  (reduce-kv (fn [node k v]
               (if (nil? v)
                 (dissoc node k)
                 (assoc node k v)))
             node
             attrs))

(defn impl-update-shape-attrs
  ([shape attrs]
   ;; NOTE: this arity is used in workspace for properly update the
   ;; fill color using colorpalette, then the predicate should be
   ;; defined.
   (impl-update-shape-attrs shape attrs nil #_is-text-node?))
  ([{:keys [type content] :as shape} attrs pred]
   (assert (= :text type) "should be shape type")
   #_(let [merge-attrs #(merge-attrs % attrs)]
     (update shape :content #(transform-nodes pred merge-attrs %)))))

;; (defn update-attrs
;;   [{:keys [id editor attrs pred split]
;;     :or {pred is-text-node?}}]
;;   (if editor
;;     (ptk/reify ::update-attrs
;;       ptk/EffectEvent
;;       (effect [_ state stream]
;;         (editor-set! editor (clj->js attrs) #js {:match pred :split split})))

;;     (ptk/reify ::update-attrs
;;       ptk/WatchEvent
;;       (watch [_ state stream]
;;         (let [objects (dwc/lookup-page-objects state)
;;               shape (get objects id)
;;               ids (cond (= (:type shape) :text)  [id]
;;                         (= (:type shape) :group) (cp/get-children id objects))]
;;           (rx/of (dwc/update-shapes ids #(impl-update-shape-attrs % attrs pred))))))))

;; --- TEXT EDITION IMPL

;; TODO: handle non editor changes
(defn update-paragraph-attrs
  [{:keys [id attrs]}]
  (letfn [(update-current-block [state]
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
                       "change-block-data"))))]
    (ptk/reify ::update-paragraph-attrs
      ptk/UpdateEvent
      (update [_ state]
        (update state :workspace-editor update-current-block)))))

;; (defn editor-current-entity
;;   [state]
;;   (let [content      (.getCurrentContent ^js state)
;;         selection    (.getSelection ^js state)
;;         start-key    (.getStartKey ^js selection)
;;         start-offset (.getStartOffset ^js selection)
;;         block        (.getBlockForKey ^js content start-key)]
;;     (.getEntityAt ^js block start-offset)))

;; (defn editor-current-entities
;;   [state]
;;   (let [content      (.getCurrentContent ^js state)
;;         selection    (.getSelection ^js state)
;;         start-key    (.getStartKey ^js selection)
;;         start-offset (.getStartOffset ^js selection)
;;         end-offset   (.getEndOffset ^js selection)
;;         block        (.getBlockForKey ^js content start-key)]
;;     (reduce (fn [res offset]
;;               (conj res (.getEntityAt ^js block offset)))
;;             #{}
;;             (range start-offset end-offset))))

(defn update-text-attrs
  [{:keys [id attrs]}]
  (letfn [(add-entity [state]
            (let [attrs     (clj->js attrs)
                  selection (.getSelection state)
                  content   (.getCurrentContent ^js state)
                  attrs     (-> (get-editor-current-entity-data state)
                                (obj/merge attrs))
                  content   (.createEntity ^js content
                                           "PENPOT"
                                           "MUTABLE"
                                           attrs)
                  ekey      (.getLastCreatedEntityKey ^js content)
                  state     (.set ^js draft/EditorState state #js {:currentContent content})
                  modifier  (.applyEntity draft/Modifier
                                          (.getCurrentContent ^js state)
                                          (.getSelection ^js state)
                                          ekey)]
              (.push draft/EditorState state modifier "apply-entity")))]
    (ptk/reify ::update-text-attrs
      ptk/UpdateEvent
      (update [_ state]
        (update state :workspace-editor add-entity)))))


;; --- RESIZE UTILS

(defn update-overflow-text [id value]
  (ptk/reify ::update-overflow-text
    ptk/UpdateEvent
    (update [_ state]
      (let [page-id (:current-page-id state)]
        (update-in state [:workspace-data :pages-index page-id :objects id] assoc :overflow-text value)))))


(def start-edit-if-selected
  (ptk/reify ::start-edit-if-selected
    ptk/UpdateEvent
    (update [_ state]
      (let [objects (dwc/lookup-page-objects state)
            selected (->> state :workspace-local :selected (map #(get objects %)))]
        (cond-> state
          (and (= 1 (count selected))
               (= (-> selected first :type) :text))
          (assoc-in [:workspace-local :edition] (-> selected first :id)))))))

(defn not-changed? [old-dim new-dim]
  (> (mth/abs (- old-dim new-dim)) 0.1))

(defn resize-text-batch [changes]
  (ptk/reify ::resize-text-batch
    ptk/WatchEvent
    (watch [_ state stream]
      (let [page-id  (:current-page-id state)

            objects0 (get-in state [:workspace-file :data :pages-index page-id :objects])
            objects1 (get-in state [:workspace-data :pages-index page-id :objects])]
        (if-not (every? #(contains? objects1(first %)) changes)
          (rx/empty)
          (let [change-text-shape
                (fn [objects [id [new-width new-height]]]
                  (when (contains? objects id)
                    (let [shape (get objects id)
                          {:keys [selrect grow-type overflow-text]} (gsh/transform-shape shape)
                          {shape-width :width shape-height :height} selrect

                          modifier-width (gsh/resize-modifiers shape :width new-width)
                          modifier-height (gsh/resize-modifiers shape :height new-height)

                          shape (cond-> shape
                                  (and overflow-text (not= :fixed grow-type))
                                  (assoc :overflow-text false)

                                  (and (= :fixed grow-type) (not overflow-text) (> new-height shape-height))
                                  (assoc :overflow-text true)

                                  (and (= :fixed grow-type) overflow-text (<= new-height shape-height))
                                  (assoc :overflow-text false)

                                  (and (not-changed? shape-width new-width) (= grow-type :auto-width))
                                  (-> (assoc :modifiers modifier-width)
                                      (gsh/transform-shape))

                                  (and (not-changed? shape-height new-height)
                                       (or (= grow-type :auto-height) (= grow-type :auto-width)))
                                  (-> (assoc :modifiers modifier-height)
                                      (gsh/transform-shape)))]
                      (assoc objects id shape))))

                undo-transaction (get-in state [:workspace-undo :transaction])
                objects2 (->> changes (reduce change-text-shape objects1))

                regchg   {:type :reg-objects
                          :page-id page-id
                          :shapes (vec (keys changes))}

                rchanges (dwc/generate-changes page-id objects1 objects2)
                uchanges (dwc/generate-changes page-id objects2 objects0)]

            (if (seq rchanges)
              (rx/concat
               (when-not undo-transaction
                 (rx/of (dwc/start-undo-transaction)))
               (rx/of (dwc/commit-changes (conj rchanges regchg) (conj uchanges regchg) {:commit-local? true}))
               (when-not undo-transaction
                 (rx/of (dwc/discard-undo-transaction)))))))))))

;; When a resize-event arrives we start "buffering" for a time
;; after that time we invoke `resize-text-batch` with all the changes
;; together. This improves the performance because we only re-render the
;; resized components once even if there are changes that applies to
;; lots of texts like changing a font
(defn resize-text
  [id new-width new-height]
  (ptk/reify ::resize-text
    IDeref
    (-deref [_]
      {:id id :width new-width :height new-height})

    ptk/WatchEvent
    (watch [_ state stream]
      (let [;; This stream aggregates the events of "resizing"
            resize-events
            (rx/merge
             (->> (rx/of (resize-text id new-width new-height)))
             (->> stream (rx/filter (ptk/type? ::resize-text))))

            ;; Stop buffering after time without resizes
            stop-buffer (->> resize-events (rx/debounce 100))

            ;; Agregates the resizes so only send the resize when the sizes are stable
            resize-batch
            (->> resize-events
                 (rx/take-until stop-buffer)
                 (rx/reduce (fn [acc event]
                              (assoc acc (:id @event) [(:width @event) (:height @event)]))
                            {id [new-width new-height]})
                 (rx/map #(resize-text-batch %)))

            ;; This stream retrieves the changes of page so we cancel the agregation
            change-page
            (->> stream
                 (rx/filter (ptk/type? :app.main.data.workspace/finalize-page))
                 (rx/take 1)
                 (rx/ignore))]

        (if-not (::handling-texts state)
          (->> (rx/concat
                (rx/of #(assoc % ::handling-texts true))
                (rx/race resize-batch change-page)
                (rx/of #(dissoc % ::handling-texts))))
          (rx/empty))))))
