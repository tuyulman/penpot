;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020-2021 UXBOX Labs SL

(ns app.main.data.workspace.texts
  (:require
   ["draft-js" :as draft]
   [app.common.math :as mth]
   [app.common.attrs :as attrs]
   [app.common.text :as txt]
   [app.common.geom.shapes :as gsh]
   [app.common.pages :as cp]
   [app.common.data :as d]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.common :as dwc]
   [app.main.data.workspace.transforms :as dwt]
   [app.main.fonts :as fonts]
   [app.util.object :as obj]
   [app.util.text-editor :as ted]
   [app.util.timers :as ts]
   [beicon.core :as rx]
   [cljs.spec.alpha :as s]
   [goog.object :as gobj]
   [cuerdas.core :as str]
   [potok.core :as ptk]))


;; TODO: abstract some draft internals

(defn update-editor
  [editor]
  (ptk/reify ::update-editor
    ptk/UpdateEvent
    (update [_ state]
      (if (some? editor)
        (assoc state :workspace-editor editor)
        (dissoc state :workspace-editor)))))

(defn focus-editor
  []
  (ptk/reify ::focus-editor
    ptk/EffectEvent
    (effect [_ state stream]
      (when-let [editor (:workspace-editor state)]
        (ts/schedule #(.focus ^js editor))))))

(defn update-editor-state
  [estate]
  (ptk/reify ::update-editor-state
    ptk/UpdateEvent
    (update [_ state]
      (if (some? estate)
        (assoc state :workspace-editor-state estate)
        (dissoc state :workspace-editor-state)))))

(defn initialize-editor-state
  [{:keys [id content] :as shape}]
  (ptk/reify ::initialize-editor-state
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-editor-state
              (fn [_]
                (if content
                  (->> content
                       (ted/penpot->draft)
                       (clj->js)
                       (draft/convertFromRaw)
                       (.createWithContent ^js draft/EditorState))
                  (.createEmpty ^js draft/EditorState)))))))

(defn finalize-editor-state
  [{:keys [id] :as shape}]
  (ptk/reify ::finalize-editor-state
    ptk/WatchEvent
    (watch [_ state stream]
      (let [estate  (:workspace-editor-state state)
            content (.getCurrentContent ^js estate)]
        (if ^boolean (.hasText ^js content)
          (let [content (-> content
                            (draft/convertToRaw)
                            (js->clj :keywordize-keys true)
                            (ted/draft->penpot))
                ]
            ;; (println "=============================")
            ;; (println "====== draft->penpot:")
            ;; (cljs.pprint/pprint (txt/draft->penpot content))
            ;; (println "====== original:")
            ;; (cljs.pprint/pprint content)
            ;; (println "====== penpot->draft:")
            ;; (cljs.pprint/pprint (txt/penpot->draft (txt/draft->penpot content)))

            (rx/merge
             (rx/of (update-editor-state nil))
             (when (not= content (:content shape))
               (rx/of (dwc/update-shapes [id] #(assoc % :content content))))))
          (rx/of (dws/deselect-shape id)
                 (dwc/delete-shapes [id])))))))

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
                  params    #js {:anchorKey fbk
                                 :anchorOffset 0
                                 :focusKey lbk
                                 :focusOffset lbl}
                  selection (draft/SelectionState. params)]
              (.forceSelection ^js draft/EditorState state selection)))]
    (ptk/reify ::editor-select-all
      ptk/UpdateEvent
      (update [_ state]
        (d/update-when state :workspace-editor-state select-all)))))

;; --- Helpers

(defn- get-editor-current-block
  [state]
  (let [content (.getCurrentContent ^js state)
        key     (.. ^js state getSelection getStartKey)]
    (.getBlockForKey ^js content key)))

(defn- shape-current-values
  [shape pred attrs]
  (let [root  (:content shape)
        nodes (->> (txt/nodes-seq pred root)
                   (map #(if (txt/is-text-node? %)
                           (merge txt/default-text-attrs %)
                           %)))]
    (attrs/get-attrs-multi nodes attrs)))

(defn current-paragraph-values
  [{:keys [editor-state attrs shape]}]
  (if editor-state
    (let [block (get-editor-current-block editor-state)]
      (-> (.getData ^js block)
          (ted/immutable-map->map)
          (select-keys attrs)))

    (shape-current-values shape txt/is-paragraph-node? attrs)))

(defn current-text-values
  [{:keys [editor-state attrs shape]}]
  (if editor-state
    (-> (.getCurrentInlineStyle ^js editor-state)
        (ted/styles-to-attrs)
        (select-keys attrs))
    (shape-current-values shape txt/is-text-node? attrs)))


;; --- TEXT EDITION IMPL

(defn update-root-attrs
  [{:keys [id attrs]}]
  (ptk/reify ::update-root-attrs
    ptk/WatchEvent
    (watch [_ state stream]
      (rx/of (dwc/update-shapes [id] #(attrs/merge % attrs))
             (focus-editor)))))

(defn update-paragraph-attrs
  [{:keys [id attrs]}]
  (letfn [(update-shape-blocks [shape]
            (let [merge-attrs #(attrs/merge % attrs)]
              (update shape :content #(txt/transform-nodes txt/is-paragraph-node? merge-attrs %))))

          (update-editor-current-block [state]
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
        (d/update-when state :workspace-editor-state update-editor-current-block))

      ptk/WatchEvent
      (watch [_ state stream]
        (if (:workspace-editor-state state)
          (rx/of focus-editor)
          (let [objects (dwc/lookup-page-objects state)
                shape   (get objects id)
                ids     (cond (= (:type shape) :text)  [id]
                              (= (:type shape) :group) (cp/get-children id objects))]
            (rx/of (dwc/update-shapes ids update-shape-blocks))))))))

(defn update-text-attrs
  [{:keys [id attrs]}]
  (letfn [(update-shape-inline-style [shape]
            (let [merge-attrs #(attrs/merge % attrs)]
              (update shape :content #(txt/transform-nodes txt/is-text-node? merge-attrs %))))

          (update-editor-inline-style [state]
            (let [selection (.getSelection ^js state)
                  content   (.getCurrentContent ^js state)
                  styles    (ted/attrs-to-styles attrs)]
              (reduce (fn [state style]
                        (let [modifier (.applyInlineStyle draft/Modifier
                                                          (.getCurrentContent ^js state)
                                                          selection
                                                          style)]
                          (.push draft/EditorState state modifier "change-inline-style")))
                      state
                      styles)))]

    (ptk/reify ::update-text-attrs
      ptk/UpdateEvent
      (update [_ state]
        (d/update-when state :workspace-editor-state update-editor-inline-style))

      ptk/WatchEvent
      (watch [_ state stream]
        (if (:workspace-editor-state state)
          (rx/of (focus-editor))
          (let [objects (dwc/lookup-page-objects state)
                shape   (get objects id)
                ids     (cond (= (:type shape) :text)  [id]
                              (= (:type shape) :group) (cp/get-children id objects))]
            (rx/of (dwc/update-shapes ids update-shape-inline-style))))))))


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
