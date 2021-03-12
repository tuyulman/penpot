;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns app.main.ui.workspace.shapes.text.editor
  (:require
   ["draft-js" :as draft]
   ["immutable" :as imm]
   [okulary.core :as l]
   [cuerdas.core :as str]
   [goog.events :as events]
   [rumext.alpha :as mf]
   [app.common.data :as d]
   [app.common.geom.shapes :as gsh]
   [app.util.dom :as dom]
   [app.util.text :as txt]
   [app.util.object :as obj]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.common :as dwc]
   [app.main.data.workspace.texts :as dwt]
   [app.main.data.workspace.selection :as dws]
   [app.main.ui.cursors :as cur]
   [app.main.ui.shapes.text.styles :as sts])
  (:import
   goog.events.EventType
   goog.events.KeyCodes))

;; --- Data functions

;; (defn- content-size
;;   [node]
;;   (let [current (count (:text node))
;;         children-count (->> node :children (map content-size) (reduce +))]
;;     (+ current children-count)))

;; (defn- fix-gradients
;;   "Fix for the gradient types that need to be keywords"
;;   [content]
;;   (let [fix-node
;;         (fn [node]
;;           (d/update-in-when node [:fill-color-gradient :type] keyword))]
;;     (txt/map-node fix-node content)))

;; --- Text Editor Rendering

(mf/defc block-component
  {::mf/wrap-props false}
  [props]
  (let [children (obj/get props "children")
        bprops   (obj/get props "blockProps")
        style    (sts/generate-paragraph-styles* (obj/get bprops "shape")
                                                 (obj/get bprops "data"))]
    [:div {:style style}
     [:> draft/EditorBlock props]]))

(defn render-block
  [block shape]
  (let [type (.getType ^js block)
        data (.getData ^js block)]
    ;; (js/console.log "render-block" "data" data)
    (case type
      "unstyled"
      #js {:editable true
           :component block-component
           :props #js {:data (.toJS data)
                       :shape shape}}
      nil)))

(def empty-editor-state
  (.createEmpty ^js draft/EditorState))

(mf/defc text-shape-edit-html
  {::mf/wrap [mf/memo]
   ::mf/wrap-props false
   ::mf/forward-ref true}
  [props ref]
  (let [{:keys [id x y width height content2 grow-type] :as shape} (unchecked-get props "shape")

        zoom          (mf/deref refs/selected-zoom)
        state         (or (mf/deref refs/workspace-editor-state) empty-editor-state)

        self-ref      (mf/use-ref)

        ;; on-close
        ;; (fn []
        ;;   (st/emit! dw/clear-edition-mode)

        ;;   #_(when (= 0 (content-size @content-var))
        ;;     (st/emit! (dws/deselect-shape id)
        ;;               (dw/delete-shapes [id]))))

        on-click-outside
        (fn [event]
          (let [target     (dom/get-target event)
                options    (dom/get-element-by-class "element-options")
                assets     (dom/get-element-by-class "assets-bar")
                cpicker    (dom/get-element-by-class "colorpicker-tooltip")
                palette    (dom/get-element-by-class "color-palette")

                self       (mf/ref-val self-ref)]
            (if (or (and options (.contains options target))
                    (and assets  (.contains assets target))
                    (and self    (.contains self target))
                    (and cpicker (.contains cpicker target))
                    (and palette (.contains palette target))
                    (= "foreignObject" (.-tagName ^js target)))
              (dom/stop-propagation event)
              (st/emit! dw/clear-edition-mode))))

        on-key-up
        (fn [event]
          (dom/stop-propagation event)
          (when (= (.-keyCode event) 27) ; ESC
            (do
              (st/emit! :interrupt)
              (st/emit! dw/clear-edition-mode))))

        on-mount
        (fn []
          (let [keys [(events/listen js/document EventType.MOUSEDOWN on-click-outside)
                      (events/listen js/document EventType.CLICK on-click-outside)
                      (events/listen js/document EventType.KEYUP on-key-up)]]
            (st/emit! (dwt/initialize-editor-state shape)
                      (dwt/select-all))
            #(do
               (st/emit! (dwt/finalize-editor-state shape))
               (doseq [key keys]
                 (events/unlistenByKey key)))))

        on-blur
        (fn [event]
          (dom/stop-propagation event)
          (dom/prevent-default event))

        on-change
        (mf/use-callback
         (fn [val]
           (st/emit! (dwt/update-editor-state val))))

        on-editor
        (mf/use-callback
         (fn [editor]
           ;; (mf/set-ref-val! editor-ref editor)
           (when editor
             (.focus ^js editor))))
        ]

    (mf/use-layout-effect on-mount)

    [:div.text-editor {:ref self-ref
                       :class (dom/classnames
                               :align-top (= (:vertical-align shape "top") "top")
                               :align-center (= (:vertical-align shape) "center")
                               :align-bottom (= (:vertical-align shape) "bottom"))}
     [:style
      "span { line-height: inherit; }
      .gradient { background: var(--text-color); -webkit-text-fill-color: transparent; -webkit-background-clip: text;"]

     [:> draft/Editor
      {:on-change on-change
       :on-blur on-blur
       :on-select (fn [event]
                    (js/console.log event))
       :custom-style-fn (fn [styles block]
                          (-> (txt/styles-to-attrs styles)
                              (sts/generate-text-styles*)))
       :block-renderer-fn #(render-block % shape)
       :ref on-editor
       :editor-state state}]]))

(mf/defc text-shape-edit
  {::mf/wrap [mf/memo]
   ::mf/wrap-props false
   ::mf/forward-ref true}
  [props ref]
  (let [shape (unchecked-get props "shape")
        {:keys [x y width height grow-type]} shape]
    [:foreignObject {:transform (gsh/transform-matrix shape)
                     :x x :y y
                     :width  (if (#{:auto-width} grow-type) 100000 width)
                     :height (if (#{:auto-height :auto-width} grow-type) 100000 height)}

     [:& text-shape-edit-html {:shape shape}]]))
