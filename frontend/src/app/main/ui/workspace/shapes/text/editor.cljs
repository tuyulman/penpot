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
   ;; ["slate" :as slate]
   ;; ["slate-react" :as rslate]
   ["draft-js" :as draft]
   [okulary.core :as l]
   [goog.events :as events]
   [rumext.alpha :as mf]
   [app.common.data :as d]
   [app.common.geom.shapes :as gsh]
   [app.util.dom :as dom]
   [app.util.text :as ut]
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

;; (defn- initial-text
;;   [text]
;;   (clj->js
;;    [{:type "root"
;;      :children [{:type "paragraph-set"
;;                  :children [{:type "paragraph"
;;                              :children [{:fill-color "#000000"
;;                                          :fill-opacity 1
;;                                          :text (or text "")}]}]}]}]))
;; (defn- parse-content
;;   [content]
;;   (cond
;;     (string? content) (initial-text content)
;;     (map? content) (clj->js [content])
;;     :else (initial-text "")))

(defn- content-size
  [node]
  (let [current (count (:text node))
        children-count (->> node :children (map content-size) (reduce +))]
    (+ current children-count)))

(defn- fix-gradients
  "Fix for the gradient types that need to be keywords"
  [content]
  (let [fix-node
        (fn [node]
          (d/update-in-when node [:fill-color-gradient :type] keyword))]
    (ut/map-node fix-node content)))

;; --- Text Editor Rendering

;; (mf/defc editor-root-node
;;   {::mf/wrap-props false
;;    ::mf/wrap [mf/memo]}
;;   [props]
;;   (let [
;;         childs (obj/get props "children")
;;         data   (obj/get props "element")
;;         type   (obj/get data "type")
;;         style  (sts/generate-root-styles data props)
;;         attrs  (-> (obj/get props "attributes")
;;                    (obj/set! "style" style)
;;                    (obj/set! "className" type))]
;;     [:> :div attrs childs]))

;; (mf/defc editor-paragraph-set-node
;;   {::mf/wrap-props false}
;;   [props]
;;   (let [childs (obj/get props "children")
;;         data   (obj/get props "element")
;;         type   (obj/get data "type")
;;         shape  (obj/get props "shape")
;;         style  (sts/generate-paragraph-set-styles data props)
;;         attrs  (-> (obj/get props "attributes")
;;                    (obj/set! "style" style)
;;                    (obj/set! "className" type))]
;;     [:> :div attrs childs]))

;; (mf/defc editor-paragraph-node
;;   {::mf/wrap-props false}
;;   [props]
;;   (let [
;;         childs (obj/get props "children")
;;         data   (obj/get props "element")
;;         type   (obj/get data "type")
;;         style  (sts/generate-paragraph-styles data props)
;;         attrs  (-> (obj/get props "attributes")
;;                    (obj/set! "style" style)
;;                    (obj/set! "className" type))]
;;     [:> :p attrs childs]))

;; (mf/defc editor-text-node
;;   {::mf/wrap-props false}
;;   [props]
;;   (let [childs (obj/get props "children")
;;         data   (obj/get props "leaf")
;;         type   (obj/get data "type")
;;         style  (sts/generate-text-styles data props)
;;         attrs  (-> (obj/get props "attributes")
;;                    (obj/set! "style" style))
;;         gradient (obj/get data "fill-color-gradient" nil)]
;;     (if gradient
;;       (obj/set! attrs "className" (str type " gradient"))
;;       (obj/set! attrs "className" type))
;;     [:> :span attrs childs]))

;; (defn- render-element
;;   [shape props]
;;   (mf/html
;;    (let [element (obj/get props "element")
;;          type    (obj/get element "type")
;;          props   (obj/merge! props #js {:shape shape})
;;          props   (cond-> props
;;                    (= type "root") (obj/set! "key" "root")
;;                    (= type "paragraph-set") (obj/set! "key" "paragraph-set"))]

;;      (case type
;;        "root"          [:> editor-root-node props]
;;        "paragraph-set" [:> editor-paragraph-set-node props]
;;        "paragraph"     [:> editor-paragraph-node props]
;;        nil))))

;; (defn- render-text
;;   [props]
;;   (mf/html
;;    [:> editor-text-node props]))

(mf/defc block-component
  {::mf/wrap-props false}
  [props]
  (let [children (obj/get props "children")
        bprops   (obj/get props "blockProps")
        style    (sts/generate-paragraph-styles* (obj/get bprops "shape")
                                                 (obj/get bprops "data"))]
    [:div {:style style}
     [:> draft/EditorBlock props]]))

;; --- Text Shape Edit

(def empty-editor-state
  (.createEmpty ^js draft/EditorState))

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

(mf/defc text-shape-edit-html
  {::mf/wrap [mf/memo]
   ::mf/wrap-props false
   ::mf/forward-ref true}
  [props ref]
  (let [{:keys [id x y width height content2 grow-type] :as shape} (unchecked-get props "shape")

        zoom          (mf/deref refs/selected-zoom)
        state         (or (mf/deref refs/workspace-editor) empty-editor-state)

        ;; editor-ref    (mf/use-ref)
        self-ref      (mf/use-ref)
        selecting-ref (mf/use-ref)
        ;; measure-ref   (mf/use-ref)

        on-close
        (fn []
          (st/emit! dw/clear-edition-mode)

          #_(when (= 0 (content-size @content-var))
            (st/emit! (dws/deselect-shape id)
                      (dw/delete-shapes [id]))))

        on-click-outside
        (fn [event]
          (let [target     (dom/get-target event)
                options    (dom/get-element-by-class "element-options")
                assets     (dom/get-element-by-class "assets-bar")
                cpicker    (dom/get-element-by-class "colorpicker-tooltip")
                palette    (dom/get-element-by-class "color-palette")

                self       (mf/ref-val self-ref)
                selecting? (mf/ref-val selecting-ref)]
            (if (or (and options (.contains options target))
                    (and assets  (.contains assets target))
                    (and self    (.contains self target))
                    (and cpicker (.contains cpicker target))
                    (and palette (.contains palette target))
                    (= "foreignObject" (.-tagName ^js target)))
              (dom/stop-propagation event)
              (if selecting?
                (do
                  (mf/set-ref-val! selecting-ref false)
                  (dom/stop-propagation event))
                (on-close)))))

        on-mouse-down
        (fn [event]
          (mf/set-ref-val! selecting-ref true))

        on-mouse-up
        (fn [event]
          (mf/set-ref-val! selecting-ref false))

        on-key-up
        (fn [event]
          (dom/stop-propagation event)
          (when (= (.-keyCode event) 27) ; ESC
            (do
              (st/emit! :interrupt)
              (on-close))))

        on-mount
        (fn []
          (let [keys [(events/listen js/document EventType.MOUSEDOWN on-click-outside)
                      (events/listen js/document EventType.CLICK on-click-outside)
                      (events/listen js/document EventType.KEYUP on-key-up)]]
            (st/emit! (dwt/initialize-editor-state shape)
                      (dwt/editor-select-all))
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

    [:div.text-editor {:ref self-ref}
     [:style "span { line-height: inherit; }
              .gradient { background: var(--text-color); -webkit-text-fill-color: transparent; -webkit-background-clip: text;"]

     [:> draft/Editor
      {:on-change on-change
       :on-blur on-blur
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
