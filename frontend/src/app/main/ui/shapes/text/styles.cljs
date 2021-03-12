;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020-2021 UXBOX Labs SL

(ns app.main.ui.shapes.text.styles
  (:require
   [app.common.data :as d]
   [app.main.fonts :as fonts]
   [app.util.color :as uc]
   [app.util.object :as obj]
   [app.util.text :as txt]
   [cuerdas.core :as str]))

;; TODO: there are no paragraph-set inside editor, and we cant change
;; styles of analogous elements becase we have no control of it. Maybe
;; using css properties can solve this.
(defn generate-paragraph-set-styles*
  [{:keys [grow-type] :as shape}]
  ;; This element will control the auto-width/auto-height size for the
  ;; shape. The properties try to adjust to the shape and "overflow" if
  ;; the shape is not big enough.
  ;; We `inherit` the property `justify-content` so it's set by the root where
  ;; the property it's known.
  ;; `inline-flex` is similar to flex but `overflows` outside the bounds of the
  ;; parent
  (let [auto-width?  (= grow-type :auto-width)
        auto-height? (= grow-type :auto-height)]
    #js {:display "inline-flex"
         :flexDirection "column"
         :justifyContent "inherit"
         :minHeight (when-not (or auto-width? auto-height?) "100%")
         :minWidth (when-not auto-width? "100%")
         :verticalAlign "top"}))

(defn generate-paragraph-styles*
  [shape data]
  (let [data      (cond-> data (map? data) (clj->js))
        grow-type (:grow-type shape)
        base #js  {:fontSize "14px"
                   :margin "inherit"
                   :lineHeight "1.2"}
        lh        (obj/get data "line-height")
        ta        (obj/get data "text-align")]
    (cond-> base
      ta                        (obj/set! "textAlign" ta)
      lh                        (obj/set! "lineHeight" lh)
      (= grow-type :auto-width) (obj/set! "whiteSpace" "pre"))))

(defn generate-text-styles*
  [data]
  (let [data            (cond-> data (map? data) (clj->js))
        letter-spacing  (obj/get data "letter-spacing")
        text-decoration (obj/get data "text-decoration")
        text-transform  (obj/get data "text-transform")
        line-height     (obj/get data "line-height")

        font-id         (obj/get data "font-id" (:font-id txt/default-text-attrs))
        font-variant-id (obj/get data "font-variant-id")

        font-family     (obj/get data "font-family")
        font-size       (obj/get data "font-size")

        fill-color (obj/get data "fill-color")
        fill-opacity (obj/get data "fill-opacity" 1)
        fill-color-gradient (obj/get data "fill-color-gradient" nil)
        fill-color-gradient (when fill-color-gradient
                              (-> (js->clj fill-color-gradient :keywordize-keys true)
                                  (update :type keyword)))

        ;; Uncomment this to allow to remove text colors. This could break the texts that already exist
        ;;[r g b a] (if (nil? fill-color)
        ;;            [0 0 0 0] ;; Transparent color
        ;;            (uc/hex->rgba fill-color fill-opacity))

        [r g b a] (uc/hex->rgba fill-color fill-opacity)

        text-color (if fill-color-gradient
                     (uc/gradient->css (js->clj fill-color-gradient))
                     (str/format "rgba(%s, %s, %s, %s)" r g b a))

        fontsdb (deref fonts/fontsdb)

        base #js {:textDecoration text-decoration
                  :textTransform text-transform
                  :lineHeight (or line-height "inherit")
                  :color text-color
                  "--text-color" text-color}]

    (when (and (string? letter-spacing)
               (pos? (alength letter-spacing)))
      (obj/set! base "letterSpacing" (str letter-spacing "px")))

    (when (and (string? font-size)
               (pos? (alength font-size)))
      (obj/set! base "fontSize" (str font-size "px")))

    (when (and (string? font-id)
               (pos? (alength font-id)))
      (fonts/ensure-loaded! font-id)
      (let [font (get fontsdb font-id)]
        (let [font-family (or (:family font)
                              (obj/get data "fontFamily"))
              font-variant (d/seek #(= font-variant-id (:id %))
                                   (:variants font))
              font-style  (or (:style font-variant)
                              (obj/get data "fontStyle"))
              font-weight (or (:weight font-variant)
                              (obj/get data "fontWeight"))]
          (obj/set! base "fontFamily" font-family)
          (obj/set! base "fontStyle" font-style)
          (obj/set! base "fontWeight" font-weight))))


    base))
