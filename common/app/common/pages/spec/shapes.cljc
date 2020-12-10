;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns app.common.pages.spec.shapes
  (:require
   [app.common.spec :as us]
   [app.common.pages.common :as pc]
   [app.common.pages.spec.common :as sc]
   [clojure.spec.alpha :as s]))

;; --- BASIC TYPES AND ALIASES ---

(s/def ::selrect     ::sc/selrect)
(s/def ::matrix      ::sc/matrix)
(s/def ::id          ::us/id)
(s/def ::number      ::us/safe-number)
(s/def ::pos-num     (s/and ::number #(>= % 0)))
(s/def ::gtz-num     (s/and ::number #(> % 0))) ;; gtz = greater than zero
(s/def ::int         ::us/safe-integer)
(s/def ::pos-int     (s/and ::us/safe-integer #(>= % 0)))
(s/def ::gtz-int     (s/and ::us/safe-integer #(> % 0))) ;; gtz = greather than zero
(s/def ::points      (s/coll-of ::sc/points :kind vector?))
(s/def ::string      string?)
(s/def ::bool        boolean?)
(s/def ::unit        (s/and ::number #(>= % 0) #(<= % 1)))
(s/def ::hex-color   (::sc/hex-color))

(defn root? [id] (= id pc/root))

;; --- SHAPE MAIN SPEC ---

(declare shape)

(s/def ::shape shape)

(defmulti shape
  (fn [:keys [{id type}]]
    (if (root? id) :root type)))

(defmethod shape :root [_]
  (s/and ::shape-basic))

(defmethod shape :frame [_]
  (s/and ::shape-basic
         ::shape-common
         ::shape-measures
         ::shape-fill
         ::shape-children
         ::shape-grids))

(defmethod shape :group [_]
  (s/and ::shape-basic
         ::shape-common
         ::shape-children
         ::shape-mask
         ::shape-components
         ::shape-exports
         ::shape-shadow
         ::shape-blur))

(defmethod shape :path [_]
  (s/and ::shape-basic
         ::shape-common
         ::shape-fill
         ::shape-stroke
         ::shape-path
         ::shape-exports
         ::shape-shadow
         ::shape-blur))

(defmethod shape :text [_]
  (s/and ::shape-basic
         ::shape-common
         ::shape-fill
         ::shape-measures
         ::shape-stroke
         ::shape-exports
         ::shape-shadow
         ::shape-blur))

(defmethod shape :image [_]
  (s/and ::shape-basic
         ::shape-common
         ::shape-measures
         ::shape-metadata
         ::shape-exports
         ::shape-shadow
         ::shape-blur))

(defmethod shape :circle [_]
  (s/and ::shape-basic
         ::shape-common
         ::shape-fill
         ::shape-measures
         ::shape-stroke
         ::shape-exports
         ::shape-shadow
         ::shape-blur))

(defmethod shape :rect [_]
  (s/and ::shape-basic
         ::shape-common
         ::shape-fill
         ::shape-radius
         ::shape-measures
         ::shape-stroke
         ::shape-exports
         ::shape-shadow
         ::shape-blur))

;; --- BASIC DATA ---

(s/def ::shape-basic
  (s/keys :req-un
          [:shape.basic/id
           :shape.basic/type]))

(s/def :shape.basic/id   ::id)
(s/def :shape.basic/type #{:frame :group :rect :circle :image :path :text})

;; --- COMMON DATA ---

(s/def ::shape-common
  (s/keys :req-un
          [:shape.common/name
           :shape.common/selrect
           :shape.common/points]

          :opt-un
          [:shape.common/parent-id
           :shape.common/frame-id
           :shape.common/blocked
           :shape.common/collapsed
           :shape.common/hidden
           :shape.common/proportion
           :shape.common/proportion-lock
           :shape.common/exports
           :shape.common/transform
           :shape.common/transform-inverse
           :shape.common/interactions]))

(s/def :shape.common/name              ::string)
(s/def :shape.common/selrect           ::selrect)
(s/def :shape.common/points            ::points)
(s/def :shape.common/blocked           ::bool)
(s/def :shape.common/collapsed         ::bool)
(s/def :shape.common/hidden            ::bool)
(s/def :shape.common/proportion        ::number)
(s/def :shape.common/proportion-lock   ::bool)
(s/def :shape.common/exports           ::exports)
(s/def :shape.common/transform         ::matrix)
(s/def :shape.common/transform-inverse ::matrix)
(s/def :shape.common/interactions      ::interactions)

;; --- PATH ---

(s/def ::shape-path
  (s/keys :opt-un
          [:shape.path/content]))

(s/def :shape.path/content ::path-content)

;; --- FILL ---

(s/def ::shape-fill
  (s/keys :opt-un
          [:shape.fill/fill-color
           :shape.fill/fill-opacity
           :shape.fill/fill-color-gradient
           :shape.fill/fill-color-ref-file
           :shape.fill/fill-color-ref-id]))

(s/def :shape.fill/fill-color          ::hex-color)
(s/def :shape.fill/fill-opacity        ::unit)
(s/def :shape.fill/fill-color-gradient ::gradient)
(s/def :shape.fill/fill-color-ref-file ::id)
(s/def :shape.fill/fill-color-ref-id   ::id)

;; --- TEXT ---

(s/def ::shape-text
  (s/keys :opt-un
          [:shape.text/content
           :shape.text/grow-type]))

(s/def :shape.text/content   ::text-content)
(s/def :shape.text/grow-type #{:fixed :auto-width :auto-height})


(s/def ::shape-typography
  (s/keys :opt-un
          [:shape.typography/typography-ref-id
           :shape.typography/typography-ref-file]))

(s/def :shape.typography/typography-ref-id   ::id)
(s/def :shape.typography/typography-ref-file (s/nilable ::id))

(s/def ::shape-font
  (s/keys :opt-un
          [:shape.font/font-family
           :shape.font/font-id
           :shape.font/font-size
           :shape.font/font-style
           :shape.font/font-variant-id
           :shape.font/font-weight
           :shape.font/letter-spacing
           :shape.font/line-height
           :shape.font/text-align
           :shape.font/text-decoration
           :shape.font/text-transform]))

(s/def :shape.font/font-family     ::string)
(s/def :shape.font/font-id         ::string)
(s/def :shape.font/font-size       ::pos-int)
(s/def :shape.font/font-style      ::string)
(s/def :shape.font/font-variant-id ::string)
(s/def :shape.font/font-weight     ::string)
(s/def :shape.font/letter-spacing  ::string)
(s/def :shape.font/line-height     ::string)
(s/def :shape.font/text-align      ::string)
(s/def :shape.font/text-decoration ::string)
(s/def :shape.font/text-transform  ::string)

(s/def ::shape-radius
  (s/keys :opt-un
          [:shape.radius/rx
           :shape.radius/ry]))

(s/def :shape.radius/rx ::number)
(s/def :shape.radius/ry ::number)

(s/def ::shape-measures
  (s/keys :opt-un
          [:shape.mesures/x
           :shape.mesures/y
           :shape.mesures/width
           :shape.mesures/height]))

(s/def :shape.mesures/x      ::number)
(s/def :shape.mesures/y      ::number)
(s/def :shape.mesures/width  ::gtz-num)
(s/def :shape.mesures/height ::gtz-num)

(s/def ::shape-image
  (s/keys :opt-un
          [:shape.image/metadata]))

(s/def :shape.image/metadata  ::image-metadata)

(s/def ::shape-mask
  (s/keys :opt-un
          [:shape.mask/masked-group?]))

(s/def :shape.mask/masked-group? ::bool)

(s/def ::shape-children
  (s/keys :opt-un
          [:shape.children/shapes]))

(s/def :shape.children/shapes (s/coll-of ::id :kind vector?))

(s/def ::shape-stroke
  (s/keys :opt-un
          [:shape.stroke/stroke-color
           :shape.stroke/stroke-color-ref-file
           :shape.stroke/stroke-color-ref-id
           :shape.stroke/stroke-opacity
           :shape.stroke/stroke-style
           :shape.stroke/stroke-width
           :shape.stroke/stroke-alignment]))

(s/def :shape.stroke/stroke-color          ::hex-color)
(s/def :shape.stroke/stroke-color-ref-file (s/nilable ::id))
(s/def :shape.stroke/stroke-color-ref-id   ::id)
(s/def :shape.stroke/stroke-opacity        ::unit)
(s/def :shape.stroke/stroke-style          #{:solid :dotted :dashed :mixed :none})
(s/def :shape.stroke/stroke-width          ::pos-int)
(s/def :shape.stroke/stroke-alignment      #{"left" "right" "center" "justify"})

(s/def ::shape-exports
  (s/keys :opt-un
          [:shape.exports/exports]))

(s/def :shape.exports/exports ::exports)

(s/def ::shape-shadow
  (s/keys :opt-un
          [:shape.shadow/shadow]))

(s/def :shape.shadow/shadow ::shadows)


(s/def ::shape-blur
  (s/keys :opt-un
          [:shape.blur/blur]))

(s/def :shape.blur/blur ::blur)

(s/def ::shape-grids
  (s/keys :opt-un
          [:shape.grid/grids]))

(s/def :shape.grid/grid ::grids)

(s/def ::shape-component
  (s/keys :opt-un
          [:shape.component/component-id
           :shape.component/component-file
           :shape.component/component-root?
           :shape.component/shape-ref]))

(s/def :shape.component/component-id    ::id)
(s/def :shape.component/component-file  ::id)
(s/def :shape.component/component-root? ::bool)
(s/def :shape.component/shape-ref       ::id)

;; =============================================== ;;
;;     COMPLEX TYPES
;; =============================================== ;;

;; --- COLORS ---

(s/def ::color
  (s/keys :opt-un
          [::id
           :color/name
           :color/value
           :color/color
           :color/opacity
           :color/gradient]))

(s/def :color/name     ::string)
(s/def :color/value    (s/nilable ::string))
(s/def :color/color    (s/nilable ::string))
(s/def :color/opacity  (s/nilable ::number))
(s/def :color/gradient (s/nilable ::gradient))

;; --- SHADOWS ---

(s/def ::shadow
  (s/coll-of :shadow/shadow :kind vector?))

(s/def :shadow/shadow
  (s/keys :req-un
          [:shadow/id
           :shadow/style
           :shadow/color
           :shadow/offset-x
           :shadow/offset-y
           :shadow/blur
           :shadow/spread
           :shadow/hidden]))

(s/def :shadow/id       ::id)
(s/def :shadow/style    #{:drop-shadow :inner-shadow})
(s/def :shadow/color    ::color)
(s/def :shadow/offset-x ::number)
(s/def :shadow/offset-y ::number)
(s/def :shadow/blur     ::number)
(s/def :shadow/spread   ::number)
(s/def :shadow/hidden   ::bool)

;; --- BLUR ---

(s/def ::blur
  (s/keys :req-un
          [:blur/id
           :blur/type
           :blur/value
           :blur/hidden]))

(s/def :blur/id     ::id)
(s/def :blur/type   #{:layer-blur})
(s/def :blur/value  ::number)
(s/def :blur/hidden ::bool)


;; --- GRADIENTS ---

(s/def ::gradient
  (s/keys :req-un
          [:gradient/type
           :gradient/start-x
           :gradient/start-y
           :gradient/end-x
           :gradient/end-y
           :gradient/width
           :gradient/stops]))

(s/def :gradient/type    #{:linear :radial})
(s/def :gradient/start-x ::number)
(s/def :gradient/start-y ::number)
(s/def :gradient/end-x   ::number)
(s/def :gradient/end-y   ::number)
(s/def :gradient/width   ::gtz-num)

(s/def :gradient/stops
  (s/coll-of :gradient/stop :kind vector?))

(s/def :gradient/stop
  (s/keys :req-un
          [:gradient.stop/color
           :gradient.stop/opacity
           :gradient.stop/offset]))

(s/def :gradient.stop/color   ::string)
(s/def :gradient.stop/opacity ::unit)
(s/def :gradient.stop/offset  ::unit)

;; --- EXPORT ---

(s/def ::exports
  (s/coll-of :export/export :kind vector?))

(s/def :export/export
  (s/keys :req-un [:export/type
                   :export/suffix
                   :export/scale]))

(s/def :export/type   ::string)
(s/def :export/suffix ::string)
(s/def :export/scale  ::number)

;; --- GRID ---

(s/def ::grids
  (s/coll-of :grid/grid :kind vector?))

(s/def :grid/grid
  (s/keys :req-un [:grid/display
                   :grid/params
                   :grid/type]))

(s/def :grid/type #{:square :column :row})

(s/def :grid/params
  (s/or :square :grid.params/square
        :column :grid.params/column
        :row    :grid.params/row))

(s/def :grid.params/square
  (s/keys :req-un [:grid.params/size
                   :grid.params/color]))

(s/def :grid.params/column
  (s/keys :req-un [:grid.params/size
                   :grid.params/color
                   :grid.params/type
                   :grid.params/item-length
                   :grid.params/gutter
                   :grid.params/margin]))

(s/def :grid.params/row :grid.params/column)

(s/def :grid.params/size        ::pos-int)
(s/def :grid.params/color       ::color)
(s/def :grid.params/type        #{:square :row :column})
(s/def :grid.params/item-length ::pos-int)
(s/def :grid.params/gutter      ::pos-int)
(s/def :grid.params/margin      ::pos-int)

;; --- INTERACTIONS ---

(s/def ::interactions
  (s/coll-of :interaction/interaction :kind vector?))

(s/def :interaction/interaction
  (s/keys :req-un [:interaction/event-type
                   :interaction/action-type
                   :interaction/destination]))

(s/def :interaction/event-type  #{:click}) ; In the future we will have more options
(s/def :interaction/action-type #{:navigate})
(s/def :interaction/destination ::id)


;; --- IMAGE METADATA ---

(s/def ::image-metadata
  (s/keys :opt-un
          [:image-metadata/width
           :image-metadata/height
           :image-metadata/path
           :image-metadata/id]))

(s/def :image-metadata/width  ::gtz-num)
(s/def :image-metadata/height ::gtz-num)
(s/def :image-metadata/path   ::string)
(s/def :image-metadata/id     ::id)

;; -- PATH CONTENT ---

(s/def ::path-content
  (s/coll-of :path/entry :kind vector?))

(s/def :path/entry
  (s/keys :req-un [:path/command]
          :req-opt [:path/params
                    :path/relative?]))

(s/def :path/command
  #{:move-to
    :line-to
    :line-to-horizontal
    :line-to-vertical
    :curve-to
    :smooth-curve-to
    :quadratic-bezier-curve-to
    :smooth-quadratic-bezier-curve-to
    :elliptical-arc
    :close-path})

(s/def :path/relative? ::bool)
(s/def :path/params
  (s/keys :req-un
          [:path.params/x
           :path.params/y]
          :opt-un
          [:path.params/c1x
           :path.params/c1y
           :path.params/c2x
           :path.params/c2y]))

(s/def :paths.params/x   ::number)
(s/def :paths.params/y   ::number)
(s/def :paths.params/c1x ::number)
(s/def :paths.params/c1y ::number)
(s/def :paths.params/c2x ::number)
(s/def :paths.params/c2y ::number)

;; --- TEXT CONTENT ---

(s/def ::text-content :text/node)

(s/def :text/node
  (s/or (s/keys :req-un [:text/children
                         :text/type])
        (s/and ::shape-typography
               ::shape-font
               (s/keys :req-un [:text/text]))))

(s/def :text/children (s/coll-of :text/node :kind vector?))
(s/def :text/type      #{"root" "paragraph" "paragraph-set" :root :paragraph :paragraph-set})
(s/def :text/text      ::string)
