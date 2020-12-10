;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns app.common.pages.spec.common
  (:require
   [app.common.spec :as us]
   [clojure.spec.alpha :as s]))

;; --- Rect ---

(s/def ::rect
  (s/keys :req-un
          [:rect/x
           :rect/y
           :rect/width
           :rect/height]))

(s/def :rect/x ::us/safe-integer)
(s/def :rect/y ::us/safe-integer)
(s/def :rect/width ::us/safe-integer)
(s/def :rect/height ::us/safe-integer)

;; --- Selection Rect ---

(s/def ::selrect
  (s/and ::rect
         (s/keys :req-un
                 [:selrect/x1
                  :selrect/y1
                  :selrect/x2
                  :selrect/y2])))

(s/def :selrect/x1 ::us/safe-integer)
(s/def :selrect/y1 ::us/safe-integer)
(s/def :selrect/x2 ::us/safe-integer)
(s/def :selrect/y2 ::us/safe-integer)

;; --- Point ---

(s/def ::point
  (s/and (s/keys :req-un
                 [:point/x
                  :point/y])
         gpt/point?))

(s/def point/x ::us/safe-integer)
(s/def point/y ::us/safe-integer)

;; --- Matrix ---

(s/def ::matrix
  (s/and (s/keys :req-un
                 [:matrix/a
                  :matrix/b
                  :matrix/c
                  :matrix/d
                  :matrix/e
                  :matrix/f])
         gmt/matrix?))

(s/def :matrix/a ::us/safe-number)
(s/def :matrix/b ::us/safe-number)
(s/def :matrix/c ::us/safe-number)
(s/def :matrix/d ::us/safe-number)
(s/def :matrix/e ::us/safe-number)
(s/def :matrix/f ::us/safe-number)


;; -- Hex color ---

(s/def ::hex-color string?)


