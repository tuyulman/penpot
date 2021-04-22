;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.data.dashboard.fonts
  (:require
   ["opentype.js" :as ot]
   [app.common.exceptions :as ex]
   [app.common.data :as d]
   [app.common.pages :as cp]
   [app.common.spec :as us]
   [app.common.uuid :as uuid]
   [app.main.repo :as rp]
   [app.main.data.users :as du]
   [app.util.router :as rt]
   [app.util.time :as dt]
   [app.util.timers :as ts]
   [app.util.avatars :as avatars]
   [app.main.data.media :as di]
   [app.main.data.messages :as dm]
   [app.util.webapi :as wa]
   [app.util.object :as obj]
   [app.util.transit :as t]
   [beicon.core :as rx]
   [cljs.spec.alpha :as s]
   [cuerdas.core :as str]
   [potok.core :as ptk]))

(defn- parse-weight
  [variant]
  (cond
    (re-seq #"(?i)(?:hairline|thin)" variant)           100
    (re-seq #"(?i)(?:extra light|ultra light)" variant) 200
    (re-seq #"(?i)(?:light)" variant)                   300
    (re-seq #"(?i)(?:normal|regular)" variant)          400
    (re-seq #"(?i)(?:medium)" variant)                  500
    (re-seq #"(?i)(?:semi bold|demi bold)" variant)     600
    (re-seq #"(?i)(?:bold)" variant)                    700
    (re-seq #"(?i)(?:extra bold|ultra bold)" variant)   800
    (re-seq #"(?i)(?:black|heavy)" variant)             900
    (re-seq #"(?i)(?:extra black|ultra black)" variant) 950
    :else                                               400))

(defn- parse-style
  [variant]
  (if (re-seq #"(?i)(?:italic)" variant)
    "italic"
    "normal"))

(defn- parse-mtype
  [mtype]
  (case mtype
    "application/vnd.oasis.opendocument.formula-template" "font/otf"
    mtype))

(defn- parse-font
  [{:keys [data mtype] :as params}]
  (let [font    (ot/parse data)
        family  (or (.getEnglishName ^js font "preferredFamily")
                    (.getEnglishName ^js font "fontFamily"))
        variant (or (.getEnglishName ^js font "preferredSubfamily")
                    (.getEnglishName ^js font "fontSubfamily"))]
    {:data (js/Uint8Array. data)
     :mtype mtype
     :font-id (str "custom-" (str/slug family))
     :font-family family
     :font-weight (parse-weight variant)
     :font-style  (parse-style variant)}))

(defn prepare-fonts
  [blobs]
  (ptk/reify ::prepare-fonts
    ptk/EffectEvent
    (effect [_ state stream]
      (->> (rx/from blobs)
           (rx/mapcat (fn [blob]
                        (->> (wa/read-file-as-array-buffer blob)
                             (rx/map (fn [data]
                                       {:data data
                                        :mtype (parse-mtype (.-type blob))})))))
           (rx/map parse-font)
           (rx/transform (d/distinct-xf (juxt :font-family :font-weight :font-style)))
           (rx/subs (fn [font]
                      (prn (dissoc font :data))
                      ;; (js/console.log (t/encode font))
                      #_(let [data  (.toTables ^js font)
                            table (d/seek #(= "name" (obj/get % "tableName"))
                                          (seq (.-tables ^js data)))]
                        (js/console.log table)))
                    (fn [error]
                      (js/console.error "test" error)))))))
