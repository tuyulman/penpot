;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.data.workspace.path.streams
  (:require
   [app.main.data.workspace.path.helpers :as helpers]
   [app.main.data.workspace.path.state :as state]
   [app.common.geom.point :as gpt]
   [app.main.store :as st]
   [app.main.streams :as ms]
   [beicon.core :as rx]
   [potok.core :as ptk]
   [app.common.math :as mth]
   [app.main.snap :as snap]
   [okulary.core :as l]
   [app.util.geom.path :as ugp]))

(defonce drag-threshold 5)

(defn dragging? [start zoom]
  (fn [current]
    (>= (gpt/distance start current) (/ drag-threshold zoom))))

(defn drag-stream
  ([to-stream]
   (drag-stream to-stream (rx/empty)))

  ([to-stream not-drag-stream]
   (let [start @ms/mouse-position
         zoom  (get-in @st/state [:workspace-local :zoom] 1)
         mouse-up (->> st/stream (rx/filter #(ms/mouse-up? %)))

         position-stream
         (->> ms/mouse-position
              (rx/take-until mouse-up)
              (rx/filter (dragging? start zoom))
              (rx/take 1))]

     (rx/merge
      (->> position-stream
           (rx/if-empty ::empty)
           (rx/merge-map (fn [value]
                           (if (= value ::empty)
                             not-drag-stream
                             (rx/empty)))))
      
      (->> position-stream
           (rx/merge-map (fn [] to-stream)))))))

(defn to-dec [num]
  (let [k 50]
    (* (mth/floor (/ num k)) k)))

(defn move-points-stream
  [snap-toggled start-point selected-points points]

  (let [zoom (get-in @st/state [:workspace-local :zoom] 1)
        ranges (snap/create-ranges points selected-points)
        d-pos (/ snap/snap-path-accuracy zoom)

        check-path-snap
        (fn [position]
          (if snap-toggled
            (let [delta (gpt/subtract position start-point)
                  moved-points (->> selected-points (mapv #(gpt/add % delta)))
                  snap (snap/get-snap-delta moved-points ranges d-pos)]
              (gpt/add position snap))
            position))]
    (->> ms/mouse-position
         (rx/map check-path-snap))))

(defn move-handler-stream
  [snap-toggled start-point handler points]

  (let [zoom (get-in @st/state [:workspace-local :zoom] 1)
        ranges (snap/create-ranges points)
        d-pos (/ snap/snap-path-accuracy zoom)

        check-path-snap
        (fn [position]
          (if snap-toggled
            (let [delta (gpt/subtract position start-point)
                  handler-position (gpt/add handler delta)
                  snap (snap/get-snap-delta [handler-position] ranges d-pos)]
              (gpt/add position snap))
            position))]
    (->> ms/mouse-position
         (rx/map check-path-snap))))

(defn position-stream
  [snap-toggled points]
  (let [zoom (get-in @st/state [:workspace-local :zoom] 1)
        ;; ranges (snap/create-ranges points)
        d-pos (/ snap/snap-path-accuracy zoom)
        get-content (fn [state] (get-in state (state/get-path state :content)))

        content-stream
        (-> (l/derived get-content st/state)
            (rx/from-atom {:emit-current-value? true}))

        ranges-stream
        (->> content-stream
             (rx/map ugp/content->points)
             (rx/map snap/create-ranges))]

    (->> ms/mouse-position
         (rx/with-latest vector ranges-stream)
         (rx/map (fn [[position ranges]]
                   (if snap-toggled
                     (let [snap (snap/get-snap-delta [position] ranges d-pos)]
                       (gpt/add position snap))
                     position)))

         (rx/with-latest merge (->> ms/mouse-position-shift (rx/map #(hash-map :shift? %))))
         (rx/with-latest merge (->> ms/mouse-position-alt (rx/map #(hash-map :alt? %)))))))