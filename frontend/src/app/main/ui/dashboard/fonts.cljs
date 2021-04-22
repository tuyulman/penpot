;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.dashboard.fonts
  (:require
   ["opentype.js" :as ot]
   [app.common.media :as cm]
   [app.main.data.dashboard :as dd]
   [app.main.data.dashboard.fonts :as df]
   [app.main.data.modal :as modal]
   [app.main.ui.components.file-uploader :refer [file-uploader]]
   [app.main.store :as st]
   [app.main.ui.icons :as i]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [app.util.router :as rt]
   [okulary.core :as l]
   [rumext.alpha :as mf]))

(defn- use-set-page-title
  [team section]
  (mf/use-effect
   (mf/deps team)
   (fn []
     (when team
       (let [tname (if (:is-default team)
                     (tr "dashboard.your-penpot")
                     (:name team))]
         (case section
           :fonts (dom/set-html-title (tr "title.dashboard.fonts" tname))
           :providers (dom/set-html-title (tr "title.dashboard.font-providers" tname))))))))

(mf/defc header
  {::mf/wrap [mf/memo]}
  [{:keys [section team] :as props}]
  (let [go-fonts
        (mf/use-callback
         (mf/deps team)
         (st/emitf (rt/nav :dashboard-fonts {:team-id (:id team)})))

        go-providers
        (mf/use-callback
         (mf/deps team)
         (st/emitf (rt/nav :dashboard-font-providers {:team-id (:id team)})))]

    (use-set-page-title team section)

    [:header.dashboard-header
     [:div.dashboard-title
      [:h1 (tr "labels.fonts")]]
     [:nav
      [:ul
       [:li {:class (when (= section :fonts) "active")}
        [:a {:on-click go-fonts} (tr "labels.custom-fonts")]]
       [:li {:class (when (= section :providers) "active")}
        [:a {:on-click go-providers} (tr "labels.font-providers")]]]]

     [:div]]))


(mf/defc upload-fonts-button
  [props]
  (let [input-ref (mf/use-ref)

        on-click
        (mf/use-callback #(dom/click (mf/ref-val input-ref)))

        on-selected
        (mf/use-callback
         (fn [blobs]
           (st/emit! (df/prepare-fonts blobs))))]

    [:div.btn-primary
     {:on-click on-click}
     [:span "Add custom font"]
     [:& file-uploader {:input-id "font-upload"
                        :accept cm/str-font-types
                        :multi true
                        :input-ref input-ref
                        :on-selected on-selected}]]))

(mf/defc fonts-hero-section
  []
  [:div.dashboard-fonts-hero
   [:div.desc
    [:h2 (tr "labels.upload-custom-fonts")]
    [:p (tr "dashboard.fonts.hero-text1")]
    [:p (tr "dashboard.fonts.hero-text2")]]
   [:& upload-fonts-button]])

(mf/defc font-item
  [{:keys [item] :as props}]
  [:div.table-row {:key item}
   [:div.table-field.font-family
    [:input {:type "text" :value "Lato"}]]
   [:div.table-field.font-weight "300"]
   [:div.table-field.font-style "normal"]
   [:div.table-field.options
    [:button.btn-primary "Upload"]
    [:span.icon i/close]]])

(mf/defc fonts-table-section
  []
  [:div.dashboard-fonts-table
   [:div.table-header
    [:div.table-field.font-family "Font Family"]
    [:div.table-field.font-weight "Font Weight"]
    [:div.table-field.font-style "Font Style"]
    [:div.table-field.search-input
     [:input {:placeholder "Search font"}]]]
   [:div.table-rows
    (for [i (range 4)]
      [:& font-item {:item i}])]])

(mf/defc fonts-page
  [{:keys [team] :as props}]
  [:*
   [:& header {:team team :section :fonts}]
   [:section.dashboard-container.dashboard-fonts
    [:& fonts-hero-section]
    [:& fonts-table-section]]])

(mf/defc font-providers-page
  [{:keys [team] :as props}]
  [:*
   [:& header {:team team :section :providers}]
   [:section.dashboard-container
    [:span "hello world font providers"]]])
