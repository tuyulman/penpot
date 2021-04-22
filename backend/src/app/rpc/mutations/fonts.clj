;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.rpc.mutations.fonts
  (:require
   [app.common.exceptions :as ex]
   [app.common.spec :as us]
   [app.db :as db]
   [app.media :as media]
   [app.rpc.queries.teams :as teams]
   [app.util.services :as sv]
   [app.util.time :as dt]
   [cuerdas.core :as str]
   [clojure.spec.alpha :as s]))

(declare create-font-variant)

(def valid-weight #{200 300 400 500 600 700 800})
(def valid-style #{"regular" "italic"})

(s/def ::profile-id ::us/uuid)
(s/def ::team-id ::us/uuid)
(s/def ::name ::us/not-empty-string)
(s/def ::weight valid-weight)
(s/def ::style valid-style)
(s/def ::font-id (s/and ::us/string #(str/starts-with? % "user:")))
(s/def ::content-type ::media/font-content-type)

(s/def ::content
  (s/and ::media/upload
         (s/keys :req-un [::content-type])))

(s/def ::create-font-variant
  (s/keys :req-un [::profile-id ::team-id ::font-id ::name ::weight ::style ::content]))

(sv/defmethod ::create-font-variant
  [{:keys [pool] :as cfg} {:keys [team-id profile-id] :as params}]
  (db/with-atomic [conn pool]
    (teams/check-edition-permissions! conn profile-id team-id)
    (create-font-variant conn params)))

(defn create-font-variant
  [conn {:keys [team-id font-id name weight style]}])
