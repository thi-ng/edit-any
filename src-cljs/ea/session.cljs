(ns ea.session)

(defn update-session-user
  [db {:keys [id title role] :as body}]
  (assoc-in db [:session :user]
            {:username id
             :name title
             :role role}))
