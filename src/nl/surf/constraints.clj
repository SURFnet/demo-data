(ns nl.surf.constraints)

(defn unique
  "Constraint to check that generated values don't already occur in
  world."
  [{:keys [world attr]} val]
  (let [entity-type (-> attr :name namespace keyword)]
    (not-any? #(= % val) (map (:name attr) (get world entity-type)))))
