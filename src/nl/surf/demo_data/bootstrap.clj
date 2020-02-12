;; Copyright (C) 2020 SURFnet B.V.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see http://www.gnu.org/licenses/.

(ns nl.surf.demo-data.bootstrap
  (:require [clojure.string :as string]))

(defmulti prop->attributes
  (fn [ctx {:strs [type] :as property}]
    type))

(defmethod prop->attributes :default
  [{:keys [property-name hidden] :as ctx} {:strs [type] :as prop}]
  [[property-name (cond-> {:generator type}
                    hidden
                    (assoc :hidden true))]])

(defmethod prop->attributes "integer"
  [{:keys [property-name hidden] :as ctx} {:strs [type] :as prop}]
  [[property-name (cond-> {:generator "int"}
                    hidden
                    (assoc :hidden true))]])

(defmethod prop->attributes "number"
  [{:keys [property-name hidden] :as ctx} {:strs [type] :as prop}]
  [[property-name (cond-> {:generator "float"}
                    hidden
                    (assoc :hidden true))]])

(defmethod prop->attributes "string"
  [{:keys [property-name hidden] :as ctx} {:strs [format] :as prop}]
  [[property-name (cond-> (if (= "uri" format)
                            {:value "http://example.com/"}
                            {:generator "string"})
                    hidden
                    (assoc :hidden true))]])

(defmethod prop->attributes "array"
  [{:keys [property-name hidden] :as ctx} {{:strs [type]} "items" :strs [example] :as prop}]
  [[property-name (cond-> {:value example}
                    hidden
                    (assoc :hidden true))]])

(defmethod prop->attributes "object"
  [{:keys [property-name schema-name hidden] :as ctx} {:strs [properties] :as prop}]
  (-> [[property-name (cond-> {:generator (into ["object"] (keys properties))
                               :deps      (mapv #(str schema-name "/__" property-name "__" %) (keys properties))}
                        hidden
                        (assoc :hidden true))]]
      (into (->> properties
                 (mapcat (fn [[sub-prop-name prop]]
                           (prop->attributes (assoc ctx
                                                    :property-name (str "__" property-name "__" sub-prop-name)
                                                    :hidden true)
                                             prop)))))))

(defn schema->type
  [schema-name {:strs [type properties required]}]
  (when-not (= "object" type)
    (throw (ex-info (str "Entity schema must be type `object`, was " (pr-str type)) {:type type})))
  (let [required? (set required)]
    {:name       schema-name
     :attributes (into {}
                       (mapcat
                        (fn [[property-name spec]]
                          (cond-> (prop->attributes {:schema-name schema-name :property-name property-name} spec)
                            (not (required? property-name))
                            (assoc-in [0 1 :optional] true)))
                        properties))}))

(defn clean-typename
  [path]
  (string/replace path #"\W" "_"))

(defn schema-for-path
  [spec path]
  (get-in spec ["paths" path "get" "responses" "200"
                "content" "application/hal+json" "schema"]))


(defn spec->types
  [spec]
  {:types (->> (get spec "paths")
               keys
               sort
               (mapv (fn [path]
                       (schema->type (clean-typename path)
                                     (schema-for-path spec path)))))})

