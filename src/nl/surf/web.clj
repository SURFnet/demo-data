(ns nl.surf.web
  (:require [nl.surf.ooapi :as ooapi]
            [nl.surf.export :as export]
            [nl.surf.world :as world]
            [clojure.string :as s]
            [hiccup.core :as hiccup]
            [ring.middleware.params :refer [wrap-params]]
            [ring.adapter.jetty :refer [run-jetty]]))

(def data
  (export/export (world/gen ooapi/attributes {:service 1
                                              :institution 1
                                              :educational-programme 2
                                              :course 5
                                              :lecturer 20
                                              :course-offering 10
                                              :person 15})
                 ooapi/export-conf))

(declare render-map)
(declare render-coll)

(defn render [v]
  (cond
    (map? v) (render-map v)
    (coll? v) (render-coll v)

    :else
    (pr-str v)))

(defn render-coll [data]
  [:ul
   (for [v data]
     [:li (render v)])])

(defn render-map [data]
  [:dl
   (for [[k v] data]
     [:div
      [:dt (pr-str k)]
      [:dd (if (= :href k)
             [:a {:href v} v]
             (render v))]])])

(defn render-html [data]
  (hiccup/html
   [:html
    [:head
     [:title "OOAPI"]
     [:style "body > ul > li { border-top: 2px solid black }"]]
    [:body
     (render data)]]))

(defn app [{:keys [uri params] :as request}]
  (let [[_ root member] (re-find #"^(/.*?)(/.*)?$" uri)
        member          (when member (s/replace member #"^/" ""))
        filter-fn       (reduce (fn [m [k v]]
                                  (let [k (keyword (str k "Id"))
                                        v (str v)]
                                    #(and (m %) (= v (str (get % k))))))
                                (if member
                                  #(= (str (:id %)) member)
                                  identity)
                                params)
        data            (->> (get data root) (filter filter-fn))]
    {:status 200
     :body   (render-html data)}))

(defonce server-atom (atom nil))

(defn stop! []
  (when-let [server @server-atom]
    (.stop server)
    (reset! server-atom nil)))

(defn start! []
  (stop!)
  (let [host (get (System/getenv) "HOST")
        port (Integer/parseInt (get (System/getenv) "PORT" "8080"))]
    (reset! server-atom
            (run-jetty (wrap-params #'app)
                       {:host host, :port port, :join? false}))))

(defn -main [& _]
  (start!))
