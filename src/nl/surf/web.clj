(ns nl.surf.web
  (:require [nl.surf.ooapi :as ooapi]
            [nl.surf.export :as export]
            [nl.surf.world :as world]
            [clojure.string :as s]
            [clojure.data.generators :as dgen]
            [hiccup.core :as hiccup]
            [ring.middleware.params :refer [wrap-params]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.json :refer [wrap-json-response]]))

(def data
  (binding [dgen/*rnd* (java.util.Random. 42)]
    (export/export (world/gen ooapi/attributes {:service               1
                                                :institution           1
                                                :educational-programme 2
                                                :course-programme      8
                                                :course                5
                                                :lecturer              20
                                                :course-offering       10
                                                :person                15})
                   ooapi/export-conf)))

(def queries
  {"/courses" {"educationalProgramme" (fn [programme-id]
                                        (fn [course]
                                          (some #(= (str "/educational-programmes/" programme-id)
                                                    (:href %))
                                                (get-in course [:links :educationalProgrammes]))))}})

(declare render-map)
(declare render-coll)

(defn render [v]
  (cond
    (map? v)  (render-map v)
    (coll? v) (render-coll v)

    :else
    (pr-str v)))

(defn render-coll [data]
  [:ul
   (for [v data]
     [:li (render v)])])

(defn htmlify-link
  [url]
  (str url
       (if (re-find #"\?" url) \& \?)
       "html=1"))

(defn render-map [data]
  [:dl
   (for [[k v] data]
     [:div
      [:dt (pr-str k)]
      [:dd (if (= :href k)
             [:a {:href (htmlify-link v)} v]
             (render v))]])])

(defn render-html [data]
  (hiccup/html
   [:html
    [:head
     [:title "OOAPI"]
     [:meta {:charset "UTF-8"}]
     [:style "body > ul > li { border-top: 2px solid black }"]]
    [:body
     (render data)]]))

(defn app [{:keys [uri params] :as request}]
  (let [[_ root member] (re-find #"^(/.*?)(/.*)?$" uri)
        member          (when member (s/replace member #"^/" ""))
        filter-fn       (reduce (fn [m [k v]]
                                  (if-let [query (get-in queries [root k])]
                                    (query v)
                                    (let [k (keyword (str k "Id"))
                                          v (str v)]
                                      #(and (m %) (= v (str (get % k)))))))
                                (if member
                                  #(= (str (:id %)) member)
                                  identity)
                                params)
        data            (->> (get data root) (filter filter-fn))]
    {:status 200
     :body   data}))

(defn wrap-html-response
  "Middleware rendering response body as HTML if requested in the"
  [f]
  (fn [{:keys [params] :as request}]
    (if (get params "html")
      (-> request
          (update :params dissoc "html")
          (f)
          (update :body render-html))
      (f request))))

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
            (run-jetty (-> #'app
                           wrap-html-response
                           wrap-json-response
                           wrap-params)
                       {:host host, :port port, :join? false}))))

(defn -main [& _]
  (start!))
