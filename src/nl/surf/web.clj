(ns nl.surf.web
  (:require [clojure.data.generators :as dgen]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.tools.logging :as log]
            [hiccup.core :as hiccup]
            [nl.surf.export :as export]
            [nl.surf.ooapi :as ooapi]
            [nl.surf.world :as world]
            [nl.zeekat.ring-openapi-validator :as validator]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.json :refer [wrap-json-response]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.stacktrace :refer [wrap-stacktrace]]
            [ring.util.response :as response]))

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
                                  (if (seq v)
                                    (if-let [query (get-in queries [root k])]
                                      (query v)
                                      (let [k (keyword (str k "Id"))
                                            v (str v)]
                                        #(and (m %) (= v (str (get % k))))))
                                    identity))
                                (if member
                                  #(= (str (:id %)) member)
                                  identity)
                                params)
        body            (get data root)
        body            (if (sequential? body)
                          (filter filter-fn body)
                          body)]
    {:status 200
     :body   body}))

(defn wrap-html-response
  "Middleware rendering response body as HTML if requested in the"
  [f]
  (fn [{:keys [params] :as request}]
    (if (get params "html")
      (-> request
          (update :params dissoc "html")
          (f)
          (update :body render-html)
          (response/content-type "text/html; charset=utf-8"))
      (-> request
          f
          ;; hal+json is required for ooapi
          (response/content-type "application/hal+json; charset=utf-8")))))

(defn wrap-validator
  [f validator]
  (fn [{:keys [params] :as request}]
    (let [response (f request)]
      (doseq [{:keys [level] :as m} (validator/validate-interaction validator (dissoc request :body) response)]
        (when-not (= :ignore level)
          (log/log level (prn-str m))))
      response)))

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
                           (wrap-validator (validator/openapi-validator "/public/ooapi.json" {:base-path (str "http://" host ":" port "/")}))
                           (wrap-stacktrace)
                           (wrap-resource "/public")
                           wrap-params)
                       {:host host, :port port, :join? false}))))

(defn -main [& _]
  (start!))

(defn restart!
  []
  (stop!)
  (start!))
