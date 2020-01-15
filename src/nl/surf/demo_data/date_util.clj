(ns nl.surf.demo-data.date-util
  (:refer-clojure :exclude [get])
  (:import java.text.SimpleDateFormat
           java.util.Calendar))

(defn simple-format [^String format ^Calendar inst]
  (.format (doto (SimpleDateFormat. format) (.setCalendar inst)) (.getTime inst)))

(defn rfc3339
  "Format given `inst` to RFC 3339 defined string (only the date part)."
  [^Calendar inst]
  (simple-format "yyyy-MM-dd'T'HH:mm:ssXXX" inst))

(defn rfc3339-date
  "Format given `inst` to RFC 3339 defined string."
  [^Calendar inst]
  (simple-format "yyyy-MM-dd" inst))

(defn parse-date
  [^String s]
  (doto (java.util.Calendar/getInstance)
    (.setTime (.parse (SimpleDateFormat. "yyyy-MM-dd") s))))

(def january Calendar/JANUARY)
(def february Calendar/FEBRUARY)
(def march Calendar/MARCH)
(def april Calendar/APRIL)
(def may Calendar/MAY)
(def june Calendar/JUNE)
(def july Calendar/JULY)
(def august Calendar/AUGUST)
(def september Calendar/SEPTEMBER)
(def october Calendar/OCTOBER)
(def november Calendar/NOVEMBER)
(def december Calendar/DECEMBER)

(def sunday Calendar/SUNDAY)
(def monday Calendar/MONDAY)
(def tuesday Calendar/TUESDAY)
(def wednesday Calendar/WEDNESDAY)
(def thursday Calendar/THURSDAY)
(def friday Calendar/FRIDAY)
(def saturday Calendar/SATURDAY)

(defn nth-weekday-of
  "Return `java.util.Calendar` instance of the `n`-th (starting at `0`)
  `weekday` of `month` in `year`."
  [n weekday year month]
  (loop [cal (doto (Calendar/getInstance) (.setTimeInMillis 0)
                   (.set year month 1 0 0 0))]
    (if (= weekday
           (.get cal Calendar/DAY_OF_WEEK))
      (doto cal (.add Calendar/DAY_OF_MONTH (* n 7)))
      (recur (doto cal (.add Calendar/DAY_OF_MONTH 1))))))

(defn last-day-of
  "Return `java.util.Calendar` instance of the last day of `month` in `year`."
  [year month]
  (doto (Calendar/getInstance) (.setTimeInMillis 0)
        (.set year month 1 0 0 0)
        (.add Calendar/MONTH 1)
        (.add Calendar/DAY_OF_MONTH -1)))

(def month Calendar/MONTH)
(def year Calendar/YEAR)

(defn get
  "Get property of `java.util.Calendar`."
  [cal prop]
  (.get cal prop))

(defn ->msecs-since-epoch
  [^Calendar cal]
  (.getTimeInMillis cal))

(defn <-msecs-since-epoch
  [^long msecs]
  (doto (Calendar/getInstance) (.setTimeInMillis msecs)))
