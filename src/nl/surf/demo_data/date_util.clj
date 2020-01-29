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

(ns nl.surf.demo-data.date-util
  (:refer-clojure :exclude [get])
  (:require [clojure.string :as s])
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
  (doto (Calendar/getInstance)
    (.setTime (.parse (SimpleDateFormat. "yyyy-MM-dd") s))))

(defn calendar-field-from-string [name]
  (.get (.getDeclaredField Calendar (s/upper-case name)) nil))

(defn nth-weekday-of
  "Return `java.util.Calendar` instance of the `n`-th (starting at `0`)
  `weekday` of `month` in `year`."
  [n weekday month year]
  (loop [cal (doto (Calendar/getInstance) (.setTimeInMillis 0)
                   (.set year (calendar-field-from-string month) 1 0 0 0))]
    (if (= (calendar-field-from-string weekday)
           (.get cal Calendar/DAY_OF_WEEK))
      (doto cal (.add Calendar/DAY_OF_MONTH (* n 7)))
      (recur (doto cal (.add Calendar/DAY_OF_MONTH 1))))))

(defn last-day-of
  "Return `java.util.Calendar` instance of the last day of `month` in `year`."
  [month year]
  (doto (Calendar/getInstance) (.setTimeInMillis 0)
        (.set year (calendar-field-from-string month) 1 0 0 0)
        (.add Calendar/MONTH 1)
        (.add Calendar/DAY_OF_MONTH -1)))

(defn get
  "Get property of `java.util.Calendar`."
  [cal prop]
  (.get cal (calendar-field-from-string prop)))

(defn ->msecs-since-epoch
  [^Calendar cal]
  (.getTimeInMillis cal))

(defn <-msecs-since-epoch
  [^long msecs]
  (doto (Calendar/getInstance) (.setTimeInMillis msecs)))
