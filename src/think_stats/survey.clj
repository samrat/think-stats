(ns think-stats.survey
  (:require [clojure.java.io :as io])
  (:import java.util.zip.GZIPInputStream))

(defn read-file
  "Return content of a gzipped file"
  [input]
  (with-open [input (-> input
                        io/input-stream
                        GZIPInputStream.)]
    (slurp input)))

(defn make-record
  "Args:
       line: line from a .dat file
       fields: vector of [name start end conversion]'s
   Returns:
       A hashmap"
  [line fields]
  (reduce (fn [m field]
            (let [[name start end conversion] field]
              (assoc m name (try (conversion (read-string
                                              (subs line start end)))
                                 (catch Exception e "NA")))))
          {} fields))

(defn respondents
  [path]
  (reduce (fn [v line]
            (conj v (make-record line [["caseid" 1 12 int]])))
          [] (clojure.string/split-lines (read-file path))))

(defn- recode-agepreg [pregnancies]
  (reduce (fn [v preg]
            (if (not= (get preg "agepreg") "NA")
              (conj v (update-in preg ["agepreg"] / 100))
              (conj v (assoc preg "agepreg" "NA"))))
          [] pregnancies))

(defn- recode-birthwgt-oz [pregnancies]
  (reduce (fn [v preg]
            (let [birthwgt_lb (get preg "birthwgt_lb")
                  birthwgt_oz (get preg "birthwgt_oz")]
              (if (and (not= birthwgt_lb "NA")
                       (< birthwgt_lb 20)
                       (not= birthwgt_oz "NA")
                       (<= birthwgt_oz 16))
                (conj v (assoc preg "totalwgt_oz"
                               (+ (* birthwgt_lb 16) birthwgt_oz)))
                (conj v (assoc preg "totalwgt_oz" "NA")))))
          [] pregnancies))

(defn pregnancies
  [path]
  (-> (reduce (fn [v line]
                (conj v (make-record line [["caseid" 1 12 int]
                                           ["nbrnaliv" 22 22 int]
                                           ["babysex" 55 56 int]
                                           ["birthwgt_lb" 57 58 int]
                                           ["birthwgt_oz" 59 60 int]
                                           ["prglength" 274 276 int]
                                           ["outcome" 276 277 int]
                                           ["birthord" 278 279 int]
                                           ["agepreg" 284 287 int]
                                           ["finalwgt" 423 440 float]])))
              [] (clojure.string/split-lines (read-file path)))
      (recode-agepreg)
      (recode-birthwgt-oz)
      ))
