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
              [] (clojure.string/split-lines (read-file path)))))
