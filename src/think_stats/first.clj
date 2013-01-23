(ns think-stats.first
  (:use think-stats.survey))

(defn count-total-live-births [path]
  (count (filter #(= (get % "outcome") 1)
                 (pregnancies path))))

(defn count-live-births-by-birthord
  [path]
  {:first-baby (count (filter #(and (= (get % "outcome") 1)
                                    (= (get % "birthord") 1))
                              (pregnancies path)))
   :not-first-baby (count (filter #(and (= (get % "outcome") 1)
                                        (not (= (get % "birthord") 1)))
                                  (pregnancies path)))})

(defn average-prglength [pregnancies]
  (/ (apply + (map #(get % "prglength") pregnancies))
     (count pregnancies)))

(defn average-prglength-by-birthord
  [path]
  {:first-baby (float (average-prglength (filter #(and (= (get % "outcome") 1)
                                                       (= (get % "birthord") 1))
                                                 (pregnancies path))))
   :not-first-baby (float (average-prglength (filter #(and (= (get % "outcome") 1)
                                                           (not (= (get % "birthord") 1)))
                                                     (pregnancies path))))})