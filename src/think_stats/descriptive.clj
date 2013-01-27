(ns think-stats.descriptive
  (:use (incanter core charts)
        think-stats.survey
        think-stats.pmf))

(defn gestation-period-by-birthord [pregnancies first]
  (map #(get % "prglength")
       (filter #(and (= (get % "outcome") 1)
                     (if first
                       (= (get % "birthord") 1)
                       (not= (get % "birthord") 1)))
               pregnancies)))

(defn plot-frequencies [path]
  (let [first-born (gestation-period-by-birthord (pregnancies path) true)
        others (gestation-period-by-birthord (pregnancies path) false)
        first-hist (into (sorted-map) (hist first-born))
        others-hist (into (sorted-map) (hist others))]
    (save (bar-chart (interleave (keys first-hist)
                                 (keys others-hist))
                     (interleave (vals first-hist)
                                 (vals others-hist))
                     :x-label "weeks"
                     :y-label "frequency"
                     :legend true
                     :group-by (take (count (interleave
                                                      (keys first-hist)
                                                      (keys others-hist)))
                                              (interleave
                                               (repeat "first born")
                                               (repeat "others"))))
          "/tmp/foo2.png"
          :width 1000)))

(defn plot-pmf [path]
  (let [first-born (gestation-period-by-birthord (pregnancies path) true)
        others (gestation-period-by-birthord (pregnancies path) false)
        first-pmf (into (sorted-map) (pmf (hist first-born)))
        others-pmf (into (sorted-map) (pmf (hist others)))]
    (save (bar-chart (interleave (keys first-pmf)
                                 (keys others-pmf))
                     (interleave (vals first-pmf)
                                 (vals others-pmf))
                     :x-label "weeks"
                     :y-label "probability"
                     :legend true
                     :group-by (take (count (interleave
                                                      (keys first-pmf)
                                                      (keys others-pmf)))
                                              (interleave
                                               (repeat "first born")
                                               (repeat "others"))))
          "/tmp/pmf.png"
          :width 1000)))
