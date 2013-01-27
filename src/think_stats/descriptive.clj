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

(def path "2002FemPreg.dat.gz")
(def first-born (gestation-period-by-birthord (pregnancies path) true))
(def others (gestation-period-by-birthord (pregnancies path) false))  

(defn plot-frequencies []
  (let [first-hist (into (sorted-map) (hist first-born))
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
          "/tmp/freq.png"
          :width 1000)))

(defn plot-pmf []
  (let [first-pmf (into (sorted-map) (pmf (hist first-born)))
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

(defn plot-pmf-differences []
  (let [first-pmf (into (sorted-map) (pmf (hist first-born)))
        others-pmf (into (sorted-map) (pmf (hist others)))
        weeks (range 35 46)]
    (save (bar-chart weeks
                     (map
                      #(* 100 (- (get first-pmf %)
                                 (get others-pmf %)))
                      weeks)
                     :x-label "weeks"
                     :y-label (str "100x(PMF(first) - PMF(second))"))
          "/tmp/bar.png"
          :width 1000)))
