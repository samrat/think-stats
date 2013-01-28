(ns think-stats.risk
  "Corresponds to http://thinkstats.com/risk.py.
   Relative risk."
  (:use think-stats.pmf
        think-stats.descriptive))

(def first-pmf (pmf (hist first-born)))
(def other-pmf (pmf (hist others)))
(def all-live-pmf (pmf (hist (concat first-born others))))

(defn probability-bins [pmf]
  (let [total (apply + (vals pmf))]
    {:early (/ (apply + (vals (filter #(<= (key %) 37)
                                      pmf)))
               total)
     :on-time (/ (apply + (vals (filter #(#{38 39 40} (key %))
                                        pmf)))
                 total)
     :late (/ (apply + (vals (filter #(>= (key %) 41)
                                     pmf)))
              total)}))

(defn relative-risk [prob1 prob2]
  (float (/ prob1 prob2)))

(comment
  (relative-risk (:early (probability-bins first-pmf))
                 (:early (probability-bins other-pmf))))
;; A value of 1.08 means that it is 8% more likely that a first babies
;; will be born early
