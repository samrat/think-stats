(ns think-stats.2-7
  "Example 2-7"
  (:use think-stats.pmf
        think-stats.descriptive)
  (:use (incanter core charts)))

(defn prob-week39 [pmf]
  (-> (normalize (into {} (remove #(< (key %) 39) pmf)))
      (get 39)
      (float)))

(defn prob-week-x [pmf x]
  (-> (normalize (into {} (remove #(< (key %) x) pmf)))
      (get x)
      (float)))

(defn plot-prob-x []
  (let [first-pmf (pmf (hist first-born))
        others-pmf (pmf (hist others))
        weeks (range 35 46)]
    (save (line-chart (interleave weeks
                                 weeks)
                     (interleave (map #(prob-week-x first-pmf %) weeks)
                                 (map #(prob-week-x others-pmf %) weeks))
                     :x-label "weeks"
                     :y-label "P(born on week x | not born before week x)"
                     :legend true
                     :group-by (take (count (interleave
                                             (keys (hist first-born))
                                             (keys (hist others))))
                                     (interleave (repeat "first born")
                                                 (repeat "others"))))
          "/tmp/probx.png"
          :width 1000
          :height 800)))
