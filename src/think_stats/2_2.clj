(ns think-stats.2-2
  "Example 2-2"
  (:use think-stats.survey
        think-stats.thinkstats))

(defn- sd [pregnancies]
  (Math/sqrt (variance (map #(get % "prglength")
                            pregnancies))))

(defn sd-of-gestatation-time [path]
  {:first-baby (sd (filter #(and (= (get % "outcome") 1)
                                    (= (get % "birthord") 1))
                           (pregnancies path)))
   :not-first-baby (sd (filter #(and (= (get % "outcome") 1)
                                     (not (= (get % "birthord") 1)))
                           (pregnancies path)))})