(ns think-stats.thinkstats)

;; Corresponds to http://greenteapress.com/thinkstats/thinkstats.py

(defn mean [l]
  (/ (apply + l)
     (count l)))

(defn variance
  ([l]
     (let [mu (mean l)
           dev2 (map #(Math/pow (- % mu) 2) l)]
       (mean dev2)))
  ([l mu]
     (let [dev2 (map #(Math/pow (- % mu) 2) l)]
       (mean dev2))))