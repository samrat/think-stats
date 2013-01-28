(ns think-stats.2-4
  "Example 2-4"
  (:use think-stats.pmf))

(defn remaining-lifetime [pmf age]
  (normalize
   (into {} (remove #(neg? (key %))
                    (apply hash-map
                           (interleave (map #(- (key %) age) pmf)
                                       (vals pmf)))))))