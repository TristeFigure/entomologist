(ns entomologist.generated.clojure.core.butlast)

(butlast [1 2 3])
(1 2)
(butlast (butlast [1 2 3]))
(1)
(butlast (butlast (butlast [1 2 3])))
nil

;; -------------

;really slow reverse
;put the last item of the list at the start of a new list, and recur over all but the last item of the list.
;butlast acts similar to next in that it returns null for a 1-item list.

(defn my-reverse [xs]
  (when xs
    (cons (last xs) (my-reverse (butlast xs)))))