(ns entomologist.summarized-coll-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [entomologist.client-server.summarized-coll
             :refer [summarized-coll queue]
             :reload true]))

(def limit
  50)

(def sm
  (summarized-coll limit [1 2 3 4 5]))

(deftest test-queue
  (testing "Testing reader macro"
    (let [q (queue [1 2 3 4])]
      (is (= (conj (read-string (pr-str q))
                   5)
             [1 2 3 4 5])))))

(deftest test-summarized-coll
  (testing "Testing equivalence"
    (let [sc1 (summarized-coll 4 [1 2 3 4])
          sc2 (summarized-coll 4 [1 2 3 4])
          sc3 (summarized-coll 4 [1 2 :a 4 5])
          sc4 (summarized-coll 4 [1 2 :b 4 5])]
      (is (= sc1 sc2))
      (is (= sc3 sc4))
      (is (= sc1 [1 2 3 4]))
      (is (not= sc3 [1 2 :a 4 5]))))
  (testing "Testing reader macro"
    (let [sc1 (summarized-coll 4 [1 2 3 4])
          sc2 (binding [*print-dup* true]
                (conj (read-string (pr-str sc1))
                      5))]
      (is (= sc2 (summarized-coll 4 [1 2 3 4 5]))))))

(defmacro chrono [& body]
  `(let [start# (System/currentTimeMillis)
         res# (doall ~@body)
         end# (System/currentTimeMillis)]
     {:result res#
      :duration (- end# start#)}))

(deftest test-summarized-coll-perfs
  (testing "Testing conj perfs (should be constant)"
    (let [{dur-a :duration} (chrono (reduce conj (summarized-coll 50)
                                            (range 1000)))
          {dur-b :duration} (chrono (reduce conj (summarized-coll 50)
                                            (range 10000)))
          {dur-c :duration} (chrono (reduce conj (summarized-coll 50)
                                            (range 100000)))
          ratio1 (/ (* dur-a 10) dur-b)
          ratio2 (/ (* dur-b 10) dur-c)]
      (is (and (> ratio1 80/100)
               (< ratio1 120/100)))
      (is (and (> ratio2 80/100)
               (< ratio2 120/100))))))

(run-tests)