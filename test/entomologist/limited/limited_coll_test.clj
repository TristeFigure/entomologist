(ns entomologist.limited.limited-coll-test
  (:require [clojure.test :refer :all]
            [entomologist.client-server.limited-coll
             :refer [limited-coll]
             :reload true]))

(deftest test-limited-coll
  (testing "Testing conj"
    (let [lc1 (limited-coll 4 [1 2 3])
          lc2 (conj lc1 4)
          lc3 (conj lc2 5)]
      (is (= lc2 (limited-coll 4 [1 2 3 4])))
      (is (= lc3 (limited-coll 4 [1 2 3 4 5])))))
  (testing "Testing reader macro"
    (let [c1 (limited-coll 4 [1 2 3])
          c2 (binding [*print-dup* true]
               (conj (read-string (pr-str c1))
                     4))]
      (is (= c2 (limited-coll 4 [1 2 3 4]))))))

(run-tests)