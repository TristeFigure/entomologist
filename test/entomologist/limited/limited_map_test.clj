(ns entomologist.limited.limited-map-test
  (:require [clojure.test :refer :all]
            [entomologist.client-server.limited-map
             :refer [limited-map]
             :reload true]))

(deftest test-limited-map
  (testing "Testing assoc"
    (let [lc1 (limited-map 3 {:a :b, :c :d})
          lc2 (assoc lc1 :e :f)
          lc3 (assoc lc2 :g :h)
          result (limited-map 3 {:a :b, :c :d, :e :f})]
      (is (= lc2 result))
      (is (= lc3 result))))
  (testing "Testing get"
    (let [lc (limited-map 3 {:a :b, :c :d})]
      (is (= :b (get lc :a)))))
  (testing "Testing reader macro"
    (let [c1 (limited-map 2 {:a :b})
          c2 (binding [*print-dup* true]
               (assoc (read-string (pr-str c1))
                 :c :d))]
      (is (= c2 (limited-map 2 {:a :b :c :d}))))))

(run-tests)