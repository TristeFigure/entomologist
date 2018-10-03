(ns entomologist.store-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [entomologist.cst :as cst]
            [entomologist.config :as config]
            [entomologist.client-server.protocols.limited :refer [limited?]]
            [entomologist.self.read :refer [raw-code]
             :reload true]
            [entomologist.store :as store
             :reload true]
            [entomologist.client-server.limited-map
             :refer [limited-map]
             :reload true]))

;; TODO: ponder on whether :insect/name kind of keywords should be splitted into
;;       2 keywords, namely :insect & :name.
;;       According to the way plugins should work: no since every keyword should
;;       also be namespaced to aboid namespace conflict between plugins.
;;       But we could use '-o' as keyword namespace

(deftest test-differ-as-sexprs?
  (testing
    "code with different sexprs differ as sexprs"
    (is (true? (store/differ-as-sexprs?
                 (raw-code (def abc 123))
                 (raw-code (def abc 456))))))
  (testing
    "code with different strings but same sexprs do not differ as sexprs"
    (is (false? (store/differ-as-sexprs?
                  (raw-code (def abc 123))
                  (raw-code (def abc    123)))))))

(deftest test-differ-as-strings?
  (testing
    "code with different strings and different sexprs differ as strings"
    (is (true? (store/differ-as-strings?
                 (raw-code (def abc 123))
                 (raw-code (def abc 456))))))
  (testing
    "code with different strings but same sexprs differ as strings"
    (is (true? (store/differ-as-strings?
                 (raw-code (def abc 123))
                 (raw-code (def abc    123)))))))

(deftest test-insect-type
  (testing "an anonymous form is recognized as such"
    (is (= :anonymous-form
           (store/insect-type (raw-code (inc 123))))))
  ;; MAYBE: abstract logic to 'named form', where :def-form is just a special
  ;;        case for a to be created named-form. A named form is a form that
  ;;        creates a var in a namespace. Named anonymous fn forms should be
  ;;        recognized as anonymous for now.
  (testing "a def form is recognized as such"
    (is (= :def-form
           (store/insect-type (raw-code (def inc-one [x] (inc x)))))))
  (testing "an named anonymous fn form is recognized as an anonymous form"
    (is (= :anonymous-form
           (store/insect-type (raw-code (fn abc [x] (inc x))))))))

(deftest test-insect-name
  (let [_uuid (str (java.util.UUID/randomUUID))
        uuid? (fn [x]
                (and (string? x)
                     (= (count _uuid) (count x))))]
    (testing "a def-form's name is as expected"
      (is (= "abcdef"
             (store/insect-name
               (raw-code (def abcdef :abcded))))))
    ;; TODO: should change when we implement insect ID written in code itself.
    (testing "an anonymous form's name is an UUID"
      (is (uuid? (store/insect-name
                   (raw-code (inc 1))))))
    (testing "a named ananonymous fn form's name is still an UUID"
      (is (uuid? (store/insect-name
                   (raw-code (fn abc [x] (inc x)))))))))

(deftest test-get-insect
  (let [raw (raw-code (def abc :abc))
        expected raw
        db-init {*ns* {"abc" {:insect/cst raw}}}]
    (with-redefs [store/db (atom db-init)]
      (is (store/get-insect *ns* "abc")
          {:insect/cst raw}))))

(deftest test-insect-exists?
  (let [raw (raw-code (def abc :abc))
        db-init {*ns* {"abc" {:insect/cst raw}}}]
    (with-redefs [store/db (atom db-init)]
      (is (true? (store/insect-exists? *ns* "abc"))))))

(deftest test-create-insect!
  (let [raw (raw-code (def abc :abc))
        expected {*ns* {"abc" {:insect/cst raw}}}
        db-init {}]
    (with-redefs [store/db (atom db-init)]
      (store/create-insect! *ns* raw)
      (is (= @store/db
             expected)))))

(deftest test-renew-insect!
  (let [raw (raw-code (def abc :abc))
        expected (raw-code (def abc   :abc))
        db-init {*ns* {"abc" {:insect/cst raw}}}]
    (with-redefs [store/db (atom db-init)]
      (store/renew-insect! *ns* expected)
      (is (= (get-in @store/db [*ns* "abc" :insect/cst])
             expected)))))

(deftest test-update-insect-cst!
  (let [raw (raw-code (def abc :abc))]
    (testing "when CSTs differ as sexprs, whole insect is renewed"
      (let [expected-raw (raw-code (def abc :def))
            expected {*ns* {"abc" {:insect/cst expected-raw}}}
            db-init {*ns* {"abc" {:insect/cst raw
                                  :something-else :xyz}}}]
        (with-redefs [store/db (atom db-init)]
          (store/update-insect-cst! *ns* expected-raw)
          (is (= @store/db
                 expected)))))
    (testing "when CSTs differ as strings, only cst is updated"
      (let [expected-raw (raw-code (def abc   :abc))
            expected {*ns* {"abc" {:insect/cst expected-raw
                                   :something-else :xyz}}}
            db-init {*ns* {"abc" {:insect/cst raw
                                  :something-else :xyz}}}]
        (with-redefs [store/db (atom db-init)]
          (store/update-insect-cst! *ns* expected-raw)
          (is (= @store/db
                 expected)))))
    (testing "when CSTs do not differ either as sexprs or as strings,
             there is no change"
      (let [expected-raw raw
            expected {*ns* {"abc" {:insect/cst expected-raw
                                   :something-else :xyz}}}
            db-init {*ns* {"abc" {:insect/cst raw
                                  :something-else :xyz}}}]
        (with-redefs [store/db (atom db-init)]
          (store/update-insect-cst! *ns* expected-raw)
          (is (= @store/db
                 expected)))))))

(deftest test-upsert-insect!
  (let [raw (raw-code (def abc :abc))]
    (testing "upserting an already existing insect updates the insect's cst"
      (let [db-init {*ns* {"abc" {:insect/cst raw
                                  :something-else :xyz}}}]
        (with-redefs [store/db (atom db-init)]
          (store/upsert-insect! *ns* (raw-code (def abc  :abc)))
          (= (count (get-in db-init [*ns* "abc"]))
             (count (get-in @store/db [*ns* "abc"]))))))
    (testing "upserting a never seen before insect insect creates a new insect"
      (let [db-init {*ns* {"abc" {:insect/cst raw
                                  :something-else :xyz}}}]
        (with-redefs [store/db (atom db-init)]
          (store/upsert-insect! *ns* (raw-code (def abcd  :abcd)))
          (= (count (get-in db-init [*ns*]))
             (dec (count (get-in @store/db [*ns*])))))))))

(deftest test-remark-insect!
  (let [raw (raw-code (def abc
                          :abc))
        cleaned-raw (cst/clean-node raw)
        db-init {}]
    (with-redefs [store/db (atom db-init)]
      (let [ret (store/remark-insect! *ns* raw)
            new-cst (get-in @store/db [*ns* "abc" :insect/cst])]
        (testing "it returns the insect's name"
          (= ret "abc"))
        (testing "it cleans the cst and places the insect inside the db"
          (is (= new-cst
                 cleaned-raw)))
        (testing "no uncleaned CSTs are stored"
          (is (false? (= new-cst
                         raw))))))))

(deftest test-set-observation!
  (let [raw (raw-code (def abc :abc))]
    (testing "new observations get created"
      (let [expected-obs {0 {0 {7 {{:type :one} {{:type :two} :value}}}}}
            expected {*ns* {"abc" {:insect/cst raw
                                   :insect/observations expected-obs}}}
            db-init {*ns* {"abc" {:insect/cst raw}}}]
        (with-redefs [store/db (atom db-init)]
          (store/set-observation! *ns* "abc" [0 0 7] [{:type :one}{:type :two}]
                                  :value)
          (is (= @store/db
                 expected)))))
    (testing "old observations get updated"
      (let [original-obs {0 {0 {7 {{:type :one} {{:type :two} :old-value}}}}}
            expected-obs {0 {0 {7 {{:type :one} {{:type :two} :new-value}}}}}
            expected {*ns* {"abc" {:insect/cst raw
                                   :insect/observations expected-obs}}}
            db-init  {*ns* {"abc" {:insect/cst raw
                                   :insect/observations original-obs}}}]
        (with-redefs [store/db (atom db-init)]
          (store/set-observation! *ns* "abc" [0 0 7] [{:type :one}{:type :two}]
                                  :new-value)
          (is (= @store/db
                 expected)))))
    (testing "observations with a path containing a limited sub-key are limited
             in this part of the path"
             (let [limit 2
                   limited-key (first store/limited-observations-keys)
                   expected-obs {0 {limited-key (limited-map limit
                                                             {:k1 :v1
                                                              :k2 :v2})}}
                   expected {*ns* {"abc" {:insect/observations expected-obs}}}
                   db-init  {}
                   config-init (assoc @config/config :observations-limit limit)]
               (with-redefs [store/db (atom db-init)
                             config/config (atom config-init)]
                 (store/set-observation! *ns* "abc" [0] [limited-key :k1] :v1)
                 (store/set-observation! *ns* "abc" [0] [limited-key :k2] :v2)
                 (store/set-observation! *ns* "abc" [0] [limited-key :k3] :v3)
                 (let [tested (get-in @store/db [*ns* "abc" :insect/observations
                                                 0 limited-key])]
                   (is (not (nil? tested)))
                   (is (true? (limited? tested)))
                   (is (= @store/db
                          expected))))))))

; (deftest test-get-observations []
;   (let [raw (raw-code (def abc :abc))
;         db-init {*ns* {"abc" {:insect/cst raw}}}]
;     (with-redefs [store/db (atom db-init)]
;       (store/set-observation! *ns* "abc" [0] [:runtime :k1] :v1))))

(comment
  (deftest test-mark-exec!
    ;; it's just a transparent call to set-observation!
    ;;  --> TODO: remove mark-exec?
    ))

(run-tests)
