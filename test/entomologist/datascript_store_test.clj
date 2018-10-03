(ns entomologist.datascript-store-test
  (:use [clojure.pprint])
  (:require [clojure.test :refer :all]
            [datascript.core :as d]
            [entomologist.self.read :refer [raw-code]
             :reload true]
            [entomologist.cst :as cst
             :reload true]
            [entomologist.store :as store
             :reload true]))


;; TODO : find a better way to get entities of a certain type
(defn fetch-all [db k]
  (d/q '[:find (pull ?e [*])
         :in $ ?k
         :where [?e ?k _]]
       @db k))

(defn assert-db [db k content]
  (let [db-content (map first (fetch-all db k))]
    (when (not= (set content)
                (set (map #(dissoc % :db/id) db-content)))
      (println "==============================")
      (pprint (set content))
      (println "------------------------------")
      (pprint (set (map #(dissoc % :db/id) db-content)))
      (println "==============================")
      )
    (is (= (set content)
           (set (map #(dissoc % :db/id) db-content))))))

(defmacro assert-thrown [exception-pred & body]
  `(let [ex# (try
               ~@body
               false
               (catch Throwable t#
                 t#))]
     (assert (and ex# (~exception-pred ex#)))))





(defn db-fixture [f]
  (with-redefs [store/db (d/create-conn store/schema)]
    (f)))

(use-fixtures :each db-fixture)

(def cst
  (-> (raw-code
        (def toto (inc 32)))
      cst/add-paths))

(def cst-2
  (-> (raw-code
        (def toto (inc 31)))
      cst/add-paths))

(def cst-3
  (-> (raw-code
        (def tata (inc 33)))
      cst/add-paths))

(def insect-name
  "toto")

(def toto-insect
  {:insect/name insect-name
   :insect/type :def-form
   :insect/cst cst
   :insect/namespace *ns*})

(def tata-insect
  {:insect/name "tata"
   :insect/type :def-form
   :insect/cst cst-3
   :insect/namespace *ns*})





(deftest test-differ-as-sexprs?
  (is (testing
        "code with different sexprs differ as sexprs"
        (store/differ-as-sexprs?
          (raw-code (def abc 123))
          (raw-code (def abc 456))))
      (testing
        "code with different strings but same sexprs do not differ as sexprs"
        (not (store/differ-as-sexprs?
               (raw-code (def abc 123))
               (raw-code (def abc    123)))))))

(deftest test-differ-as-strings?
  (is (testing
        "code with different strings but same sexprs differ as strings"
        (store/differ-as-strings?
          (raw-code (def abc 123))
          (raw-code (def abc    123))))
      (testing
        "code with different strings and different sexprs differ as strings"
        (store/differ-as-strings?
          (raw-code (def abc 123))
          (raw-code (def abc 456))))))

(deftest test-insect-name
  (store/create-insect! *ns* cst)
  (is (= insect-name  (store/insect-name *ns* cst)))
  (is (= "[:a :b :c]" (store/insect-name *ns* (raw-code [:a :b :c])))))

(deftest test-create-insect!
  (store/create-insect! *ns* cst)
  (store/create-insect! *ns* cst-3)
  (testing
    "Insects get created"
    (assert-db store/db :insect/name
               [toto-insect
                tata-insect]))
  (testing
    "Creating an insect for a namespace and a name that already exist yields an
    ex-info of :type :insect-already-exists"
    (assert-thrown #(-> % ex-data :type (= :insect-already-exists))
                   (store/create-insect! *ns* cst))))

(deftest test-insect-exists?
  (store/create-insect! *ns* cst)
  (is (store/insect-exists? *ns* insect-name)
      (not (store/insect-exists? *ns* "non-existent"))))

(deftest test-get-insect!
  (store/create-insect! *ns* cst)
  (testing
    "Insects can be get"
    (is (= (dissoc (store/get-insect *ns* insect-name) :db/id)
           toto-insect)))
  (testing
    "Getting a non-existent insect with respect to :insect/namespace and
    :insect/name yields an ex-info of :type :insect-not-found"
    (assert-thrown #(-> % ex-data :type (= :insect-not-found))
                   (store/get-insect *ns* "non-existent"))))

(deftest test-delete-insect!
  (testing "Insects get deleted"
    (store/create-insect! *ns* cst)
    (store/delete-insect! *ns* insect-name)
    (assert-db store/db :insect/name []))
  (testing
    "Deleting an insect also deletes the associated observations"
    (store/create-insect! *ns* cst)
    (store/create-observation! *ns* insect-name [0] [[:ctx1][:ctx2]] :value1)
    (store/create-observation! *ns* insect-name [0] [[:ctx1]] :value2)
    (store/delete-insect! *ns* insect-name)
    (assert-db store/db :insect/name [])
    (assert-db store/db :observation/insect [])))

(deftest test-create-observation!
  (store/create-insect! *ns* cst)
  (store/create-observation! *ns* insect-name [0] [[:ctx1][:ctx2]] :value)
  (let [insect-id (:db/id (store/get-insect *ns* insect-name))]
    (testing "Observations get created for existent insects"
      (assert-db store/db :observation/context
                 [{:observation/context [[:ctx1][:ctx2]]
                   :observation/value :value
                   :observation/path [0]}]))
    (testing
      "Creating an observation for a non-existent insect yields an ex-info of
      :type :insect-not-found"
      (assert-thrown #(-> % ex-data :type (= :insect-not-found))
                     (store/create-observation!
                       *ns* "x" [0] [[:ctx1][:ctx2]] :value)))
    (testing
      "Creating an observation that already exist with respect to
      :insect/namespace, :insect/name and observation/context yield an ex-info
      of :type :observation-already-exists"
      (assert-thrown #(-> % ex-data :type (= :observation-already-exists))
                     (store/create-observation!
                       *ns* insect-name [0] [[:ctx1][:ctx2]] :value2)))))

(deftest test-set-observation!
  (let [t1 [(new java.util.Date) (System/nanoTime)]
        ctx1 [{:key :execution :value t1}
              {:key :something :value :something}]
        ctx2 [{:key :execution :value t1}
              {:key :something :value :something-else}]
        t2 [(new java.util.Date) (System/nanoTime)]
        ctx3 [{:key :execution :value t2}
              {:key :something :value :something}]]
    (testing
      "An observation get set when no other observation is present"
      (store/create-insect! *ns* cst)
      (store/set-observation! *ns* insect-name [0] ctx1 :value)
      (assert-db store/db :observation/value
                 [{:observation/context ctx1
                   :observation/value :value
                   :observation/path [0]}]))
    (testing
      "An observation get set (added) when other observations are present for
      the same exec context but under a different path"
      (store/set-observation! *ns* insect-name [1] ctx1 :value)
      (assert-db store/db :observation/value
                 [{:observation/context ctx1
                   :observation/value :value
                   :observation/path [0]}
                  {:observation/context ctx1
                   :observation/value :value
                   :observation/path [1]}]))
    (testing
      "An observation get set (added) when observations are present for the same
      path but with different execs contexts that share this observation's root"
      (store/set-observation! *ns* insect-name [0] ctx2 :value)
      (assert-db store/db :observation/value
                 [{:observation/context ctx1
                   :observation/value :value
                   :observation/path [0]}
                  {:observation/context ctx1
                   :observation/value :value
                   :observation/path [1]}
                  {:observation/context ctx2
                   :observation/value :value
                   :observation/path [0]}]))
    (testing
      "When setting an observation with an execution context that shares no root
      with already present contexts, all these observations get deleted when
      this one gets added."
      (store/set-observation! *ns* insect-name [0] ctx3 :value)
      (assert-db store/db :observation/value
                 [{:observation/context ctx3
                   :observation/value :value
                   :observation/path [0]}]))))

(deftest test-renew-insect!
  (store/create-insect! *ns* cst)
  (store/create-observation! *ns* insect-name [0] [[:ctx1][:ctx2]] :value1)
  (store/renew-insect! *ns* insect-name cst-2)
  (assert-db store/db :insect/name
             [(assoc toto-insect :insect/cst cst-2)])
  (assert-db store/db :observation/value
             []))

(deftest test-update-insect-cst!
  (store/create-insect! *ns* cst)
  (store/update-insect-cst! *ns* insect-name cst-2)
  (assert-db store/db :insect/name
             [(assoc toto-insect :insect/cst cst-2)]))

(deftest test-upsert-insect!
  (testing
    "Insects get upserted (inserted)"
    (store/upsert-insect! *ns* cst)
    (assert-db store/db :insect/name
               [toto-insect]))
  (testing
    "Insects get upserted (sexpr-equivalent: update cst)"
    (store/create-observation! *ns* insect-name [0] [[:ctx1]] :value1)
    (store/upsert-insect! *ns* (raw-code (def toto    (inc 32))))
    (assert-db store/db :observation/context
               [{:observation/context [[:ctx1]]
                 :observation/value :value1
                 :observation/path [0]}]))
  (testing
    "Insects get upserted (sexpr-different: renew insect)"
    (store/upsert-insect! *ns* (raw-code (def toto 456)))
    (assert-db store/db :observation/context [])))


(run-tests)


