(ns entomologist.client.test
  (:use [cljs.pprint :only [pprint]])
  (:require [cljs.test :refer-macros [deftest is testing run-tests use-fixtures]]
            [entomologist.client.store :as store]))

(enable-console-print!)

(def cst
  {:path []
   :class :seq-node
   :tag :list
   :children '({:path [0]
                :value def
                :string-value "def"
                :class :token-node}
               {:whitespace " "
                :class :whitespace-node}
               {:path [1] :value toto
                :string-value "toto"
                :class :token-node}
               {:whitespace " "
                :class :whitespace-node}
               {:path [2] :value 33
                :string-value "33"
                :class :token-node})})

(def cst-2
  )

(def ctx
  '({:key :execution
     :value ["2016-01-16T11:02:25.052-00:00"
             1452942145052515000]}))

(def all-data
  `({:insect/cst ~cst
     :insect/name "toto"
     :insect/observation
     {[2] {~ctx :default-displayed}
      [0] {~ctx :special-form-info-displayed}}
     :insect/type :def-form
     :insect/namespace "entomologist.core" :db/id 187}))

(def delete-obs-data
  {:insect/name "toto"
   :observation/path [2]
   :insect/namespace "entomologist.core"
   :observation/context '({:key :execution
                          :value ["2016-01-16T11:02:25.052-00:00"
                                  1452942145052515000]})})

(def delete-insect-data
  {:insect/name "toto", :insect/namespace "entomologist.core"})

(use-fixtures :each
  {:before (fn [] (reset! store/state {}))})

(deftest test-set-all-insects!
  (store/set-all-insects! all-data)
  (is (= @store/state
         {:insects
          {"entomologist.core"
           {"toto"
            {:observations
             {[2] {ctx :default-displayed}
                            [0] {ctx :special-form-info-displayed}}
             
             :cst cst}}}})))

(deftest test-delete-observation!
  (store/set-all-insects! all-data)
  (store/delete-observation! (:insect/namespace delete-obs-data)
                             (:insect/name delete-obs-data)
                             (:observation/path delete-obs-data)
                             (:observation/context delete-obs-data))
  (is (= @store/state
         {:insects
          {"entomologist.core"
           {"toto"
            {:observations
             {[2] {}
              [0] {ctx :special-form-info-displayed}}
             :cst cst}}}})))

(deftest test-delete-insect!
  (store/set-all-insects! all-data)
  (store/delete-insect! (:insect/namespace delete-insect-data)
                        (:insect/name delete-insect-data))
  (is (= @store/state
         {:insects {"entomologist.core" {}}})))

(defn run []
  (run-tests))

