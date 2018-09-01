(ns clj-planner.parser-test
  (:require [clj-planner.parser :as sut]
            [clojure.test :as t]))

(def *test-types* '(car - object saab colvo - target))

(testing "Parser"
  (testing "Domain parser"
    (testing "type parsing"
      (is =()


