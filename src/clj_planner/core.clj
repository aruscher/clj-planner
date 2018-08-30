(ns clj-planner.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

(def *test-foo* "(define (domain foo))")

(def *foo* (slurp "src/resources/hanoi-domain.pddl"))

(defn tokenize [str]
  (remove #(str/blank? %)
          (str/split (str/replace str #"[\(\)]" " $0 ")
                     #"\s")))




(defmacro define
  [[category name] & body]
  (cond
    (= category 'domain)(println "domain")
    (= category 'problem)(println "problem")
    :else (println "Cant parse " category)))


(eval (read-string *foo*))
