(ns clj-planner.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

(def *test-foo* "(define (domain foo))")

(def *foo* (slurp "src/resources/hanoi-problem.pddl"))

(defn tokenize [str]
  (remove #(str/blank? %)
          (str/split (str/replace str #"[\(\)]" " $0 ")
                     #"\s")))

(defn parse-word
  [tokens]
  {:result (first tokens) :used-tokens 1})

(defn parse-domain-name
  [tokens]
  (let [{name :result used-tokens :used-tokens} (parse-word (rest tokens))]
    {:result {:type :domain :name name} :used-tokens 2}))

(defn parse
  [tokens result]
  (let [f-token (first tokens) r-tokens (rest tokens)]
    (cond
      (= f-token "(") (parse r-tokens result)
      (= f-token ")") result
      (= f-token "define") (parse r-tokens result)
      (= f-token "domain") (let [{name-result :result used-tokens :used-tokens} (parse-domain-name tokens)]
                             (parse (drop used-tokens tokens) (merge result name-result)))
      :else (throw (IllegalArgumentException. (str "cant parse " f-token))))))

(pprint/pprint (tokenize *test-foo*))
(pprint/pprint (parse (tokenize *test-foo*) {}))
