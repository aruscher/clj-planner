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

(defn cant-parse [what]
  (throw (IllegalArgumentException. (str "Cant parse " what))))

(defn build-type-map
  [subtypes supertype]
  (reduce merge
          (map #(do {% (set [% supertype 'object])})
           subtypes)))

(defn parse-requirements
  [content]
  {:requirements content})

(defn parse-types
  ([content] (parse-types content {}))
  ([content result]
   (cond
     (empty? content) result
     :else            (cant-parse content))))

(defn parse
  [clause]
  (let [type (first clause) content (rest clause)]
    (cond
      (= type ':requirements) (parse-requirements content)
      (= type ':types)        (parse-types content)
      :else                   (cant-parse clause))))

(defn parse-domain
  [body result]
  (let [[head & rest] body
        parse-result (parse head)
        n-result (merge result parse-result)]
    (pprint/pprint n-result)
    (parse-domain rest n-result)))

(defmacro define
  [[category name] & body]
  (cond
    (= category 'domain) (parse-domain body {:type :domain :name (str name)})
    (= category 'problem) (println "problem")
    :else (cant-parse category)))

(eval (read-string *foo*))
