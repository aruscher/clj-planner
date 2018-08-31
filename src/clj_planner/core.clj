(ns clj-planner.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

(def *test-definition "(define (domain foo))")
(def *test-domain* (slurp "src/resources/hanoi-domain.pddl"))
(def *test-problem* (slurp "src/resources/hanoi-problem.pddl"))
(def *test-types* '(disc - target peg - target))

(defn tokenize [str]
  (remove #(str/blank? %)
          (str/split (str/replace str #"[\(\)]" " $0 ")
                     #"\s")))

(defn cant-parse [what]
  (throw (IllegalArgumentException. (str "Cant parse " what))))


(defn build-supertype-set-map
  [subtype supertype]
  (cond
    (nil? supertype) {subtype #{subtype 'object}}
    :else {subtype #{subtype supertype 'object}}))

(defn build-type-map
  [subtypes supertype]
  (reduce merge
          (map #(build-supertype-set-map % supertype) subtypes)))

(defn parse-requirements
  [content]
  {:requirements content})

(defn parse-types
  ([content] (parse-types content {}))
  ([content result]
   (let [[subtypes others] (split-with #(not= '- %) content)
         supertype (first (next others))
         ]
     (println "Subtypes: " subtypes "; Supertype: " supertype))))

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
