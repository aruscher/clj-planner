(ns clj-planner.parser
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

(def *test-definition "(define (domain foo))")
(def *test-domain* (slurp "src/resources/hanoi-domain.pddl"))
(def *test-problem* (slurp "src/resources/hanoi-problem.pddl"))
(def *test-types* '(disc - target peg - target))

(defn cant-parse [what]
  (throw (IllegalArgumentException. (str "Cant parse " what))))

(defn build-supertype-set-map
  [subtype supertype]
  (if (nil? supertype)
    {subtype (into #{} [subtype 'object])}
    {subtype (into #{} [subtype supertype 'object])}))

(defn build-type-map
  [subtypes supertype]
  (reduce merge
          (map #(build-supertype-set-map % supertype) subtypes)))

(defn split-types
  [content]
  (split-with #(not= '- %) content))

(defn get-supertype
  [content]
  (let [rest (next content)
        supertype (first rest)
        to-parse (next rest)]
    [supertype to-parse]))

(defn parse-requirements
  [content]
  content)

(defn parse-types
  ([content] (parse-types content {}))
  ([content result]
   (let [[subtypes others] (split-types content)
         [supertype to-parse] (get-supertype others)
         type-map (build-type-map subtypes supertype)]
     (if (empty? content) result (parse-types to-parse (merge type-map result))))))

(defn parse
  [clause]
  (let [type (first clause) content (rest clause)]
    (cond
      (= type ':requirements) {:requirements (parse-requirements content)}
      (= type ':types)        {:types (parse-types content)}
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
