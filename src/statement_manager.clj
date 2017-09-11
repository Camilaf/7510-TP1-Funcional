(ns statement-manager
  (:require [parser :refer :all]))

(defrecord Fact [predicate parameters])
(defrecord Rule [predicate variables objectives])
(defrecord Query [predicate parameters])

(defprotocol Consult
  (respond-query [clause query database]))

;; Creates Fact or Rule records according to the given clause.
(defmulti create-clause-record (fn [clause] (rule? clause)))

;; Creates a Fact record with its predicate and parameters.
(defmethod create-clause-record fact-case [fact]
  (let [fact-predicate (obtain-clause-predicate fact)
        fact-parameters (obtain-clause-parameters fact fact-parameters-regex)] 
    (->Fact fact-predicate fact-parameters)))

;; Creates a Rule record with its predicate, variables and objectives.
(defmethod create-clause-record rule-case [rule]
  (let [rule-predicate (obtain-clause-predicate rule)
        rule-variables (obtain-clause-parameters rule rule-variables-regex)
        rule-objectives (obtain-rule-objectives rule)] 
    (->Rule rule-predicate rule-variables rule-objectives)))

(defn create-query-record 
  "Creates a Query record with its predicate and parameters."
  [query]
  (let [query-predicate (obtain-clause-predicate query)
        query-parameters (obtain-clause-parameters query query-parameters-regex)]
    (->Query query-predicate query-parameters)))

(defn same-predicate? 
  "Determines whether the clause and query have the same predicate."
  [clause query]
  (= (:predicate clause) (:predicate query)))

(defn equal-number-of-parameters? 
  "Determines whether the clause and query have the same number
  of parameters."
  [rule query]
  (= (count (:variables rule)) (count (:parameters query))))

(defn create-database-structure 
  "Returns a list with the clauses of the given database converted
  to record."
  [database]
  (map create-clause-record database))

(defn filter-by-query-predicate 
  "Receives the database and a query. Returns a list with records in
  the database that have the same predicate as the query."
  [database query]
  (let [clause-predicate-equal-to-query-predicate? (fn [clause] (same-predicate? clause query))] 
    (filter clause-predicate-equal-to-query-predicate? database)))

(defn process-query 
  "Receives a database and a query. Looks for the clauses with the same
  predicate as the query, and returns a list indicating whether the query 
  is met or not for each clause."
  [database query]
  (let [filter-predicate-list (filter-by-query-predicate database query)]
    (if (empty? filter-predicate-list) '(false)
      (map (fn [clause] (respond-query clause query database)) (filter-by-query-predicate database query)))))

(defn respond-to-query 
  "Receives a database and a query. Indicates whether the query is in 
  the database or if it is false."
  [database query]
  (reduce #(or %1 %2) (process-query database query)))

(defn reconstruct-objective 
  "Receives an objective, variables of a rule and parameters of a query. 
  Maps each rule variable to its corresponding query parameter, and 
  creates a new query for that objective, using the its predicate and 
  instantiating its parameters through the mapping."
  [objective rule-variables query-parameters]
  (let [objective-paramaters-map (zipmap (map keyword rule-variables) query-parameters)
        obtain-parameter-from-variable (fn [parameter] (get objective-paramaters-map (keyword parameter) parameter))]
    (->Query (obtain-clause-predicate objective) (map obtain-parameter-from-variable (obtain-clause-parameters objective query-parameters-regex)))))

(defn respond-objectives 
  "Receives a rule, a query and a database. It takes each objective of the rule as
  a new query and searches it in the database. Returns a list with the results of
  each query."
  [rule query database]
  (map (fn [objective] (respond-to-query database (reconstruct-objective objective (:variables rule) (:parameters query)))) (:objectives rule)))

(extend-protocol Consult
  Fact
  ; Receives a query with its same predicate and returns true in case of having the 
  ; same parameters.
  (respond-query [this query _] 
    (= (:parameters this) (:parameters query)))
  
  Rule
  ; Receives a query with its same predicate and the database. If the query has the
  ; same number of parameters as the rule, it verifies that all the objectives of
  ; the rule are true and in that case it returns true. 
  (respond-query [this query database] 
    (if-not (equal-number-of-parameters? this query) false
      (every? true? (respond-objectives this query database)))))

