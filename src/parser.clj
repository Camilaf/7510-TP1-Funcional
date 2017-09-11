(ns parser
  (:require [clojure.string]))
  
;; Definitions

(def rule-operator ":-")

;; Statements
(def query-regex #"([a-z]+[a-z-]*)\(([a-z0-9]+)(,[a-z0-9]+)*\)")
(def fact-regex #"([a-z]+[a-z-]*)\(([a-z0-9]+)(,[a-z0-9]+)*\)\.")
(def rule-regex #"([a-z]+[a-z-]*)\(([A-Z]+)(,[A-Z]+)*\):-([a-z]+[a-z-]*)\((\w+)(,\w+)*\)(,([a-z]+[a-z-]*)\((\w+)(,\w+)*\))*\.")

;; Parameters
(def query-parameters-regex #".*\((.*)\)")
(def rule-variables-regex #".*\((.*)\)\:-.*")
(def fact-parameters-regex #".*\((.*)\)\.")

;; Rule objectives
(def rule-objectives-regex #".*\(.*\)\:-(.*)\.")

(def rule-case true)
(def fact-case false)

;; Cleaning

(defn obtain-database-lines
  "Receives the database as a string and returns a list with each
  of its lines (clauses)."
  [database]
  (remove empty? (clojure.string/split-lines database)))

(defn remove-tabs-and-spaces 
  "Receives a clause and removes tabs and spaces from it."
  [clause]
  (clojure.string/replace clause #"\t| " ""))

(defn remove-spaces 
  "Receives a clause and removes spaces from it."
  [clause]
  (clojure.string/replace clause #" " ""))

;; Validation

(defn rule? 
  "Receives a clause and indicates whether it is a rule or not."
  [clause]
  (clojure.string/includes? clause rule-operator))

(defn valid? 
  "Receives a clause and its regex type. Indicates whether it is
  a valid clause or not."
  [clause type-clause-regex]
  ((complement nil?) (re-matches type-clause-regex clause)))

; Multimethod with a dispatch function that determines what to do
; according to the type of 'clause'.
(defmulti valid-clause? (fn [clause] (rule? clause)))

; Determines if the received fact is valid.
(defmethod valid-clause? fact-case [fact]
  (valid? fact fact-regex))

; Determines if the received rule is valid.
(defmethod valid-clause? rule-case [rule]
  (valid? rule rule-regex))

(defn valid-query? 
  "Determines if the received query is valid."
  [query]
  (valid? query query-regex))

(defn valid-database? 
  "Determines if the received database is valid."
  [database]
  (every? true? (map valid-clause? database)))

(defn valid-entries? 
  "Determines whether the database and the query are valid or not."
  [database query]
  (and (valid-database? database) (valid-query? query)))

;; Obtaining information

(defn obtain-clause-predicate 
  "Returns the predicate of the given clause."
  [clause]
  (re-find #"[a-z]+" clause))

(defn obtain-rule-objectives [rule]
  "Returns a list with the objectives (components) of the given rule."
  (let [rule-objectives-index 1
        rule-objectives-string (nth (re-matches rule-objectives-regex rule) rule-objectives-index)]
    (clojure.string/split rule-objectives-string #"(?<=\)),")))

(defn obtain-clause-parameters 
  "Returns the parameters of the given clause, according to
  the regular expression."
  [clause clause-parameters-regex]
  (let [clause-parameters-index 1
        clause-parameters-string (nth (re-matches clause-parameters-regex clause) clause-parameters-index)]
    (clojure.string/split clause-parameters-string #",")))

