(ns logical-interpreter
  (:require [parser :refer :all]
            [statement-manager :refer :all]))

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil."
  [database query]
  (let [clean-database (map remove-tabs-and-spaces (obtain-database-lines database))
        clean-query (remove-spaces query)]
    (if (valid-entries? clean-database clean-query)
        (let [query-record (create-query-record clean-query)
              database-structure (create-database-structure clean-database)]
            (respond-to-query database-structure query-record))
        nil)))
