(ns parser-test
  (:require [clojure.test :refer :all]
            [parser :refer :all]))
   
   
(def parent-database "
	varon(juan).
	varon(pepe).
	mujer(maria).
	mujer(cecilia).
	padre(juan, pepe).
	padre(juan, pepa).
	hijo(X, Y) :- varon(X), padre(Y, X).
	hija(X, Y) :- mujer(X), padre(Y, X).
")
         

(deftest parser-cleaning-test
  (testing "Obtaining database lines"
    (is (= (obtain-database-lines parent-database) 
        '("\tvaron(juan)." "\tvaron(pepe)." "\tmujer(maria)." "\tmujer(cecilia)." "\tpadre(juan, pepe)." "\tpadre(juan, pepa)." "\thijo(X, Y) :- varon(X), padre(Y, X)." "\thija(X, Y) :- mujer(X), padre(Y, X)."))))
    (testing "Removing spaces"
    (is (= (remove-spaces "hijo(X, Y) :- varon(X), padre(Y, X).") 
           "hijo(X,Y):-varon(X),padre(Y,X).")))
    (testing "Removing tabs and spaces"
    (is (= (map remove-tabs-and-spaces (obtain-database-lines parent-database)) 
           '("varon(juan)." "varon(pepe)." "mujer(maria)." "mujer(cecilia)." "padre(juan,pepe)." "padre(juan,pepa)." "hijo(X,Y):-varon(X),padre(Y,X)." "hija(X,Y):-mujer(X),padre(Y,X).")))))
           
(def clean-database (map remove-tabs-and-spaces (obtain-database-lines parent-database)))

(deftest parser-validation-test   
    (testing "rule? hijo(X,Y):-varon(X),padre(Y,X). should be true"
    (is (= (rule? "hijo(X,Y):-varon(X),padre(Y,X).") 
           true)))
    (testing "rule? mujer(maria). should be false"
    (is (= (rule? "mujer(maria).") 
           false)))
    (testing "rule? padre(juan,pepe). should be false"
    (is (= (rule? "padre(juan,pepe).") 
           false)))
    (testing "fact: valid-clause? padre(juan,pepe). should be true"
    (is (= (valid-clause? "padre(juan,pepe).") 
           true)))
    (testing "fact: valid-clause? padre(juan,pepe) should be false"
    (is (= (valid-clause? "padre(juan,pepe)") 
           false)))
    (testing "fact: valid-clause? padre(X,pepe). should be false"
    (is (= (valid-clause? "padre(X,pepe).") 
           false)))
    (testing "fact: valid-clause? suma(2,3). should be true"
    (is (= (valid-clause? "suma(2,3).") 
           true)))
    (testing "fact: valid-clause? (juan). should be false"
    (is (= (valid-clause? "(juan).") 
           false)))
    (testing "fact: valid-clause? varon(juan,). should be false"
    (is (= (valid-clause? "varon(juan,).") 
           false)))
    (testing "fact: valid-clause? varon. should be false"
    (is (= (valid-clause? "varon.") 
           false)))
    (testing "fact: valid-clause? varon(). should be false"
    (is (= (valid-clause? "varon().") 
           false)))
    (testing "fact: valid-clause? suma(,3). should be false"
    (is (= (valid-clause? "suma(,3).") 
           false)))
    (testing "fact: valid-clause? SUMA(2,3). should be false"
    (is (= (valid-clause? "SUMA(2,3).") 
           false)))
    (testing "rule: valid-clause? hijo(X,Y):-varon(X),padre(Y,X). should be true"
    (is (= (valid-clause? "hijo(X,Y):-varon(X),padre(Y,X).") 
           true)))
    (testing "rule: valid-clause? HIJO(X,Y):-varon(X),padre(Y,X). should be false"
    (is (= (valid-clause? "HIJO(X,Y):-varon(X),padre(Y,X).") 
           false)))
    (testing "rule: valid-clause? hijo(X,Y):-varon(X),padre(Y,X) should be false"
    (is (= (valid-clause? "hijo(X,Y):-varon(X),padre(Y,X)") 
           false)))
    (testing "rule: valid-clause? hijo(X,Y):-,padre(Y,X). should be false"
    (is (= (valid-clause? "hijo(X,Y):-,padre(Y,X).") 
           false)))
    (testing "rule: valid-clause? hijo(pepe,Y):-varon(pepe),padre(Y,pepe). should be false"
    (is (= (valid-clause? "hijo(pepe,Y):-varon(pepe),padre(Y,pepe).") 
           false)))
    (testing "rule: valid-clause? hijo():-varon(X),padre(Y,X). should be false"
    (is (= (valid-clause? "hijo():-varon(X),padre(Y,X).") 
           false)))
    (testing "rule: valid-clause? hijo:-varon(X),padre(Y,X). should be false"
    (is (= (valid-clause? "hijo:-varon(X),padre(Y,X).") 
           false)))
    (testing "rule: valid-clause? hijo(X,Y):-(X),padre(Y,X). should be false"
    (is (= (valid-clause? "hijo(X,Y):-(X),padre(Y,X).") 
           false)))
    (testing "rule: valid-clause? subtract(X,Y,Z):-add(Y,Z,X). should be true"
    (is (= (valid-clause? "subtract(X,Y,Z):-add(Y,Z,X).") 
           true)))
    (testing "rule: valid-clause? subtract(X,Y,):-add(Y,Z,X). should be false"
    (is (= (valid-clause? "subtract(X,Y,):-add(Y,Z,X).") 
           false)))
    (testing "rule: valid-clause? subtract(X,Y,Z):-add(Y,Z,). should be false"
    (is (= (valid-clause? "subtract(X,Y,Z):-add(Y,Z,).") 
           false)))
    (testing "rule: valid-clause? (X,Y,Z):-add(Y,Z,X). should be false"
    (is (= (valid-clause? "(X,Y,Z):-add(Y,Z,X).") 
           false)))
    (testing "rule: valid-clause? subtract(X,Y,Z):-(Y,Z,X). should be false"
    (is (= (valid-clause? "subtract(X,Y,Z):-(Y,Z,X).") 
           false)))
    (testing "valid-database? clean-database should be true"
    (is (= (valid-database? clean-database) 
           true)))
    (testing "valid-database? '(varon(juan). varon(pepe). mujer(maria)) should be false"
    (is (= (valid-database? '("varon(juan)." "varon(pepe)." "mujer(maria)")) 
           false)))
    (testing "valid-database? '(varon(juan). valor(2). mujer(maria).) should be true"
    (is (= (valid-database? '("varon(juan)." "valor(2)." "mujer(maria).")) 
           true)))
    (testing "valid-database? '(varon(juan). (pepe). mujer(maria)) should be false"
    (is (= (valid-database? '("varon(juan)." "varon(pepe)." "mujer(maria)")) 
           false)))
    (testing "valid-entries? clean-database varon(pepe) should be true"
    (is (= (valid-entries? clean-database "varon(pepe)") 
           true)))
    (testing "valid-entries? clean-database varon() should be false"
    (is (= (valid-entries? clean-database "varon()") 
           false)))
    (testing "valid-entries? clean-database varon(PEPE) should be false"
    (is (= (valid-entries? clean-database "varon(PEPE)") 
           false)))
    (testing "valid-entries? clean-database (pepe) should be false"
    (is (= (valid-entries? clean-database "(pepe)") 
           false)))
    (testing "valid-entries? '(varon(juan). varon(pepe). mujer(maria)) varon(pepe) should be false"
    (is (= (valid-entries? '("varon(juan)." "varon(pepe)." "mujer(maria)") "varon(pepe)") 
           false)))
    (testing "valid-entries? '(varon(juan). varon(pepe). mujer(maria)) varon() should be false"
    (is (= (valid-entries? '("varon(juan)." "varon(pepe)." "mujer(maria)") "varon()") 
           false))))
           
(deftest parser-obtain-test
  (testing "obtain-clause-predicate varon(pepe) should be varon"
    (is (= (obtain-clause-predicate "varon(pepe).") 
        "varon")))
  (testing "obtain-clause-predicate hijo(X,Y):-varon(X),padre(Y,X) should be hijo"
    (is (= (obtain-clause-predicate "hijo(X,Y):-varon(X),padre(Y,X).") 
        "hijo")))
  (testing "obtain-rule-objectives hijo(X,Y):-varon(X),padre(Y,X) should be '(varon(X) padre(Y,X))"
  (is (= (obtain-rule-objectives "hijo(X,Y):-varon(X),padre(Y,X).") 
       '("varon(X)" "padre(Y,X)"))))
  (testing "obtain-rule-objectives subtract(X,Y,Z):-add(Y,Z,X). should be '(add(Y,Z,X))"
  (is (= (obtain-rule-objectives "subtract(X,Y,Z):-add(Y,Z,X).") 
       '("add(Y,Z,X)"))))
  (testing "obtain-rule-objectives hijo(X,Y):-varon(X),padre(Y,X),varon(Y). should be '(varon(X) padre(Y,X) varon(Y))"
  (is (= (obtain-rule-objectives "hijo(X,Y):-varon(X),padre(Y,X),varon(Y).") 
       '("varon(X)" "padre(Y,X)" "varon(Y)"))))
  (testing "rule: obtain-clause-parameters hijo(X,Y):-varon(X),padre(Y,X) should be '(X Y)"
  (is (= (obtain-clause-parameters "hijo(X,Y):-varon(X),padre(Y,X)." rule-variables-regex) 
       '("X" "Y"))))
  (testing "rule: obtain-clause-parameters subtract(X,Y,Z):-add(Y,Z,X). should be '(X Y Z)"
  (is (= (obtain-clause-parameters "subtract(X,Y,Z):-add(Y,Z,X)." rule-variables-regex) 
       '("X" "Y" "Z"))))
  (testing "fact: obtain-clause-parameters varon(pepe) should be '(pepe)"
  (is (= (obtain-clause-parameters "varon(pepe)." fact-parameters-regex) 
       '("pepe"))))
  (testing "fact: obtain-clause-parameters padre(maria,pepe) should be '(pepe)"
  (is (= (obtain-clause-parameters "padre(maria,pepe)." fact-parameters-regex) 
       '("maria" "pepe"))))
  (testing "query: obtain-clause-parameters varon(pepe) should be '(pepe)"
  (is (= (obtain-clause-parameters "varon(pepe)" query-parameters-regex) 
       '("pepe")))))
