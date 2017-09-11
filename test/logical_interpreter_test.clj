(ns logical-interpreter-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))
            

(def parent-database "
	varon(juan).
	varon(pepe).
	padre(juan, pepe).
	padre(juan, pepa).
	mujer(pepa).
	mujer(celia).
	hijo(X, Y) :- varon(X), padre(Y, X).
	hija(X, Y) :- mujer(X), padre(Y, X).
	nieto(X, Y) :- varon(X), abuelo(Y, X).
	padre-de-ana(juan).
	hija-de-juan(X) :- mujer(X), padre(juan,X).
")
    
(def incomplete-database-1 "
	varon(juan)
	varon(pepe).
")     

(def incomplete-database-2 "
	varon(juan).
	varon().
")     

(def incomplete-database-3 "
	varon(juan).
	(pepe).
")     

(def wrong-database-1 "
	varon(juan).
	VARON(pepe).
")

(def wrong-database-2 "
	varon(juan).
	varon(X).
")  

(deftest logical-interpreter-entry-test
  (testing "varon(juan) should be nil"
    (is (= (evaluate-query incomplete-database-1 "varon(juan)")
           nil)))
  (testing "varon(juan) should be nil"
    (is (= (evaluate-query incomplete-database-2 "varon(juan)")
           nil))) 
  (testing "varon(juan) should be nil"
    (is (= (evaluate-query wrong-database-1 "varon(juan)")
           nil))) 
  (testing "varon(juan) should be nil"
    (is (= (evaluate-query wrong-database-2 "varon(juan)")
           nil))) 
  (testing "varon(juan) should be nil"
    (is (= (evaluate-query parent-database "(juan)")
           nil)))
  (testing "varon(juan) should be nil"
    (is (= (evaluate-query parent-database "varon(juan,)")
           nil)))
  (testing "varon(juan) should be nil"
    (is (= (evaluate-query parent-database "V(juan)")
           nil)))
  (testing "varon(juan) should be nil"
    (is (= (evaluate-query parent-database "varon(,juan)")
           nil)))
  (testing "-(juan) should be nil"
    (is (= (evaluate-query parent-database "-(juan)")
           nil))))
           
(deftest logical-interpreter-query-test
  (testing "varon(juan) should be true"
    (is (= (evaluate-query parent-database "varon(juan)")
           true)))
  (testing "hombre(juan) should be false"
    (is (= (evaluate-query parent-database "hombre(juan)")
           false)))
  (testing "padre(juan, pepe) should be true"
    (is (= (evaluate-query parent-database "padre(juan, pepe)")
           true)))
  (testing "padre(juan,pepe) should be true"
    (is (= (evaluate-query parent-database "padre(juan,pepe)")
           true)))
  (testing "amigos(juan, pedro) should be false"
    (is (= (evaluate-query parent-database "amigos(juan, pedro)")
           false)))
  (testing "hijo(pepe, juan) should be true"
    (is (= (evaluate-query parent-database "hijo(pepe, juan)")
           true)))
  (testing "hija(maria, juan) should be false"
    (is (= (evaluate-query parent-database "hija(maria, juan)")
           false)))
  (testing "hija(pepa, juan) should be true"
    (is (= (evaluate-query parent-database "hija(pepa, juan)")
           true)))
  (testing "nieto(pepe, juan) should be false"
    (is (= (evaluate-query parent-database "nieto(pepe, juan)")
           false)))
  (testing "padre-de-ana(juan) should be true"
    (is (= (evaluate-query parent-database "padre-de-ana(juan)")
           true)))
  (testing "padre-de-ana(juan) should be true"
    (is (= (evaluate-query parent-database "hija-de-juan(pepa)")
           true)))
  (testing "padre-de-ana(juan) should be true"
    (is (= (evaluate-query parent-database "hija-de-juan(celia)")
           false))))           
           
