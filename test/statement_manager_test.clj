(ns statement-manager-test
  (:require [clojure.test :refer :all]
            [statement-manager :refer :all]))
            

(deftest statement-manager-create-record-test
  (testing "create-clause-record mujer(maria) should be Fact with predicate mujer and parameters '(maria)"
    (is (= (create-clause-record "mujer(maria).")
           (->Fact "mujer" '("maria")))))
  (testing "create-clause-record padre(maria,lucas) should be Fact with predicate padre and parameters '(maria lucas)"
    (is (= (create-clause-record "padre(maria,lucas).")
           (->Fact "padre" '("maria" "lucas")))))
  (testing "create-clause-record subtract(X,Y,Z):-add(Y,Z,X) should be Rule with predicate subtract, variables '(X Y Z) and objectives '(add(Y,Z,X))"
    (is (= (create-clause-record "subtract(X,Y,Z):-add(Y,Z,X).")
           (->Rule "subtract" '("X" "Y" "Z") '("add(Y,Z,X)")))))
  (testing "create-clause-record hijo(X,Y):-varon(X),padre(Y,X) should be Rule with predicate hijo, variables '(X Y) and objectives '(varon(X) padre(Y,X))"
    (is (= (create-clause-record "hijo(X,Y):-varon(X),padre(Y,X).")
           (->Rule "hijo" '("X" "Y") '("varon(X)" "padre(Y,X)")))))
  (testing "create-query-record mujer(maria) should be Query with predicate mujer and parameters '(maria)"
    (is (= (create-query-record "mujer(maria)")
           (->Query "mujer" '("maria")))))
  (testing "create-query-record padre(maria,lucas) should be Query with predicate padre and parameters '(maria lucas)"
    (is (= (create-query-record "padre(maria,lucas)")
           (->Query "padre" '("maria" "lucas")))))
  (testing "same-predicate? (->Fact mujer '(maria)) (->Fact mujer '(ana)) should be true"
    (is (= (same-predicate? (->Fact "mujer" '("maria")) (->Fact "mujer" '("ana")))
           true)))
  (testing "same-predicate? (->Fact varon '(pedro)) (->Fact varon '(pedro)) should be true"
    (is (= (same-predicate? (->Fact "varon" '("pedro")) (->Fact "varon" '("pedro")))
           true)))
  (testing "same-predicate? (->Fact varon '(pedro)) (->Fact mujer '(maria)) should be false"
    (is (= (same-predicate? (->Fact "varon" '("pedro")) (->Fact "mujer" '("maria")))
           false)))
  (testing "equal-number-of-parameters? (->Rule subtract '(X Y Z) '(add(Y,Z,X))) (->Query varon '(pedro)) should be false"
    (is (= (equal-number-of-parameters? (->Rule "subtract" '("X" "Y" "Z") '("add(Y,Z,X)")) (->Query "varon" '("pedro")))
           false)))
  (testing "equal-number-of-parameters? (->Rule hijo '(X Y) '(varon(X) padre(Y,X))) (->Query varon '(pedro)) should be true"
    (is (= (equal-number-of-parameters? (->Rule "hijo" '("X" "Y") '("varon(X)" "padre(Y,X)")) (->Query "padre" '("maria" "lucas")))
           true)))
  (testing "equal-number-of-parameters? (->Rule hijo '(X Y) '(varon(X) padre(Y,X))) (->Query hijo '(pedro,juan)) should be true"
    (is (= (equal-number-of-parameters? (->Rule "hijo" '("X" "Y") '("varon(X)" "padre(Y,X)")) (->Query "hijo" '("pedro" "juan")))
           true)))
  (testing "create-database-structure test"
    (is (= (create-database-structure '("padre(maria,lucas)." "varon(pepe)." "hijo(X,Y):-varon(X),padre(Y,X)."))
           '(#statement_manager.Fact{:predicate "padre", :parameters ["maria" "lucas"]} #statement_manager.Fact{:predicate "varon", :parameters ["pepe"]} #statement_manager.Rule{:predicate "hijo", :variables ["X" "Y"], :objectives ["varon(X)" "padre(Y,X)"]})))))
           

(def database (create-database-structure '("padre(maria,lucas)." "varon(pepe)." "hijo(X,Y):-varon(X),padre(Y,X)." "varon(juan)." "mujer(claudia).")))
(def database-2 (create-database-structure '("padre(pepe,lucas)." "varon(pepe)." "hijo(X,Y):-varon(X),padre(Y,X)." "varon(juan)." "mujer(claudia)." "varon(lucas).")))
(def database-3 (create-database-structure '("add(one,one,two)." "add(one,two,zero)." "add(two,zero,two)." "add(two,one,zero)." "add(two,two,one)." "subtract(X,Y,Z):-add(Y,Z,X).")))

(deftest statement-manager-query-test
  (testing "filter-by-query-predicate database (->Query mujer (maria)) should be (#statement_manager.Fact{:predicate mujer, :parameters [claudia]})"
    (is (= (filter-by-query-predicate database (->Query "mujer" '("maria")))
           '(#statement_manager.Fact{:predicate "mujer", :parameters ["claudia"]}))))
  (testing "filter-by-query-predicate database (->Query varon (pepe)) should be (#statement_manager.Fact{:predicate varon, :parameters [pepe]} #statement_manager.Fact{:predicate varon, :parameters [juan]})"
    (is (= (filter-by-query-predicate database (->Query "varon" '("pepe")))
           '(#statement_manager.Fact{:predicate "varon", :parameters ["pepe"]} #statement_manager.Fact{:predicate "varon", :parameters ["juan"]}))))
  (testing "filter-by-query-predicate database (->Query hijo (pepe,alberto)) should be (#statement_manager.Rule{:predicate hijo, :variables [X Y], :objectives [varon(X) padre(Y,X)]})"
    (is (= (filter-by-query-predicate database (->Query "hijo" '("pepe" "juan")))
           '(#statement_manager.Rule{:predicate "hijo", :variables ["X" "Y"], :objectives ["varon(X)" "padre(Y,X)"]}))))
  (testing "filter-by-query-predicate database (->Query adolescente (pepe)) should be ()"
    (is (= (filter-by-query-predicate database (->Query "adolescente" '("pepe")))
           '())))
  (testing "process-query database (->Query varon (pepe)) should be (true false)"
    (is (= (process-query database (->Query "varon" '("pepe")))
           '(true false))))
  (testing "process-query database (->Query varon (lucas)) should be (false false)"
    (is (= (process-query database (->Query "varon" '("lucas")))
           '(false false))))
  (testing "process-query database (->Query hijo (pepe juan)) should be (false)"
    (is (= (process-query database (->Query "hijo" '("pepe" "juan")))
           '(false))))
  (testing "process-query database-2 (->Query hijo (lucas pepe)) should be (true)"
    (is (= (process-query database-2 (->Query "hijo" '("lucas" "pepe")))
           '(true))))
  (testing "process-query database-2 (->Query adolescente (lucas)) should be (false)"
    (is (= (process-query database-2 (->Query "adolescente" '("lucas")))
           '(false))))
  (testing "respond-to-query database-2 (->Query varon (lucas)) should be true"
    (is (= (respond-to-query database-2 (->Query "varon" '("lucas")))
           true)))
  (testing "respond-to-query database-2 (->Query hijo (lucas pepe)) should be true"
    (is (= (respond-to-query database-2 (->Query "hijo" '("lucas" "pepe")))
           true)))
  (testing "respond-to-query database-2 (->Query amigo (lucas pepe)) should be false"
    (is (= (respond-to-query database-2 (->Query "amigo" '("lucas" "pepe")))
           false)))
  (testing "respond-to-query database (->Query varon (pepe)) should be true"
    (is (= (respond-to-query database (->Query "varon" '("pepe")))
           true)))
  (testing "respond-to-query database (->Query varon (lucas)) should be false"
    (is (= (respond-to-query database (->Query "varon" '("lucas")))
           false)))
  (testing "reconstruct-objective varon(X) (X) (lucas) should be (->Query varon (lucas))"
    (is (= (reconstruct-objective "varon(X)" '("X") '("lucas"))
           (->Query "varon" '("lucas")))))
  (testing "reconstruct-objective padre(Y,X) (X,Y) (lucas pepe) should be (->Query padre (lucas pepe))"
    (is (= (reconstruct-objective "padre(Y,X)" '("X" "Y") '("lucas" "pepe"))
           (->Query "padre" '("pepe" "lucas")))))
  (testing "reconstruct-objective varon(X) (X,Y) (lucas pepe) should be (->Query varon (lucas))"
    (is (= (reconstruct-objective "varon(X)" '("X" "Y") '("lucas" "pepe"))
           (->Query "varon" '("lucas")))))
  (testing "reconstruct-objective amigos(X,ana) (X,Y) (lucas pepe) should be (->Query amigos (lucas ana))"
    (is (= (reconstruct-objective "amigos(X,ana)" '("X" "Y") '("lucas" "pepe"))
           (->Query "amigos" '("lucas" "ana")))))
  (testing "respond-objectives (->Rule hijo '(X Y) '(varon(X) padre(Y,X))) (->Query hijo '(lucas maria)) should be (false true)"
    (is (= (respond-objectives (->Rule "hijo" '("X" "Y") '("varon(X)" "padre(Y,X)")) (->Query "hijo" '("lucas" "maria")) database)
           '(false true))))
  (testing "respond-objectives (->Rule hijo '(X Y) '(varon(X) padre(Y,X))) (->Query hijo '(lucas juan)) should be (false false)"
    (is (= (respond-objectives (->Rule "hijo" '("X" "Y") '("varon(X)" "padre(Y,X)")) (->Query "hijo" '("lucas" "juan")) database)
           '(false false))))
  (testing "respond-objectives (->Rule subtract '(X Y Z) '(add(Y,Z,X))) (->Query subtract '(two one one)) should be (true)"
    (is (= (respond-objectives (->Rule "subtract" '("X" "Y" "Z") '("add(Y,Z,X)")) (->Query "subtract" '("two" "one" "one")) database-3)
           '(true))))
  (testing "respond-objectives (->Rule subtract (X Y Z) (add(Y,Z,X))) (->Query subtract '(two one one)) should be (true)"
    (is (= (respond-objectives (->Rule "subtract" '("X" "Y" "Z") '("add(Y,Z,X)")) (->Query "subtract" '("two" "one" "one")) database-3)
           '(true))))
  (testing "respond-objectives (->Rule subtract (X Y Z) (add(Y,Z,X))) (->Query subtract '(two one three)) should be (false)"
    (is (= (respond-objectives (->Rule "subtract" '("X" "Y" "Z") '("add(Y,Z,X)")) (->Query "subtract" '("two" "one" "three")) database-3)
           '(false))))
  (testing "fact: respond-query (->Fact mujer (claudia)) (->Query mujer (claudia)) database should be true"
    (is (= (respond-query (->Fact "mujer" '("claudia")) (->Query "mujer" '("claudia")) database)
           true)))
  (testing "fact: respond-query (->Fact mujer (claudia)) (->Query mujer (romina)) database should be false"
    (is (= (respond-query (->Fact "mujer" '("claudia")) (->Query "mujer" '("romina")) database)
           false)))
  (testing "fact: respond-query (->Fact padre (maria lucas)) (->Query padre (maria lucas)) database should be true"
    (is (= (respond-query (->Fact "padre" '("maria" "lucas")) (->Query "padre" '("maria" "lucas")) database)
           true)))
  (testing "fact: respond-query (->Fact padre (maria lucas)) (->Query padre (maria lucas)) database should be false"
    (is (= (respond-query (->Fact "padre" '("maria" "lucas")) (->Query "padre" '("maria" "luis")) database)
           false)))
  (testing "rule: respond-query (->Rule subtract (X Y Z) (add(Y,Z,X))) (->Query subtract '(two one one)) database-3 should be true"
    (is (= (respond-query (->Rule "subtract" '("X" "Y" "Z") '("add(Y,Z,X)")) (->Query "subtract" '("two" "one" "one")) database-3)
           true)))
  (testing "rule: respond-query (->Rule subtract (X Y Z) (add(Y,Z,X))) (->Query subtract '(two one one)) database-3 should be false"
    (is (= (respond-query (->Rule "subtract" '("X" "Y" "Z") '("add(Y,Z,X)")) (->Query "subtract" '("two" "one" "three")) database-3)
           false)))
  (testing "rule: respond-query (->Rule hijo (X Y) (varon(X) padre(Y,X))) (->Query hijo '(lucas juan)) database should be false"
    (is (= (respond-query (->Rule "hijo" '("X" "Y") '("varon(X)" "padre(Y,X)")) (->Query "hijo" '("lucas" "juan")) database)
           false)))
  (testing "rule: respond-query (->Rule hijo (X Y) (varon(X) padre(Y,X))) (->Query hijo '(lucas maria)) database should be false"
    (is (= (respond-query (->Rule "hijo" '("X" "Y") '("varon(X)" "padre(Y,X)")) (->Query "hijo" '("lucas" "maria")) database)
           false)))
)
           

