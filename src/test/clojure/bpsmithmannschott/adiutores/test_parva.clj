(ns bpsmithmannschott.adiutores.test-parva
  (:use clojure.test)
  (:require [bpsmithmannschott.adiutores.parva :as parva]))

(deftest test-dofor
  (testing "dofor is a synonym for doseq"
    (is (= (with-out-str (doseq [x nil] (print x)))
           (with-out-str (parva/dofor [x nil] (print x)))))
    (is (= (with-out-str (doseq [x [1 2 3]] (print x)))
           (with-out-str (parva/dofor [x [1 2 3]] (print x)))))))

(deftest test-domap
  (is (= "A CB D"
         (with-out-str (parva/domap print [\A \B] [\C \D])))))

(deftest test-forcat
  (is (= [0 0 1 0 1 2] (parva/forcat [x (range 4)] (range x)))))

(deftest test-let1
  (is (= 1 (parva/let1 x 1 x)))
  (is (nil? (parva/let1 x :anything))))

(deftest thread-with
  (is (= [1 2 3] (parva/thread-with x [2] (conj x 3) (cons 1 x)))))



