(ns sicp.test.complex-rectangular-spec
  (:use [sicp complex complex-rectangular])
  (:use clojure.test))

(deftest complex-rectangular-should-bind-magnitude
  (is (= 5.0 (magnitude (->Rectangular 3 4)))))

(deftest complex-rectangular-should-bind-angle
  (is (= (/ Math/PI 4) (angle (->Rectangular 1 1)))))

(deftest complex-rectangular-should-bind-real-part
  (is (= 1 (real-part (->Rectangular 1 2)))))

(deftest complex-rectangular-should-bind-real-part
  (is (= 2 (imag-part (->Rectangular 1 2)))))

