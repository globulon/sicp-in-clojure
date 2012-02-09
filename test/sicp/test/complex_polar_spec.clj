(ns sicp.test.complex-polar-spec
  (:use [sicp complex complex-polar])
  (:use clojure.test))

(deftest complex-polar-should-bind-magnitude
  (is (= 1 (magnitude (->Polar 1 2)))))

(deftest complex-polar-should-bind-angle
  (is (= 2 (angle (->Polar 1 2)))))

(deftest complex-polar-should-bind-real-part
  (is (= 1 (real-part (->Polar 1 0))))
  (is (= 0 (imag-part (->Polar 1 0)))))

(deftest complex-polar-should-bind-real-part
  (is (< 0.0001 (Math/abs  (- 1.0 (imag-part (->Polar 1 Math/PI))))))
  (is (< 0.0001 (Math/abs  (real-part (->Polar 1 Math/PI))))))

;(run-tests)