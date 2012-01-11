(ns sicp.complex-polar
  (:use sicp.core)
  (:use sicp.complex))

(defrecord Polar [radius theta]
  ComplexSelector
  (magnitude [z] (:radius z))
  (angle [z] (:theta z))
  (real-part [z]
    (* (magnitude z) (Math/cos (angle z))))
  (imag-part [z]
    (* (magnitude z) (Math/sin (angle z)))))

(defn polar-complex []
  (reify ComplexConstructor
    (make-from-real-imag-constructor [self x y]
      (->Polar (Math/sqrt (+ (square x) (square y))) (Math/atan2 y x)))

    (make-from-mag-ang-constructor [self r t] (->Polar r t))))





