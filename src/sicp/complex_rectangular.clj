(ns sicp.complex-rectangular
  (:use sicp.core)
  (:use sicp.complex))

(defrecord Rectangular [x y]
  ComplexSelector
  (real-part [z] (:x z))
  (imag-part [z] (:y z))
  (magnitude [z] (Math/sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (angle [z] (Math/atan2 (imag-part z) (imag-part z))))

(defn rectangular-complex[]
  (reify ComplexConstructor
    (make-from-real-imag-constructor [self x y]
      (make-complex (->Rectangular x y)))

    (make-from-mag-ang-constructor [self r t]
      (make-complex (->Rectangular
                      (* r (Math/cos t))
                      (* r (Math/sin t)))))))

