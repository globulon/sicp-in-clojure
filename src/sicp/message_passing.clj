(ns sicp.message-passing
  (:use sicp.core)
  (:use sicp.complex))

(defn make-from-real-imaginary [x y]
  (reify ComplexSelector
    (real-part [z] (:x z))
    (imag-part [z] (:y z))
    (magnitude [z] (Math/sqrt (+ (square (real-part z)) (square (imag-part z)))))
    (angle [z] (Math/atan2 (imag-part z) (imag-part z)))))

(defn make-from-angle-magnitude [radius theta]
  (reify ComplexSelector
  (magnitude [z] radius)
  (angle [z] theta)
  (real-part [z]
    (* (magnitude z) (Math/cos (angle z))))
  (imag-part [z]
    (* (magnitude z) (Math/sin (angle z))))))



