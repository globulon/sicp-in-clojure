(ns sicp.complex
  ^{:doc "Basically answer too exercise 2.77"}
  (:use sicp.generic-arithmetic))
(def complex-type :complex)

(defn make-complex [data]
  (tagged data complex-type))

(defprotocol ComplexSelector
  (magnitude [this])
  (angle [this])
  (real-part [this])
  (imag-part [this]))

(defprotocol ComplexConstructor
  (make-from-real-imag-constructor [self x y])
  (make-from-mag-ang-constructor [self r t]))

(defmulti make-from-real-imag (fn [x y] :real-imag-constructor))
(defmulti make-from-mag-ang (fn [r theta] :mag-ang-constructor))

(defn- add-complex [z1 z2]
  (make-from-real-imag (+ (real-part z1) (real-part z2))
    (+ (imag-part z1) (imag-part z2))))

(defn- sub-complex [z1 z2]
  (make-from-real-imag (- (real-part z1) (real-part z2))
    (- (imag-part z1) (imag-part z2))))

(defn- mul-complex [z1 z2]
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
    (+ (angle z1) (angle z2))))

(defn- div-complex [z1 z2]
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
    (- (angle z1) (angle z2))))


(defmethod add [:complex :complex] [z1 z2]
  (add-complex z1 z2))

(defmethod sub [:complex :complex] [z1 z2]
  (sub-complex z1 z2))

(defmethod div [:complex :complex] [z1 z2]
  (div-complex z1 z2))

(defmethod mul [:complex :complex] [z1 z2]
  (mul-complex z1 z2))

