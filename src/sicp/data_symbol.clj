(ns sicp.data-symbol)

(defn variable? [expression]
                                  (symbol? expression))

    (defn sum? [expression]
      (= '+ (first expression)))

    (defn product? [expression]
      (= '* (first expression)))

    (defn exponentiation? [expression]
      (= '** (first expression)))


    (defn same-variable? [expression variable]
      (and (variable? expression) (= expression variable)))

    (defn all-numbers? [in-list]
      (letfn [
               (iter [still-number remaining]
                 (cond
                   (not still-number) still-number
                   (empty? remaining) true
                   :else (recur
                           (and still-number (number? (first remaining)))
                           (rest remaining))))]
        (iter true in-list)))

    (defn make-sum
      ([x y] (make-sum [x y]))
      ([[_ :as expression]]
        (cond
          (= 1 (count expression)) (first expression)
          (all-numbers? expression) (apply + expression)
          (:simplified (meta expression)) (cons '+ expression)
          :else (make-sum
                  (with-meta
                    (filter (fn [x] (not= 0 x)) expression)
                    {:simplified true})))))

    (defn addend [expression]
      {:pre [(>= (count expression) 3)]}
      (second expression))

    (defn augend [[op _ & cdr :as expression]]
      {:pre [(>= (count expression) 3)]}
      (make-sum cdr))

    (defn make-product
      ([x y] (make-product [x y]))
      ([[_ :as expression]]
        (cond
          (= 1 (count expression)) (first expression)
          (> (count (filter (fn [x] (= 0 x)) expression)) 0) 0
          (all-numbers? expression) (apply * expression)
          (:simplified (meta expression)) (cons '* expression)
          :else (recur
                  (with-meta
                    (filter (fn [x] (not (= 1 x))) expression)
                    {:simplified true})))))

    (defn multiplier [expression]
      {:pre [(>= (count expression) 3)]}
      (second expression))

    (defn multiplicand [[op _ & cdr :as expression]]
      {:pre [(>= (count expression) 3)]}
      (make-product cdr))

    (defn make-exponentiation [base exponent]
      (cond
        (= 0 exponent) 1
        (= 1 exponent) base
        (and (number? base) (= 1 base)) 1
        (and (number? base) (= 0 base)) 0
        :else (list '** base exponent)))

    (defn base [from-exponentiation]
      {:pre [(= 3 (count from-exponentiation))]}
      (second from-exponentiation))

    (defn exponent [from-exponentiation]
      {:pre [(= 3 (count from-exponentiation))]}
      (second (rest from-exponentiation)))


    (defn deriv [expression variable]
      (println expression " derived by " variable)
      (cond
        (number? expression)
        0
        (variable? expression)
        (if (same-variable? expression variable) 1 0)
        (sum? expression)
        (make-sum
          (deriv (addend expression) variable)
          (deriv (augend expression) variable))
        (product? expression)
        (make-sum
          (make-product
            (multiplier expression)
            (deriv (multiplicand expression) variable))
          (make-product
            (deriv (multiplier expression) variable)
            (multiplicand expression)))
        (exponentiation? expression)
    (do
      (make-product
        (make-product
          (exponent expression)
          (make-exponentiation (base expression) (dec (exponent expression))))
        (deriv (base expression) variable)))
    :else (println "Invalid expression")))
