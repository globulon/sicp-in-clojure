(ns sicp.records
  (:use sicp.tree))

(defrecord Record [record-key data])

(defn make-record [record-key data]
  (Record. record-key data))

(defn key-record [record]
  (:record-key record))

(defn data [record]
  (:data record))

(defn lookup [record-key records]
  (if (empty? records)
    false
    (let [current-key (key-record (entry records))]
      (cond
        (= record-key current-key) (data (entry records))
        (< record-key current-key) (recur record-key (left-branch records))
        (> record-key current-key) (recur record-key (right-branch records))))))

