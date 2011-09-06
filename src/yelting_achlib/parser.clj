(ns yelting-achlib.parser
  (:import (java.io BufferedReader FileReader)))

(def one-format '({:name :record-type :length 1 :padding-side :none :padding :none :type :any}
                  {:name :priority-code :length 2 :padding-side :none :padding :none :type :any}
                  {:name :destination :length 10 :padding-side :none :padding :none :type :any}
                  {:name :origin :length 10 :padding-side :none :padding :none :type :any}
                  {:name :creation-date :length 6 :padding-side :none :padding :none :type :any}
                  {:name :creation-time :length 4 :padding-side :none :padding :none :type :any}
                  {:name :file-id-modifier :length 1 :padding-side :none :padding :none :type :any}
                  {:name :record-size :length 3 :padding-side :none :padding :none :type :any}
                  {:name :blocking-factor :length 2 :padding-side :none :padding :none :type :any}
                  {:name :format-code :length 1 :padding-side :none :padding :none :type :any}
                  {:name :destination-name :length 23 :padding-side :right :padding \space :type :any}
                  {:name :origin-name :length 23 :padding-side :right :padding \space :type :any}
                  {:name :reference-code :length 8 :padding-side :none :padding :none :type :any}))

(def five-format '({:name :record-type :length 1 :padding-side :none :padding :none :type :any}
                   {:name :service-class-code :length 3 :padding-side :none :padding :none :type :any}
                   {:name :name :length 16 :padding-side :right :padding \space :type :any}
                   {:name :discretionary :length 20 :padding-side :none :padding :none :type :any}
                   {:name :identification :length 10 :padding-side :none :padding :none :type :any}
                   {:name :sec-code :length 3 :padding-side :none :padding :none :type :any}
                   {:name :entry-description :length 10 :padding-side :right :padding \space :type :any}
                   {:name :descriptive-date :length 6 :padding-side :none :padding :none :type :any}
                   {:name :effective-date :length 6 :padding-side :none :padding :none :type :any}
                   {:name :settlement-date :length 3 :padding-side :none :padding :none :type :any}
                   {:name :originator-status :length 1 :padding-side :none :padding :none :type :any}
                   {:name :odfi-routing-number :length 8 :padding-side :none :padding :none :type :any}
                   {:name :batch-number :length 7 :padding-side :left :padding \0 :type :numeric}))

(def six-format '({:name :record-type :length 1 :padding-side :none :padding :none :type :any}
                  {:name :tran-code :length 2 :padding-side :none :padding :none :type :any}
                  {:name :routing-number :length 9 :padding-side :none :padding :none :type :any}
                  {:name :account-number :length 17 :padding-side :right :padding \space :type :any}
                  {:name :amount :length 10 :padding-side :left :padding \0 :type :numeric}
                  {:name :identification :length 15 :padding-side :right :padding \space :type :any}
                  {:name :name :length 22 :padding-side :right :padding \space :type :any}
                  {:name :discretionary :length 2 :padding-side :none :padding :none :type :any}
                  {:name :addenda-indicator :length 1 :padding-side :none :padding :none :type :any}
                  {:name :trace-number :length 15 :padding-side :none :padding :none :type :any}))

(def seven-format '({:name :record-type :length 1 :padding-side :none :padding :none :type :any}
                    {:name :addenda-type :length 2 :padding-side :none :padding :none :type :any}
                    {:name :payment-data :length 80 :padding-side :right :padding \space :type :any}
                    {:name :addenda-number :length 4 :padding-side :left :padding \0 :type :numeric}
                    {:name :seq-number :length 7 :padding-side :left :padding \0 :type :numeric}))

(def eight-format '({:name :record-type :length 1 :padding-side :none :padding :none :type :any}
                    {:name :service-class-code :length 3 :padding-side :none :padding :none :type :any}
                    {:name :record-count :length 6 :padding-side :left :padding \0 :type :numeric}
                    {:name :entry-hash :length 10 :padding-side :left :padding \0 :type :numeric}
                    {:name :total-debit :length 12 :padding-side :left :padding \0 :type :numeric}
                    {:name :total-credit :length 12 :padding-side :left :padding \0 :type :numeric}
                    {:name :identification :length 10 :padding-side :none :padding :none :type :any}
                    {:name :authentication :length 19 :padding-side :none :padding :none :type :any}
                    {:name :reserved :length 6 :padding-side :none :padding :none :type :any}
                    {:name :odfi-routing-number :length 8 :padding-side :none :padding :none :type :any}
                    {:name :batch-number :length 7 :padding-side :left :padding \0 :type :numeric}))

(def nine-format '({:name :record-type :length 1 :padding-side :none :padding :none :type :any}
                   {:name :batch-count :length 6 :padding-side :left :padding \0 :type :numeric}
                   {:name :block-count :length 6 :padding-side :left :padding \0 :type :numeric}
                   {:name :record-count :length 8 :padding-side :left :padding \0 :type :numeric}
                   {:name :entry-hash :length 10 :padding-side :left :padding \0 :type :numeric}
                   {:name :total-debit :length 12 :padding-side :left :padding \0 :type :numeric}
                   {:name :total-credit :length 12 :padding-side :left :padding \0 :type :numeric}
                   {:name :reserved :length 39 :padding-side :none :padding :none :type :any}))

(defmacro is-padding? [padding]
  `(fn [character#] (= character# ~padding)))

(defn- apply-padding [value padding-side padding]
  (cond
    (= padding-side :none) value
    (= padding-side :right) (reverse (drop-while (is-padding? padding) (reverse value)))
    (= padding-side :left) (drop-while (fn [item] (= item padding)) value)))

(defn- apply-format [value padding-side padding type]
  (let [padded (apply-padding value padding-side padding)]
    (if (and (empty? padded) (= type :numeric))
      [\0]
      padded)))

(defn- record-to-map [record formats]
  (if (empty? formats)
    {}
    (let [{name :name length :length padding-side :padding-side padding :padding type :type} (first formats)]
      (assoc (record-to-map (drop length record) (rest formats))
             name (apply str (apply-format (take length record) padding-side padding type))))))

(defn parse-seven [record]
  (if (= \7 (first record))
    (record-to-map record seven-format)
    nil))

(defn parse-six [record]
  (if (= \6 (first record))
    (record-to-map record six-format)
    nil))

(defn parse-five [record]
  (if (= \5 (first record))
    (record-to-map record five-format)
    nil))

(defn parse-eight [record]
  (if (= \8 (first record))
    (record-to-map record eight-format)
    nil))

(defn parse-one [record]
  (if (= \1 (first record))
    (record-to-map record one-format)
    nil))

(defn parse-nine [record]
  (if (= \9 (first record))
    (record-to-map record nine-format)
    nil))

(defn parse-detail [records]
  (let [detail (parse-six (first records))
        remaining (rest records)]
    (if (= "1" (:addenda-indicator detail))
      (cons (assoc detail :addenda (list (parse-seven (first remaining)))) (rest remaining))
      (conj remaining detail))))

(defn parse-batch [records]
  (let [header (parse-five (first records))]
    (loop [details []
           remaining (rest records)]
      (cond
        (empty? remaining)
          nil
        (= \8 (first (first remaining)))
          (cons {:header header :details details :control (parse-eight (first remaining))} (rest remaining))
        :default
          (let [[detail & unprocessed] (parse-detail remaining)]
            (recur (conj details detail) unprocessed))))))

(defn parse-file [records]
  (let [header (parse-one (first records))]
    (loop [batches []
           remaining (rest records)]
      (cond
        (empty? remaining)
          nil
        (= \9 (first (first remaining)))
          (cons {:header header :batches batches :control (parse-nine (first remaining))} (rest remaining))
        :default
          (let [[batch & unprocessed] (parse-batch remaining)]
            (recur (conj batches batch) unprocessed))))))

(defn parse-ach-file [file-path]
  (with-open [reader (BufferedReader. (FileReader. file-path))]
    (doall (parse-file (line-seq reader)))))