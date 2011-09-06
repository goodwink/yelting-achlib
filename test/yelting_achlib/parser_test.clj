(ns yelting-achlib.parser-test
  (:use yelting-achlib.parser)
  (:use clojure.test))

(deftest test-seven-record
  (let [result (parse-seven "7051234567891123456789212345678931234567894123456789512345678961234567897123456789800010000001")]
    (is (= "7" (:record-type result)))
    (is (= "05" (:addenda-type result)))
    (is (= "1" (:addenda-number result)))
    (is (= "1" (:seq-number result)))))
    
(deftest test-six-record
  (let [result (parse-six "6220818100021722             0000021600243565         MIKE A WILLIAMS       020103112590000004")]
    (is (= "6" (:record-type result)))
    (is (= "22" (:tran-code result)))
    (is (= "081810002" (:routing-number result)))
    (is (= "1722" (:account-number result)))
    (is (= "21600" (:amount result)))
    (is (= "243565" (:identification result)))
    (is (= "MIKE A WILLIAMS" (:name result)))
    (is (= "02" (:discretionary result)))
    (is (= "0" (:addenda-indicator result)))
    (is (= "103112590000004" (:trace-number result)))))

(deftest test-five-record
  (let [result (parse-five "5220ABC TEST COMPANYPAYROLL             9000000625PPDPAYROLL   041587870415   1103112590000001")]
    (is (= "5" (:record-type result)))
    (is (= "220" (:service-class-code result)))
    (is (= "ABC TEST COMPANY" (:name result)))
    (is (= "PAYROLL             " (:discretionary result)))
    (is (= "9000000625" (:identification result)))
    (is (= "PPD" (:sec-code result)))
    (is (= "PAYROLL" (:entry-description result)))
    (is (= "041587" (:descriptive-date result)))
    (is (= "870415" (:effective-date result)))
    (is (= "   " (:settlement-date result)))
    (is (= "1" (:originator-status result)))
    (is (= "10311259" (:odfi-routing-number result)))
    (is (= "1" (:batch-number result)))))

(deftest test-eight-record
  (let [result (parse-eight "822000000600081903860000000000000000001519009000000625                         103112590000001")]
    (is (= "8" (:record-type result)))
    (is (= "220" (:service-class-code result)))
    (is (= "6" (:record-count result)))
    (is (= "8190386" (:entry-hash result)))
    (is (= "0" (:total-debit result)))
    (is (= "151900" (:total-credit result)))
    (is (= "9000000625" (:identification result)))
    (is (= "                   " (:authentication result)))
    (is (= "      " (:reserved result)))
    (is (= "10311259" (:odfi-routing-number result)))
    (is (= "1" (:batch-number result)))))

(deftest test-one-record
  (let [result (parse-one "101 101000048 1031125949504140050A094101FRB KANSAS CITY        RCB BANK CLAREMORE             ")]
    (is (= "1" (:record-type result)))
    (is (= "01" (:priority-code result)))
    (is (= " 101000048" (:destination result)))
    (is (= " 103112594" (:origin result)))
    (is (= "950414" (:creation-date result)))
    (is (= "0050" (:creation-time result)))
    (is (= "A" (:file-id-modifier result)))
    (is (= "094" (:record-size result)))
    (is (= "10" (:blocking-factor result)))
    (is (= "1" (:format-code result)))
    (is (= "FRB KANSAS CITY" (:destination-name result)))
    (is (= "RCB BANK CLAREMORE" (:origin-name result)))
    (is (= "        " (:reference-code result)))))

(deftest test-nine-record
  (let [result (parse-nine "9000002000003000000140040951930000000020201000000151900                                       ")]
    (is (= "9" (:record-type result)))
    (is (= "2" (:batch-count result)))
    (is (= "3" (:block-count result)))
    (is (= "14" (:record-count result)))
    (is (= "40951930" (:entry-hash result)))
    (is (= "20201" (:total-debit result)))
    (is (= "151900" (:total-credit result)))
    (is (= "                                       " (:reserved result)))))

          (deftest test-detail-addenda-not-indicated
  (let [result (parse-detail '("6220818100021722             0000021600243565         MIKE A WILLIAMS       020103112590000004"
                               "7051234567891123456789212345678931234567894123456789512345678961234567897123456789800010000001"))]
    (is (= nil (:addenda (first result))))
    (is (= "6" (:record-type (first result))))
    (is (= '("7051234567891123456789212345678931234567894123456789512345678961234567897123456789800010000001") (rest result)))))
    
(deftest test-detail-addenda-indicated
  (let [result (parse-detail '("6220818100021722             0000021600243565         MIKE A WILLIAMS       021103112590000004"
                               "7051234567891123456789212345678931234567894123456789512345678961234567897123456789800010000001"))]
    (is (= "05" (:addenda-type (first (:addenda (first result))))))
    (is (= 1 (count (:addenda (first result)))))
    (is (= "6" (:record-type (first result))))
    (is (= () (rest result)))))

(deftest test-detail-and-more
  (let [result (parse-detail '("6220818100021326             0000025600245366         MICKEY THOMPSON       020103112590000001"
                               "6220818100021442             0000031000243628         JOE DOKES             020103112590000002"
                               "6220818100021146             0000042500243699         LANA TURNER           020103112590000003"
                               "6220818100021722             0000021600243565         MIKE A WILLIAMS       020103112590000004"
                               "6220818100021458             0000031200243571         SAMUEL SLADE          021103112590000005"
                               "701OR JUANITA SLADE                                                                           "))]
    (is (= '("6220818100021442             0000031000243628         JOE DOKES             020103112590000002"
             "6220818100021146             0000042500243699         LANA TURNER           020103112590000003"
             "6220818100021722             0000021600243565         MIKE A WILLIAMS       020103112590000004"
             "6220818100021458             0000031200243571         SAMUEL SLADE          021103112590000005"
             "701OR JUANITA SLADE                                                                           ") (rest result)))))

(deftest test-batch-no-records-after-eight
  (let [[{header :header details :details control :control} & remaining]
    (parse-batch '("5220ABC TEST COMPANYPAYROLL             9000000625PPDPAYROLL   041587870415   1103112590000001"
                   "6220818100021326             0000025600245366         MICKEY THOMPSON       020103112590000001"
                   "6220818100021442             0000031000243628         JOE DOKES             020103112590000002"
                   "6220818100021146             0000042500243699         LANA TURNER           020103112590000003"
                   "6220818100021722             0000021600243565         MIKE A WILLIAMS       020103112590000004"
                   "6220818100021458             0000031200243571         SAMUEL SLADE          021103112590000005"
                   "701OR JUANITA SLADE                                                                           "
                   "822000000600081903860000000000000000001519009000000625                         103112590000001"))]
    (is (not (nil? header)))
    (is (= (count details) 5))
    (is (not (nil? control)))
    (is (empty? remaining))))

(deftest test-file-no-extra-blocking
  (let [[{header :header batches :batches control :control} & remaining]
    (parse-file '("101 101000048 1031125949504140050A094101FRB KANSAS CITY        RCB BANK CLAREMORE             "
                  "5220ABC TEST COMPANYPAYROLL             9000000625PPDPAYROLL   041587870415   1103112590000001"
                  "6220818100021326             0000025600245366         MICKEY THOMPSON       020103112590000001"
                  "6220818100021442             0000031000243628         JOE DOKES             020103112590000002"
                  "6220818100021146             0000042500243699         LANA TURNER           020103112590000003"
                  "6220818100021722             0000021600243565         MIKE A WILLIAMS       020103112590000004"
                  "6220818100021458             0000031200243571         SAMUEL SLADE          021103112590000005"
                  "701OR JUANITA SLADE                                                                           "
                  "822000000600081903860000000000000000001519009000000625                         103112590000001"
                  "5225CENTRAL LIFE    INS PREMS           1745722725PPDINS PREMS 041587870415   1103112590000002"
                  "6270818100021667             00000024003452233254     HARRIET FRODENHAUSEN  020103112590000006"
                  "6270818100021221             00000027003454577244     MAXWELL SANDUSKI      010103112590000007"
                  "6270818100021338             00000026003445649954     JOHN DILLINGER        020103112590000008"
                  "6270818100021229             00000024003434577893     MARK HARRISON         020103112590000009"
                  "6270818100021770             00000028003451117642     BILLY WATUSI          020103112590000010"
                  "6270818100021898             00000022003455425886     FRED LANGSTON         020103112590000011"
                  "6270818100021333             00000023563452569841     MARION QUEST          020103112590000012"
                  "6270818100021880             00000027453451235731     ANNI FRANKEN          020103112590000013"
                  "822500000800081903860000000202010000000000001745722725                         103112590000002"
                  "9000002000003000000140040951930000000020201000000151900                                       "))]
    (is (not (nil? header)))
    (is (= (count batches) 2))
    (is (not (nil? control)))
    (is (empty? remaining))))

(deftest test-file-from-disk
  (let [file-result (parse-ach-file "/home/kyle/Downloads/achfile.txt")
        normal-result (parse-file '("101 101000048 1031125949504140050A094101FRB KANSAS CITY        RCB BANK CLAREMORE             "
                                     "5220ABC TEST COMPANYPAYROLL             9000000625PPDPAYROLL   041587870415   1103112590000001"
                                     "6220818100021326             0000025600245366         MICKEY THOMPSON       020103112590000001"
                                     "6220818100021442             0000031000243628         JOE DOKES             020103112590000002"
                                     "6220818100021146             0000042500243699         LANA TURNER           020103112590000003"
                                     "6220818100021722             0000021600243565         MIKE A WILLIAMS       020103112590000004"
                                     "6220818100021458             0000031200243571         SAMUEL SLADE          021103112590000005"
                                     "701OR JUANITA SLADE                                                                           "
                                     "822000000600081903860000000000000000001519009000000625                         103112590000001"
                                     "5225CENTRAL LIFE    INS PREMS           1745722725PPDINS PREMS 041587870415   1103112590000002"
                                     "6270818100021667             00000024003452233254     HARRIET FRODENHAUSEN  020103112590000006"
                                     "6270818100021221             00000027003454577244     MAXWELL SANDUSKI      010103112590000007"
                                     "6270818100021338             00000026003445649954     JOHN DILLINGER        020103112590000008"
                                     "6270818100021229             00000024003434577893     MARK HARRISON         020103112590000009"
                                     "6270818100021770             00000028003451117642     BILLY WATUSI          020103112590000010"
                                     "6270818100021898             00000022003455425886     FRED LANGSTON         020103112590000011"
                                     "6270818100021333             00000023563452569841     MARION QUEST          020103112590000012"
                                     "6270818100021880             00000027453451235731     ANNI FRANKEN          020103112590000013"
                                     "822500000800081903860000000202010000000000001745722725                         103112590000002"
                                     "9000002000003000000140040951930000000020201000000151900                                       "))]
    (is (= file-result normal-result))))
