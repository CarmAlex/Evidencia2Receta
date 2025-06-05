

;funcion para hacer que las fracciones se hagan en cadenas 
(defn parse-fraction [frac]
  (let [parts (clojure.string/split frac #"/")]
    (if (= (count parts) 1)
      (Double/parseDouble (first parts))
      (/ (Double/parseDouble (first parts)) 
         (Double/parseDouble (second parts))))))