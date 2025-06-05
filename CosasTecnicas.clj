
; Conversión de tazas a gramos
(def listaChida '(("1/2" "cups" "granulated sugar") 
                  ("1" "cups" "all-purpose flour") 
                  ("3/4" "cups" "butter")))

;funcion para hacer que las fracciones se hagan de cadenas en decimales 
(defn parse-fraction [frac]
  (let [parts (clojure.string/split frac #"/")]
    (if (= (count parts) 1)
      (Double/parseDouble (first parts))
      (/ (Double/parseDouble (first parts)) 
         (Double/parseDouble (second parts))))))


;Taza a gramos dependiendo del ingrediente
(defn tazaAg [ing]
  (let [cantidad (parse-fraction (first ing))
        ingrediente (nth ing 2) 
        conversiones {"granulated sugar" 200
                      "all-purpose flour" 125
                      "cocoa powder" 151
                      "powdered sugar" 120
                      "butter" 227
                      "water" 236}           
        gramos (get conversiones ingrediente 0)]
        (if gramos (* cantidad gramos)
        "ingrediente no encontrado"
        )
  )
)

(map tazaAg listaChida)