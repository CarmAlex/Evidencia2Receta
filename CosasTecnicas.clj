
; Conversión de tazas a gramos
(def listaChida '(("1/2" "TB" "granulated sugar") 
                  ("1" "TB" "all-purpose flour") 
                  ("3/4" "TB" "butter"))
)

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
                      "water" 236
                      "chocolate chips" 170
                      "canola oil" 217
                      "olive oil" 217
                      "grated cheese" 100
                      }           
        gramos (get conversiones ingrediente 0)]
        (if gramos (* cantidad gramos)
        "ingrediente no encontrado"
        )
  )
)

;cucharadas a gramos
(defn tbAg [ing]
  (let [cantidad (parse-fraction (first ing))
    ingrediente (nth ing 2)
    conversiones { "water" 15
                   "vanilla extract" 13.3
                   "vinegar" 15
                   "vegetable oil" 13.8     
                   "butter" 14.4             
                }
        gramos (get conversiones ingrediente 0)]
        (if gramos (* cantidad gramos)
        "ingrediente no encontrado"
        )      
  )
)

;cucharaditas a gramos
(defn tAg [ing]
  (let [cantidad (parse-fraction (first ing))
    ingrediente (nth ing 2)
    conversiones { "baking powder" 4
                   "baking soda" 3
                   "salt" 6
                   "vanilla" 4
    }
    gramos (get conversiones ingrediente 0)]
    (if gramos (* cantidad gramos)
    "ingrediente no encontrado"
    )
  )
)

;celsius a fahrenheit
(defn cAf [c]  
  (+ (* c (9 / 5)) 32)
)

;fahrenheit a celsius
(defn fAc [f]
  (/ (f - 32) (5 / 9))
)

;Para contar los pasos
;(defn contador [x])

(map tbAg listaChida)