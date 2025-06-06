
; Conversión de tazas a gramos
(def listaChida '(("1/2" "cup" "granulated sugar") 
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



; Taza a gramos y calorías 
(defn tazaAg [ing]
  (let [cantidad (parse-fraction (first ing))
        ingrediente (nth ing 2)
        conversiones {
          "granulated sugar" {:gramos 200, :calorias 774},
          "all-purpose flour" {:gramos 125, :calorias 455},
          "cocoa powder" {:gramos 151, :calorias 342},
          "powdered sugar" {:gramos 120, :calorias 467},
          "butter" {:gramos 227, :calorias 1628},
          "water" {:gramos 236, :calorias 0},
          "chocolate chips" {:gramos 170, :calorias 805},
          "canola oil" {:gramos 217, :calorias 1927},
          "olive oil" {:gramos 217, :calorias 1927},
          "grated cheese" {:gramos 100, :calorias 400}
        }
        
        datos (get conversiones ingrediente)]
    
    (if datos
      {:gramos (* cantidad (:gramos datos)),
       :calorias (* cantidad (:calorias datos))}
      "Ingrediente no encontrado")))

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

;para ver que conversion hacer
(defn convertidor [lista]
  (cond
    (= (second lista) "cup") (map tazaAg lista)
    :else "no"
  )
)

(convertidor listaChida)
