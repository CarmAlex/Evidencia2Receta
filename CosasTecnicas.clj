
; lista de prueba
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
          "granulated sugar" {:gramos 200, :calorias 714},
          "all-purpose flour" {:gramos 125, :calorias 440},
          "cocoa powder" {:gramos 151, :calorias 342},
          "powdered sugar" {:gramos 120, :calorias 467},
          "butter" {:gramos 227, :calorias 1628},
          "water" {:gramos 236, :calorias 0},
          "chocolate chips" {:gramos 170, :calorias 805},
          "canola oil" {:gramos 217, :calorias 1927},
          "olive oil" {:gramos 217, :calorias 1927},
          "grated Parmesan cheese" {:gramos 90, :calorias 1650},
          "grated Romano cheese" {:gramos 112, :calorias 800},
          "chopped parsley" {:gramos 60, :calorias 16},
          "fresh lemon juice" {:gramos 240, :calorias 60},
          "almond flour" {:gramos 95, :calorias 640}
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
    conversiones { "water" {:gramos 15, :calorias 0},
                   "vanilla extract" {:gramos 13, :calorias 37},
                   "vinegar" {:gramos 15, :calorias 0},
                   "vegetable oil" {:gramos 13, :calorias 120},
                   "butter" {:gramos 14, :calorias 110}        
                }
        datos (get conversiones ingrediente)]
        (if datos 
        {:gramos (* cantidad (:gramos datos)),
         :calorias (* cantidad (:calorias datos))}
        "Ingrediente no encontrado"
        )      
  )
)

;cucharaditas a gramos
(defn tAg [ing]
  (let [cantidad (parse-fraction (first ing))
    ingrediente (nth ing 2)
    conversiones { "baking powder" {:gramos 4.5, :calorias 5},
                   "baking soda" {:gramos 3.4, :calorias 0},
                   "salt" {:gramos 6, :calorias 0},
                   "vanilla extract" {:gramos 4, :calorias 12},
                   "granulated sugar" {:gramos 4.2, :calorias 15},
                   "dried oregano" {:gramos 1, :calorias 3},
                   "red pepper flakes" {:gramos 1, :calorias 0},
                   "smoked paprika" {:gramos 1, :calorias 1},
                   "black pepper" {:gramos 2.3, :calorias 5}
                   
    }
    datos (get conversiones ingrediente)]
    (if datos 
    {:gramos (* cantidad (:gramos datos))
     :calorias (* cantidad (:calorias datos))}
    "Ingrediente no encontrado"
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

;caso de prueba
;(map tazaAg listaChida)
