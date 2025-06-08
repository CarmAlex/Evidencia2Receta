; opciones configurables
(require '[clojure.string :as str]) 


; sistema: metrico o tazas(incluye tsp y tbsp)
; temperatura: Fahrenheit o Celsius
; porciones: 
; filtro: all, dessert, savory

; funcion para leer el .txt
(defn leer-opciones [archivo]
  (let [lineas (-> archivo
                   slurp
                   str/split-lines)
        pares (map #(str/split % #": ") lineas)
        mapa (reduce (fn [acc [clave valor]]
                       (assoc acc
                              (keyword (str/trim clave))
                              (cond
                                (re-matches #"\d+" valor) (Integer/parseInt valor)
                                (str/blank? valor) nil
                                :else (str/trim valor))))
                    {}
                    pares)]
    mapa))


; metrico o tazas
(defn metrOtz [opcion]
    (if (= opcion "metric")
        "t"
        "f"
    )
)

(defn procesar-opciones [archivo]
  (let [opciones (leer-opciones archivo)]
    {:sistema (metrOtz (:sistema opciones))
     ; Puedes añadir más procesamientos aquí
     }))


(procesar-opciones "opciones1.txt")

;(leer-opciones "opciones1.txt")
