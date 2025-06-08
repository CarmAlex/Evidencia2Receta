; opciones configurables
(require '[clojure.string :as str]) 



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


; sistema: metrico o tazas(incluye tsp y tbsp)
(defn metrOtz [opcion]
    (if (= opcion "metric")
        "t"
        "f"
    )
)

; temperatura: Fahrenheit o Celsius
(defn temperatura [opcion]
    (if (= opcion "C")
        "t"
        "f"
    )
)

; porciones: formula chida para poder dividir los ingredientes con las porciones para luego hacer ajustes


; filtro: all, dessert, savory con un cond

(defn procesar-opciones [archivo]
  (let [opciones (leer-opciones archivo)]
    {:sistema (metrOtz (:sistema opciones))
     :temp (temperatura (:temp opciones))
     :porciones  (:porciones opciones)
     :filtra (:filtra opciones)
     }))


(procesar-opciones "opciones1.txt")

;(leer-opciones "opciones1.txt")
