; opciones configurables

; sistema: metrico o tazas(incluye tsp y tbsp)
; temperatura: Fahrenheit o Celsius
; porciones: 
; filtro: all, dessert, savory

; funcion para leer si se usa un sistema metrico u otro
(defn metrOtz [opcion]
    (if (= opcion "metro")
        (println "si")
        (println "no")
    )
)

