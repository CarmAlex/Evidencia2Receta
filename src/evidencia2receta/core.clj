 (ns evidencia2receta.core)
;; --- Mapa de colores y estilos (similar al hash de Racket) ---
(def colores
  {:quantity  "color: yellow; font-weight: bold;"
   :ingredient "color: green; font-style: italic;"
   :step-number "color: cyan; font-weight: bold;"
   :temperature "color: red; text-decoration: underline;" 
   :equipment "color: blue;"
   :serves "color: magenta; font-weight: bold;" 
   :default ""})

;; --- Función para envolver texto con estilo usando span ---
(defn span-con-estilo [texto tipo]
  (let [estilo (get colores tipo "")]
    (str "<span style=\"" estilo "\">" texto "</span>")))

(def (leer-archivo [ruta]
  (with-open [rdr (clojure.java.io/reader ruta)]
    (doall (line-seq rdr)))))

;; --- Función para procesar una receta y aplicar estilos ---
(defn procesar-receta [receta]
  (let [lineas (clojure.string/split-lines receta)]
    (mapv
      (fn [linea]
        (cond
          (re-matches #"\d+\.\s.*" linea) (span-con-estilo linea :step-number)
          (re-matches #"\d+\s.*" linea) (span-con-estilo linea :quantity)
          (re-matches #".*:\s.*" linea) (span-con-estilo linea :ingredient)
          (re-matches #".*:\s.*" linea) (span-con-estilo linea :temperature)
          (re-matches #".*:\s.*" linea) (span-con-estilo linea :equipment)
          (re-matches #".*serves\s\d+" linea) (span-con-estilo linea :serves)
          :else (span-con-estilo linea :default)))
      lineas)))



(println
(slurp "../../resources/Lemon Cake-1.txt"))
