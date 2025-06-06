 (ns evidencia2receta.core)
;; --- Mapa de colores y estilos (similar al hash de Racket) ---
(def colores
  {:quantity  "color: yellow; font-weight: bold;"
   :ingredient "color: green; font-style: italic;"
   :step-number "color: cyan; font-weight: bold;"
   :temperature "color: red; text-decoration: underline;"
   :category "color: green; text-transform: uppercase;"
   :equipment "color: blue;"
   :portions "color: magenta; font-weight: bold;"
   :action "color: orange; font-style: italic;"
   :note "color: red; font-weight: bold; text-decoration: underline;"
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
          :else (span-con-estilo linea :default)))
      lineas)))



(println
(slurp "../../resources/Lemon Cake-1.txt"))