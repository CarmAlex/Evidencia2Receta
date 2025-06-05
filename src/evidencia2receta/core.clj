
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
