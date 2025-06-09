(ns evidencia2receta.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [evidencia2receta.settings :refer [procesar-opciones]]))

;; estilos por categoría para resaltado HTML
(def estilos
  {:quantity     "color: orange; font-weight: bold;"
   :unit         "color: darkorange;"
   :ingredient   "color: green; font-style: italic;"
   :step-number  "color: cyan; font-weight: bold;"
   :temperature  "color: red; text-decoration: underline;"
   :serves       "color: magenta; font-weight: bold;"
   :default      ""})

;; genera un <span> con estilo para una categoría dada
(defn span [texto tipo]
  (let [estilo (get estilos tipo "")]
    (str "<span style=\"" estilo "\">" texto "</span>")))

;; ----------------------------------------------------------
;; implementacion de CosasTecnicas (parseo, conversión, etc.)
;; ----------------------------------------------------------

(defn parse-fraction [frac]
  (let [parts (str/split frac #"/")]
    (if (= (count parts) 1)
      (Double/parseDouble (first parts))
      (/ (Double/parseDouble (first parts))
         (Double/parseDouble (second parts))))))

(defn tazaAg [ing]
  (let [cantidad (parse-fraction (first ing))
        ingrediente (nth ing 2)
        conversiones {"granulated sugar" {:gramos 200, :calorias 714},
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
                      "almond flour" {:gramos 95, :calorias 640}}
        datos (get conversiones ingrediente)]
    (if datos
      {:gramos (* cantidad (:gramos datos))
       :calorias (* cantidad (:calorias datos))}
      nil)))

(defn tbAg [ing]
  (let [cantidad (parse-fraction (first ing))
        ingrediente (nth ing 2)
        conversiones {"water" {:gramos 15, :calorias 0},
                      "vanilla extract" {:gramos 13, :calorias 37},
                      "vinegar" {:gramos 15, :calorias 0},
                      "vegetable oil" {:gramos 13, :calorias 120},
                      "butter" {:gramos 14, :calorias 110}}
        datos (get conversiones ingrediente)]
    (if datos
      {:gramos (* cantidad (:gramos datos))
       :calorias (* cantidad (:calorias datos))}
      nil)))

(defn tAg [ing]
  (let [cantidad (parse-fraction (first ing))
        ingrediente (nth ing 2)
        conversiones {"baking powder" {:gramos 4.5, :calorias 5},
                      "baking soda" {:gramos 3.4, :calorias 0},
                      "salt" {:gramos 6, :calorias 0},
                      "vanilla extract" {:gramos 4, :calorias 12},
                      "granulated sugar" {:gramos 4.2, :calorias 15},
                      "dried oregano" {:gramos 1, :calorias 3},
                      "red pepper flakes" {:gramos 1, :calorias 0},
                      "smoked paprika" {:gramos 1, :calorias 1},
                      "black pepper" {:gramos 2.3, :calorias 5}}
        datos (get conversiones ingrediente)]
    (if datos
      {:gramos (* cantidad (:gramos datos))
       :calorias (* cantidad (:calorias datos))}
      nil)))

(defn fAc [f]
  (/ (- f 32) (/ 9 5)))

;convierte decimal a escalado
(defn escalar-numero [texto factor]
  (let [parsear (fn [s]
                  (if (re-find #"/" s)
                    (let [[a b] (str/split s #"/")]
                      (/ (Double/parseDouble a) (Double/parseDouble b)))
                    (Double/parseDouble s)))
        partes (str/split texto #" ")
        total (reduce + (map parsear partes))]
    (* total factor)))


;redondear cantidades
(defn redondear-cantidad [n]
  (if (nil? n) ""
      (let [ent (int n)
            frac (- n ent)]
        (cond
          (< frac 0.01) (str ent)
          (< frac 0.26) (str ent " 1/4")
          (< frac 0.51) (str ent " 1/2")
          (< frac 0.76) (str ent " 3/4")
          :else (str (inc ent))))))

(defn resaltar-temperatura [linea opciones]
  (str/replace linea
               #"(?i)(\d+)\s*\u00b0F"
               (fn [[_ temp-str]]
                 (let [f (Double/parseDouble temp-str)
                       valor (if (= (:temp opciones) "t")
                               (Math/round (fAc f))
                               f)
                       unidad (if (= (:temp opciones) "t") "°C" "°F")]
                   (span (str valor unidad) :temperature)))))

(defn resaltar-ingrediente [linea factor opciones]
  (let [regex #"^\s*(\d+(?:\s+\d+/\d+)?|\d+/\d+)?\s*(\w+)?\s*(.*)"
        [_ cantidad unidad resto] (re-matches regex linea)
        nueva-cantidad (if (and cantidad factor)
                         (redondear-cantidad (escalar-numero cantidad factor))
                         cantidad)]
    (str (when (and nueva-cantidad (not (str/blank? nueva-cantidad)))
           (str (span nueva-cantidad :quantity) " "))
         (when (and unidad (not (str/blank? unidad)))
           (str (span unidad :unit) " "))
         (if (not (str/blank? resto))
           (span resto :ingredient)
           linea))))
;; leer un archivo de texto y devuelve sus líneas
(defn leer-archivo [ruta]
  (with-open [lector (io/reader ruta)]
    (doall (line-seq lector))))

;; extrae partes estructuradas de una receta en texto plano
(defn leer-receta [ruta]
  (let [lineas (leer-archivo ruta)
        [titulo & resto] (drop-while str/blank? lineas)
        [meta resto1] (split-with #(not (re-find #"(?i)^ingredients$" %)) resto)
        [_ & resto2] resto1
        [ingredientes resto3] (split-with #(not (re-find #"(?i)^instructions$" %)) resto2)
        [_ & instrucciones] resto3
        receta {:titulo (str/trim titulo)
                :meta (remove str/blank? meta)
                :ingredientes (remove str/blank? ingredientes)
                :instrucciones (remove str/blank? instrucciones)}]
    (when (or (empty? (:ingredientes receta)) (empty? (:instrucciones receta)))
      (println "Estructura incompleta en receta:" ruta))
    receta))

;; extrae el número original de porciones desde el texto meta
(defn obtener-porciones [meta]
  (some (fn [linea]
          (when-let [[_ _ n] (re-find #"(?i)serv(es|ings)\s*[-:]?\s*(\d+)" linea)]
            (Integer/parseInt n)))
        meta))

;; función para clasificar unidad y aplicar conversión a gramos y calorías
(defn convertir-ingrediente [cantidad unidad nombre sistema]
  (let [entrada [cantidad unidad nombre]]
    (cond
      (or (= sistema "f") (= sistema :cup))
      (cond
        (re-find #"(?i)^cup" unidad) (tazaAg entrada)
        (re-find #"(?i)^tb" unidad) (tbAg entrada)
        (re-find #"(?i)^tsp" unidad) (tAg entrada)
        :else nil)
      :else nil)))

;; calcular calorías totales de una receta en base a ingredientes
(defn calcular-calorias [ingredientes sistema factor]
  (let [procesados (map (fn [linea]
                          (let [regex #"^\s*(\d+(?:\s+\d+/\d+)?|\d+/\d+)?\s*(\w+)?\s*(.*)"
                                [_ cantidad unidad nombre] (re-matches regex linea)]
                            (if (and cantidad unidad nombre)
                              (let [nueva (redondear-cantidad (escalar-numero cantidad factor))
                                    datos (convertir-ingrediente nueva unidad nombre sistema)]
                                (if (map? datos) (:calorias datos) 0))
                              0)))
                        ingredientes)]
    (reduce + procesados)))

;; genera HTML con todo, incluyendo calorías y receta original
(defn generar-html-con-calorias [ruta opciones]
  (let [{:keys [titulo meta ingredientes instrucciones]} (leer-receta ruta)
        originales (obtener-porciones meta)
        deseadas (:porciones opciones)
        sistema (:sistema opciones)
        factor (if (and originales (pos? originales)) (/ deseadas originales) 1)
        calorias-totales (calcular-calorias ingredientes sistema factor)
        calorias-porporcion (if (pos? deseadas) (int (/ calorias-totales deseadas)) calorias-totales)
        original-html (str "<pre>" (str/join "\n" (leer-archivo ruta)) "</pre>")]
    (str "<html><head><meta charset='UTF-8'></head><body>"
         "<h1>" (span titulo :default) "</h1>"
         "<h2>Contenido Original</h2>" original-html
         "<h2>Detalles</h2><ul>"
         (apply str (map #(str "<li>" (if (re-find #"(?i)serves" %) (span % :serves)
                                       (resaltar-temperatura % opciones)) "</li>") meta))
         "</ul>"
         "<h2>Ingredientes</h2><ul>"
         (apply str (map #(str "<li>" (resaltar-ingrediente % factor opciones) "</li>") ingredientes))
         "</ul>"
         "<p><b>Calorías totales:</b> " calorias-totales " kcal</p>"
         "<p><b>Calorías por porción:</b> " calorias-porporcion " kcal</p>"
         "<h2>Instrucciones</h2><ol>"
         (apply str (map-indexed (fn [i paso]
                                   (str "<li>" (span (str (inc i) ".") :step-number)
                                        " " (resaltar-temperatura paso opciones) "</li>"))
                                 instrucciones))
         "</ol></body></html>")))

;; guarda el HTML final
(defn guardar-html [salida ruta opciones]
  (spit salida (generar-html-con-calorias ruta opciones)))

;; procesamiento paralelo y medición de rendimiento
(defn procesar-multiples-recetas []
  (let [archivos-originales (->> (io/file "resources") .listFiles
                                 (filter #(str/ends-with? (.getName %) ".txt")))
        archivos-duplicados (take 100 (cycle archivos-originales))
        opciones (procesar-opciones "opciones1.txt")
        destino "src"]

    (time
     (doseq [archivo archivos-duplicados]
       (let [nombre (.getName archivo)
             ruta (.getPath archivo)
             nombre-html (str/replace nombre #"\.txt$" (str "-" (rand-int 1000000) ".html"))
             ruta-html (str destino "/" nombre-html)]
         (guardar-html ruta-html ruta opciones))))

    (let [t-inicio (System/nanoTime)
          trabajos (doall (map (fn [archivo]
                                 (future
                                   (let [nombre (.getName archivo)
                                         ruta (.getPath archivo)
                                         nombre-html (str/replace nombre #"\.txt$" (str "-" (rand-int 1000000) ".html"))
                                         ruta-html (str destino "/" nombre-html)]
                                     (guardar-html ruta-html ruta opciones))))
                               archivos-duplicados))]
      (doseq [t trabajos] @t)
      (let [t-fin (System/nanoTime)
            tiempo-ms (/ (- t-fin t-inicio) 1000000.0)]
        (println "Tiempo total paralelo (ms):" tiempo-ms)))))
