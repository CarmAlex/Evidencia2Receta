(ns evidencia2receta.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; estilos por categoría
(def estilos
  {:quantity     "color: orange; font-weight: bold;"
   :unit         "color: darkorange;"
   :ingredient   "color: green; font-style: italic;"
   :step-number  "color: cyan; font-weight: bold;"
   :temperature  "color: red; text-decoration: underline;"
   :serves       "color: magenta; font-weight: bold;"
   :default      ""})

(defn span [texto tipo]
  (let [estilo (get estilos tipo "")]
    (str "<span style=\"" estilo "\">" texto "</span>")))

;; resaltar temperaturas sin conversión
(defn resaltar-temperaturas [texto]
  (str/replace texto
               #"(?i)(\d+)\s*\u00b0F"
               #(span % :temperature)))

;; resaltar cantidades e ingredientes etc
(defn resaltar-ingrediente [linea]
  (let [regex #"^\s*(\d+(?:\s+\d+/\d+)?|\d+/\d+)?\s*(cup|cups|tsp|tbsp|teaspoon|tablespoon|grams?|g|kg|ml|oz|ounces?|pounds?|lb|eggs?)?\s*(.*)"
        [_ cantidad unidad resto] (re-matches regex linea)]
    (str (when (and cantidad (not (str/blank? cantidad)))
           (str (span cantidad :quantity) " "))
         (when (and unidad (not (str/blank? unidad)))
           (str (span unidad :unit) " "))
         (if (not (str/blank? resto))
           (span resto :ingredient)
           linea))))

;; leer archivo de texto
(defn leer-archivo [ruta]
  (with-open [rdr (io/reader ruta)]
    (doall (line-seq rdr))))

;; extraer receta de un archivo
(defn leer-receta [ruta]
  (let [lineas (leer-archivo ruta)
        [titulo & resto] (drop-while str/blank? lineas)
        [meta resto1] (split-with #(not (re-find #"(?i)^ingredients$" %)) resto)
        [_ & resto2] resto1
        [ingredientes resto3] (split-with #(not (re-find #"(?i)^instructions$" %)) resto2)
        [_ & instrucciones] resto3]
    {:titulo (str/trim titulo)
     :meta (remove str/blank? meta)
     :ingredientes (remove str/blank? ingredientes)
     :instrucciones (remove str/blank? instrucciones)}))

;; generar html
(defn generar-html [ruta]
  (let [{:keys [titulo meta ingredientes instrucciones]} (leer-receta ruta)]
    (str "<html><head><meta charset=\"UTF-8\"></head><body>"
         "<h1>" (span titulo :default) "</h1>"

         "<h2>Detalles</h2><ul>"
         (apply str (map #(str "<li>" (if (re-find #"(?i)serves" %)
                                        (span % :serves)
                                        (resaltar-temperaturas %))
                               "</li>")
                         meta))
         "</ul>"

         "<h2>Ingredientes</h2><ul>"
         (apply str (map #(str "<li>" (resaltar-ingrediente %) "</li>")
                         ingredientes))
         "</ul>"

         "<h2>Instrucciones</h2><ol>"
         (apply str (map-indexed (fn [i paso]
                                   (str "<li>"
                                        (span (str (inc i) ".") :step-number)
                                        " " (resaltar-temperaturas paso)
                                        "</li>"))
                                 instrucciones))
         "</ol>"

         "</body></html>")))

;; guardar html
(defn guardar-html [archivo-salida archivo-receta]
  (spit archivo-salida (generar-html archivo-receta)))

;; existe folder salida?
(defn asegurar-directorio [ruta]
  (let [dir (io/file ruta)]
    (when-not (.exists dir)
      (.mkdirs dir))))

;; procesar los .txt de las recetas
(defn procesar-todas-las-recetas []
  (let [directorio "resource"
        salida-dir "salidas"]
    (asegurar-directorio salida-dir)
    (let [archivos (filter #(and (.isFile %)
                                 (str/ends-with? (.getName %) ".txt"))
                           (.listFiles (io/file directorio)))]
      (doseq [archivo archivos]
        (let [ruta-in (.getPath archivo)
              nombre (.getName archivo)
              nombre-html (str/replace nombre #"\.txt$" ".html")
              ruta-out (str salida-dir "/" nombre-html)]
          (println "Procesando:" nombre "→" nombre-html)
          (guardar-html ruta-out ruta-in))))))
