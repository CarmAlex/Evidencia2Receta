(ns evidencia2receta.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [evidencia2receta.settings :as settings]))

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

;; resalta temperaturas en formato °F
(defn resaltar-temperaturas [texto]
  (str/replace texto
               #"(?i)(\d+)\s*\u00b0F"
               #(span % :temperature)))

;; convierte cantidades como "1 1/2" o "3/4" en número decimal
(defn escalar-cantidad [cantidad-str factor]
  (let [parse (fn [s]
                (if (re-find #"/" s)
                  (let [[a b] (str/split s #"/")]
                    (/ (Double/parseDouble a) (Double/parseDouble b)))
                  (Double/parseDouble s)))
        parts (str/split cantidad-str #" ")
        total (reduce + (map parse parts))]
    (* total factor)))

;; redondea cantidades decimales a formatos como "1 1/4"
(defn redondear [n]
  (if (nil? n) ""
      (let [ent (int n)
            frac (- n ent)]
        (cond
          (< frac 0.01) (str ent)
          (< frac 0.26) (str ent " 1/4")
          (< frac 0.51) (str ent " 1/2")
          (< frac 0.76) (str ent " 3/4")
          :else (str (inc ent))))))

;; resalta una línea de ingrediente con cantidad escalada
(defn resaltar-ingrediente [linea factor]
  (let [regex #"^\s*(\d+(?:\s+\d+/\d+)?|\d+/\d+)?\s*(cup|cups|tsp|tsps|tbsp|tbsps|teaspoons?|tablespoons?|grams?|g|kg|ml|oz|ounces?|pounds?|lbs?|eggs?)?\s*(.*)"
        [_ cantidad unidad resto] (re-matches regex linea)
        cantidad-nueva (if (and cantidad factor)
                         (redondear (escalar-cantidad cantidad factor))
                         cantidad)]
    (str (when (and cantidad-nueva (not (str/blank? cantidad-nueva)))
           (str (span cantidad-nueva :quantity) " "))
         (when (and unidad (not (str/blank? unidad)))
           (str (span unidad :unit) " "))
         (if (not (str/blank? resto))
           (span resto :ingredient)
           linea))))

;; lee un archivo de texto y devuelve sus líneas
(defn leer-archivo [ruta]
  (with-open [rdr (io/reader ruta)]
    (doall (line-seq rdr))))

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
    ;; alerta si una receta no tiene ingredientes o instrucciones
    (when (or (empty? (:ingredientes receta)) (empty? (:instrucciones receta)))
      (println "Estructura incompleta en receta:" ruta))
    receta))

;; extrae el número original de porciones desde el texto meta
(defn extraer-porciones [meta]
  (some (fn [linea]
          (when-let [[_ _ n] (re-find #"(?i)serv(es|ings)\s*[-:]?\s*(\d+)" linea)]
            (Integer/parseInt n)))
        meta))

;; genera HTML resaltado para una receta aplicando escalado
(defn generar-html [ruta opciones]
  (let [{:keys [titulo meta ingredientes instrucciones]} (leer-receta ruta)
        originales (extraer-porciones meta)
        deseadas (:porciones opciones)
        factor (if (and originales (pos? originales))
                 (/ deseadas originales)
                 1)]
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
         (apply str (map #(str "<li>" (resaltar-ingrediente % factor) "</li>")
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

;; guarda un archivo HTML procesado en carpeta src
(defn guardar-html [archivo-salida archivo-receta opciones]
  (spit archivo-salida (generar-html archivo-receta opciones)))

;; función principal para procesar todas las recetas
(defn procesar-todas-las-recetas []
  (let [directorio "resources"         ;; carpeta con archivos .txt
        salida-dir "src"               ;; salida directa a src/
        opciones (settings/leer-opciones "options.txt") ;; lectura dinámica
        filtro (str/lower-case (:filtro opciones))
        archivos (filter #(and (.isFile %)
                               (str/ends-with? (.getName %) ".txt"))
                         (.listFiles (io/file directorio)))]

    ;; procesamiento funcional
    (doseq [archivo archivos]
      (let [ruta-in (.getPath archivo)
            receta (leer-receta ruta-in)
            texto-completo (str/join " " (concat [(:titulo receta)]
                                                 (:meta receta)
                                                 (:ingredientes receta)
                                                 (:instrucciones receta)))
            incluir? (or (= filtro "all")
                         (str/includes? (str/lower-case texto-completo) filtro))]

        (when incluir?
          (let [nombre (.getName archivo)
                nombre-html (str/replace nombre #"\.txt$" ".html")
                ruta-out (str salida-dir "/" nombre-html)]
            (println "Procesando:" nombre "→" nombre-html)
            (guardar-html ruta-out ruta-in opciones)))))))
