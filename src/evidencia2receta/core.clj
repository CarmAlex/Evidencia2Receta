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
(defn resaltar-temperatura [linea opciones]
  (str/replace linea
               #"(?i)(\d+)\s*\u00b0F"
               (fn [[_ temp-str]]
                 (let [f (Double/parseDouble temp-str)
                       valor (if (= (:temperatura opciones) :celsius)
                               (Math/round (fAc f)) ; ← definida en CosasTecnicas
                               f)
                       unidad (if (= (:temperatura opciones) :celsius) "°C" "°F")]
                   (span (str valor unidad) :temperature)))))

;; implementacion de CosasTecnicas irá aquí
;; ----------------------------------------





;; convierte a número decimal escalado
(defn escalar-numero [texto factor]
  (let [parsear (fn [s]
                  (if (re-find #"/" s)
                    (let [[a b] (str/split s #"/")]
                      (/ (Double/parseDouble a) (Double/parseDouble b)))
                    (Double/parseDouble s)))
        partes (str/split texto #" ")
        total (reduce + (map parsear partes))]
    (* total factor)))

;; redondea cantidades decimales a formatos como "1 1/4"
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

;; resalta una línea de ingrediente con cantidad escalada
(defn resaltar-ingrediente [linea factor opciones]
  (let [regex #"^\s*(\d+(?:\s+\d+/\d+)?|\d+/\d+)?\s*(cup|cups|tsp|tsps|tbsp|tbsps|teaspoons?|tablespoons?|grams?|g|kg|ml|oz|ounces?|pounds?|lbs?|eggs?)?\s*(.*)"
        [_ cantidad unidad resto] (re-matches regex linea)
        nueva-cantidad (if (and cantidad factor)
                         (redondear-cantidad (escalar-numero cantidad factor))
                         cantidad)]
    ;; futura conversión a gramos se integra aquí
    (str (when (and nueva-cantidad (not (str/blank? nueva-cantidad)))
           (str (span nueva-cantidad :quantity) " "))
         (when (and unidad (not (str/blank? unidad)))
           (str (span unidad :unit) " "))
         (if (not (str/blank? resto))
           (span resto :ingredient)
           linea))))

;; lee un archivo de texto y devuelve sus líneas
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
    ;; alerta si una receta no tiene ingredientes o instrucciones
    (when (or (empty? (:ingredientes receta)) (empty? (:instrucciones receta)))
      (println "Estructura incompleta en receta:" ruta))
    receta))

;; extrae el número original de porciones desde el texto meta
(defn obtener-porciones [meta]
  (some (fn [linea]
          (when-let [[_ _ n] (re-find #"(?i)serv(es|ings)\s*[-:]?\s*(\d+)" linea)]
            (Integer/parseInt n)))
        meta))

;; genera HTML resaltado para una receta aplicando escalado
(defn generar-html [ruta opciones]
  (let [{:keys [titulo meta ingredientes instrucciones]} (leer-receta ruta)
        originales (obtener-porciones meta)
        deseadas (:porciones opciones)
        factor (if (and originales (pos? originales))
                 (/ deseadas originales)
                 1)]
    (str "<html><head><meta charset=\"UTF-8\"></head><body>"
         "<h1>" (span titulo :default) "</h1>"

         "<h2>Detalles</h2><ul>"
         (apply str (map #(str "<li>" (if (re-find #"(?i)serves" %)
                                        (span % :serves)
                                        (resaltar-temperatura % opciones))
                               "</li>")
                         meta))
         "</ul>"

         "<h2>Ingredientes</h2><ul>"
         (apply str (map #(str "<li>" (resaltar-ingrediente % factor opciones) "</li>")
                         ingredientes))
         "</ul>"

         "<h2>Instrucciones</h2><ol>"
         (apply str (map-indexed (fn [i paso]
                                   (str "<li>"
                                        (span (str (inc i) ".") :step-number)
                                        " " (resaltar-temperatura paso opciones)
                                        "</li>"))
                                 instrucciones))
         "</ol>"

         "</body></html>")))

;; guarda un archivo HTML procesado en carpeta src
(defn guardar-html [salida ruta opciones]
  (spit salida (generar-html ruta opciones)))

;; función principal para procesar todas las recetas
(defn procesar-recetas []
  (let [directorio "resources"         ;; carpeta con archivos .txt
        destino "src"                  ;; salida directa a src/
        opciones (settings/leer-opciones "options.txt")
        filtro (str/lower-case (:filtro opciones))
        archivos (filter #(and (.isFile %)
                               (str/ends-with? (.getName %) ".txt"))
                         (.listFiles (io/file directorio)))]

    ;; procesamiento funcional
    (doseq [archivo archivos]
      (let [ruta (.getPath archivo)
            receta (leer-receta ruta)
            contenido (str/join " " (concat [(:titulo receta)]
                                            (:meta receta)
                                            (:ingredientes receta)
                                            (:instrucciones receta)))
            incluir? (or (= filtro "all")
                         (str/includes? (str/lower-case contenido) filtro))]
        (when incluir?
          (let [nombre (.getName archivo)
                nombre-html (str/replace nombre #"\.txt$" ".html")
                ruta-html (str destino "/" nombre-html)]
            (println "Procesando:" nombre "→" nombre-html)
            (guardar-html ruta-html ruta opciones)))))))
