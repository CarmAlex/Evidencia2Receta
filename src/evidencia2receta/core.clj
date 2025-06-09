(ns evidencia2receta.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [evidencia2receta.settings :as settings]))

;; estilos por categoría para resaltado HTML
(def estilos
  {:quantity     "color: orange; font-weight: bold;"
   :unit         "color: darkorange;"
   :ingredient   "color: green; font-style: italic;"
   :step-number  "color: darkgreen; font-weight: bold;"
   :temperature  "color: red; text-decoration: underline;"
   :serves       "color: magenta; font-weight: bold;"
   :default      ""})

(defn span [texto tipo]
  (let [estilo (get estilos tipo "")]
    (str "<span style=\"" estilo "\">" texto "</span>")))

;; función para convertir fracciones tipo 1/2 o 1 1/4 a decimal
(defn parse-fraction [frac]
  (let [parts (str/split frac #" ")]
    (reduce + (map (fn [s]
                     (if (str/includes? s "/")
                       (let [[a b] (str/split s #"/")]
                         (/ (Double/parseDouble a) (Double/parseDouble b)))
                       (Double/parseDouble s)))
                   parts))))

;; funciones de conversión por unidad
(def conversiones
  {"cup" {"granulated sugar" {:gramos 200, :calorias 714}
          "sugar" {:gramos 200, :calorias 714}
          "all-purpose flour" {:gramos 125, :calorias 440}
          "flour" {:gramos 125, :calorias 440}
          "cocoa powder" {:gramos 151, :calorias 342}
          "powdered sugar" {:gramos 120, :calorias 467}
          "butter" {:gramos 227, :calorias 1628}
          "water" {:gramos 236, :calorias 0}
          "chocolate chips" {:gramos 170, :calorias 805}
          "chocolate" {:gramos 170, :calorias 805}
          "canola oil" {:gramos 217, :calorias 1927}
          "olive oil" {:gramos 217, :calorias 1927}
          "oil" {:gramos 217, :calorias 1927}
          "grated parmesan cheese" {:gramos 90, :calorias 1650}
          "parmesan cheese" {:gramos 90, :calorias 1650}
          "grated romano cheese" {:gramos 112, :calorias 800}
          "romano cheese" {:gramos 112, :calorias 800}
          "chopped parsley" {:gramos 60, :calorias 16}
          "parsley" {:gramos 60, :calorias 16}
          "fresh lemon juice" {:gramos 240, :calorias 60}
          "lemon juice" {:gramos 240, :calorias 60}
          "almond flour" {:gramos 95, :calorias 640}}
   "tbsp" {"vanilla extract" {:gramos 13, :calorias 37}
           "vanilla" {:gramos 13, :calorias 37}
           "vinegar" {:gramos 15, :calorias 0}
           "white wine vinegar" {:gramos 15, :calorias 0}
           "vegetable oil" {:gramos 13, :calorias 120}
           "butter" {:gramos 14, :calorias 110}
           "olive oil" {:gramos 13, :calorias 120}
           "canola oil" {:gramos 13, :calorias 120}
           "oil" {:gramos 13, :calorias 120}
           "water" {:gramos 15, :calorias 0}}
   "tsp" {"baking powder" {:gramos 4.5, :calorias 5}
          "baking soda" {:gramos 3.4, :calorias 0}
          "salt" {:gramos 6, :calorias 0}
          "sea salt" {:gramos 6, :calorias 0}
          "kosher salt" {:gramos 6, :calorias 0}
          "vanilla extract" {:gramos 4, :calorias 12}
          "vanilla" {:gramos 4, :calorias 12}
          "granulated sugar" {:gramos 4.2, :calorias 15}
          "sugar" {:gramos 4.2, :calorias 15}
          "dried oregano" {:gramos 1, :calorias 3}
          "oregano" {:gramos 1, :calorias 3}
          "red pepper flakes" {:gramos 1, :calorias 0}
          "smoked paprika" {:gramos 1, :calorias 1}
          "black pepper" {:gramos 2.3, :calorias 5}
          "pepper" {:gramos 2.3, :calorias 5}}
   "oz" {"dry fettuccine pasta" {:gramos 28.3, :calorias 100}
         "fettuccine" {:gramos 28.3, :calorias 100}
         "pasta" {:gramos 28.3, :calorias 100}}
   "lb" {"new york strip steaks" {:gramos 453.6, :calorias 997.92}
         "steak" {:gramos 453.6, :calorias 997.92}
         "steaks" {:gramos 453.6, :calorias 997.92}}
   "pint" {"heavy cream" {:gramos 470.3, :calorias 1600}
           "cream" {:gramos 470.3, :calorias 1600}}
   "egg" {"egg" {:gramos 50, :calorias 70}
          "eggs" {:gramos 50, :calorias 70}
          "large egg" {:gramos 50, :calorias 70}
          "large eggs" {:gramos 50, :calorias 70}}})



;; obtiene la información de gramos y calorías para una cantidad dada de un ingrediente
(defn obtener-info [cantidad unidad ingrediente]
  (let [unidad (str/lower-case (or unidad ""))
        ingrediente (str/lower-case (or ingrediente ""))]
    (if-let [info (get-in conversiones [unidad ingrediente])]
      {:gramos (* cantidad (:gramos info))
       :calorias (* cantidad (:calorias info))}
      {:gramos 0 :calorias 0})))

(defn cAf [c] (+ (* c 9/5) 32))
(defn fAc [f] (/ (- f 32) 1.8))

;resalta las temperaturas
(defn resaltar-temperaturas [texto temp-metric?]
  (str/replace texto
               #"(?i)(\d+)\s*°F"
               (fn [[_ fstr]]
                 (let [f (Double/parseDouble fstr)
                       convertido (if (= temp-metric? "t")
                                    (format "%.1f °C" (fAc f))
                                    (str f " °F"))]
                   (span convertido :temperature)))))

;escala la cantidad de la fracción dada por un factor
(defn escalar-cantidad [cantidad-str factor]
  (try
    (let [valor (parse-fraction cantidad-str)]
      (* valor factor))
    (catch Exception _ 0)))

(defn redondear [n]
  (let [ent (int n)
        frac (- n ent)]
    (cond
      (< frac 0.13) (str ent)
      (< frac 0.38) (str ent " 1/4")
      (< frac 0.63) (str ent " 1/2")
      (< frac 0.88) (str ent " 3/4")
      :else (str (inc ent)))))
;resalta un ingrediente con su cantidad y unidad
(defn resaltar-ingrediente [linea factor]
  (let [regex #"^\s*(\d+(?:\s+\d+/\d+)?|\d+/\d+)?\s*([a-zA-Z]+)?\s+(.*)"
        [_ cantidad unidad resto] (re-matches regex linea)]
    (if cantidad
      (let [esc (redondear (escalar-cantidad cantidad factor))]
        (str
         (span esc :quantity) " "
         (when unidad (str (span unidad :unit) " "))
         (when resto (span resto :ingredient)))) 
      (str
       (when unidad (str (span unidad :unit) " "))
       (when resto (span resto :ingredient))))))

;; lee el archivo línea por línea 
(defn leer-archivo [ruta]
  (with-open [rdr (io/reader ruta)]
    (doall (line-seq rdr))))


;; lee y separa una receta en título, info adicional, ingredientes e instrucciones
(defn leer-receta [ruta]
  (let [lineas (leer-archivo ruta)
        [titulo & resto] (drop-while str/blank? lineas)
        ;; busca encabezado de ingredientes (permite "Ingredients", "Ingredients:", etc.)
        [meta resto1] (split-with #(not (re-find #"(?i)^ingredients[:]? *$" %)) resto)
        [_ & resto2] resto1
        ;; busca encabezado de instrucciones (permite "Instructions", "Instructions:", etc.)
        [ingredientes resto3] (split-with #(not (re-find #"(?i)^instructions[:]? *$" %)) resto2)
        [_ & instrucciones] resto3]
    {:titulo (str/trim titulo)
     :meta (remove str/blank? meta)
     :ingredientes (remove str/blank? ingredientes)
     :instrucciones (remove str/blank? instrucciones)}))


;; duplica las recetas para simular un conjunto grande de archivos
(defn duplicar-recetas [archivos]
  (take 100 (cycle archivos)))

;; mide el tiempo de ejecución en milisegundos de una función
(defn tiempo-ejecucion [f]
  (let [inicio (System/nanoTime)
        _ (f)
        fin (System/nanoTime)]
    (/ (- fin inicio) 1e6))) ; ms

;; guarda una receta procesada en un archivo HTML con conversiones, calorías y resaltado
(defn guardar-html [ruta-html ruta-original opciones]
  (let [{:keys [titulo meta ingredientes instrucciones]} (leer-receta ruta-original)
        porciones-original (int (or (:porciones opciones) 1))
        porciones-nuevas   (int (or (:porciones-nueva opciones) 1))
        factor (double (/ porciones-nuevas porciones-original))
        temp-metric? (:temperatura opciones)
        conversion-activa? (:convertir opciones)
        total-calorias (atom 0)]

    ;; función interna para procesar y resaltar una línea de ingrediente
 (defn procesar-ingrediente [linea]
   (let [regex #"^\s*(\d+(?:\s+\d+/\d+)?|\d+/\d+)?\s*([a-zA-Z]+)?\s+(.*)"
         [_ cantidad unidad bruto] (re-matches regex linea)
         esc (when cantidad (escalar-cantidad cantidad factor))
 
         ;; normaliza unidad (ej: tablespoons → tbsp)
         ;; normaliza unidad correctamente
unidad-norm (-> (or unidad "")
                str/lower-case
                (str/replace #"tablespoons?" "tbsp")
                (str/replace #"teaspoons?" "tsp")
                (str/replace #"ounces?" "oz")
                (str/replace #"cups?" "cup")
                (str/replace #"pints?" "pint")
                (str/replace #"pounds?" "lb")
                (str/replace #"cloves?" "clove")
                (str/replace #"dashes?" "dash")
                (str/replace #"eggs?" "egg") ; mueve esto al final
                (str/replace #"large|small|medium" "") 
                (str/replace #"s$" "")
                str/trim)

         ;; limpia nombre del ingrediente
         ingrediente-limpio
         (-> bruto
             str/lower-case
             (str/replace #"(?i)chopped|fresh|large|small|medium|diced|sliced|minced|unsalted|extra-virgin|clove[s]?|kosher|dark|white wine|sea|sifted|for dusting|vanilla$" "")
             (str/replace #"[,*\(\)].*" "")  ; remueve paréntesis, comas y notas
             (str/replace #"\s+" " ")        ; normaliza espacios
             str/trim)
 
         info (if (and conversion-activa? esc unidad-norm ingrediente-limpio)
                (obtener-info esc unidad-norm ingrediente-limpio)
                {:gramos 0 :calorias 0})]
 
     (swap! total-calorias + (:calorias info))
 
     ;; genera HTML
     (str
      (when esc (str (span (redondear esc) :quantity) " "))
      (when unidad (str (span unidad :unit) " "))
      (when bruto (span bruto :ingredient))
      (when conversion-activa?
        (str " (" (format "%.1f g, %.1f cal"
                          (double (:gramos info))
                          (double (:calorias info))) ")")))))




    ;; función para resaltar temperaturas dentro de cada paso
    (defn procesar-instruccion [linea]
      (resaltar-temperaturas linea temp-metric?))

    ;; generar HTML final
    (let [html (str "<html><head><meta charset='UTF-8'><title>" titulo "</title></head><body>"
                    "<h1>" titulo "</h1>"

                    "<h3>Meta</h3><ul>"
                    (apply str (map #(str "<li>" % "</li>") meta))
                    "</ul>"

                    "<h3>Ingredientes (para " porciones-nuevas " porciones)</h3><ul>"
                    (apply str (map #(str "<li>" (procesar-ingrediente %) "</li>") ingredientes))
                    "</ul>"

                    "<h4>Total Calorías: " @total-calorias "</h4>"
                    "<h4>Por porción: " (format "%.1f" (/ (double @total-calorias) porciones-nuevas)) "</h4>"

                    "<h3>Instrucciones</h3><ol>"
                    (apply str
                           (map #(str "<li>" (span (resaltar-temperaturas % temp-metric?) :step-number) "</li>")
                                instrucciones))
                    "</ol>"

                    "<hr><h3>Contenido Original</h3><pre>"
                    (str/join "\n" (leer-archivo ruta-original))
                    "</pre>"

                    "</body></html>")]
      (spit ruta-html html))))

;; procesa en paralelo y secuencial 100 recetas, mostrando métricas de rendimiento
(defn procesar-recetas-paralelo []
  (let [directorio "resources"
        salida-dir "src"
        opciones (settings/procesar-opciones "opciones1.txt")
        filtro (str/lower-case (:filtra opciones))
        archivos-originales (filter #(and (.isFile %)
                                          (str/ends-with? (.getName %) ".txt"))
                                    (.listFiles (io/file directorio)))
        archivos (duplicar-recetas archivos-originales)
        procesar (fn [archivo]
                   (let [ruta-in (.getPath archivo)
                         receta (leer-receta ruta-in)
                         texto (str/join " " (concat [(:titulo receta)]
                                                     (:meta receta)
                                                     (:ingredientes receta)
                                                     (:instrucciones receta)))
                         incluir? (or (= filtro "all")
                                      (str/includes? (str/lower-case texto) filtro))]
                     (when incluir?
                       (let [nombre (.getName archivo)
                             nombre-html (str/replace nombre #"\.txt$" (str "-" (System/nanoTime) ".html"))
                             ruta-out (str salida-dir "/" nombre-html)]
                         (guardar-html ruta-out ruta-in opciones)))))]

    ;; medir y comparar tiempos
    (let [tiempo-secuencial (tiempo-ejecucion #(doseq [a archivos] (procesar a)))
          tiempo-paralelo   (tiempo-ejecucion #(doall (pmap procesar archivos)))
          hilos             (.availableProcessors (Runtime/getRuntime))
          speedup           (if (zero? tiempo-paralelo) 0 (/ tiempo-secuencial tiempo-paralelo))
          eficiencia        (if (zero? hilos) 0 (/ speedup hilos))]
      (println "Tiempo secuencial: " tiempo-secuencial "ms")
      (println "Tiempo paralelo: " tiempo-paralelo "ms")
      (println "Speedup: " speedup)
      (println "Eficiencia: " eficiencia))))

;; función principal que se puede ejecutar desde la terminal
(defn -main []
  (println "Procesando recetas...")
  (procesar-recetas-paralelo))
