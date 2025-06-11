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

;; gramos a tazas
(def conversInv
  {"sugar" {:cup 200, :tbsp 13.33, :tsp 4}
   "granulated sugar" {:cup 200, :tbsp 13.33, :tsp 4}
   "flour" {:cup 125, :tbsp 7.81, :tsp 2.6}
   "all-purpose flour" {:cup 125, :tbsp 7.81, :tsp 2.6}
   "butter" {:cup 227, :tbsp 14.17, :tsp 4.72}
   "water" {:cup 125, :tbsp 7.81, :tsp 2.6}
   "oil" {:cup 217.7, :tbsp 13.6, :tsp 4.5}    
   "olive oil" {:cup 217.7, :tbsp 13.6, :tsp 4.5}  
   "canola oil" {:cup 217.7, :tbsp 13.6, :tsp 4.5} 
   "vegetable oil" {:cup 217.7, :tbsp 13.6, :tsp 4.5} 
   "fresh yeast" {:tbsp 14, :tsp 4.67}  
   "dry yeast" {:tbsp 10, :tsp 3.33}
   "yeast" {:tbsp 14, :tsp 4.6}   
   "salt" {:tbsp 17.8, :tsp 5.9}
   })

(defn gramos-a-imperial [gramos ingrediente]
  (let [ingrediente-key (-> ingrediente
                           str/lower-case
                           (str/replace #"of\s+" "")  ; Elimina "of" si existe
                           str/trim)
        conversions (get conversInv ingrediente-key)]
    (when (and conversions (> gramos 0))
      (let [cup-size (:cup conversions)
            tbsp-size (:tbsp conversions)
            tsp-size (:tsp conversions)
            cantidad (cond
                      (and cup-size (>= gramos cup-size)) (/ gramos cup-size)
                      (and tbsp-size (>= gramos tbsp-size)) (/ gramos tbsp-size)
                      tsp-size (/ gramos tsp-size)
                      :else gramos)
            unidad (cond
                    (and cup-size (>= gramos cup-size)) "cup"
                    (and tbsp-size (>= gramos tbsp-size)) "tbsp"
                    tsp-size "tsp"
                    :else "g")]
        {:cantidad cantidad
         :unidad unidad}))))


;; obtiene la información de gramos y calorías para una cantidad dada de un ingrediente
(defn obtener-info [cantidad unidad ingrediente]
  (let [unidad (str/lower-case (or unidad ""))
        ingrediente (str/lower-case (or ingrediente ""))]
    (if-let [info (get-in conversiones [unidad ingrediente])]
      {:gramos (double (* cantidad (:gramos info)))
       :calorias (double (* cantidad (:calorias info)))}
      {:gramos 0.0 :calorias 0.0})))

(defn cAf [c] (+ (* c 9/5) 32))
(defn fAc [f] (/ (- f 32) 1.8))

;resalta las temperaturas
(defn resaltar-temperaturas [texto opciones]
  (let [modo (:convertir opciones)
        convertir? (fn [unidad-deseada unidad-actual]
                     (and (contains? #{:toMetric :toImp} modo)
                          (= unidad-deseada unidad-actual)))]
  (str/replace texto
               #"(?i)(\d+)\s*°F"
               (fn [[_ valorChido unidad]]
                 (try
                   (let [valor (Double/parseDouble valorChido)
                         unidad (str/upper-case unidad)
                         convertido (cond
                                     (convertir? "F" unidad) (format "%.1f °C" (fAc valor))
                                     (convertir? "C" unidad) (format "%.1f °F" (cAf valor))
                                     :else (str valorChido " °" unidad))]
                     (span convertido :temperature))
                     (catch Exception _ (str valorChido " °" unidad))
                   )
                )
    )
  )
)  

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

(defn extraer-porciones [meta-lineas]
  (let [patron #"(?i)(serves|porciones|servings|yield|makes|para|for)[: \-]*(\d+)"
        coincidencias (keep #(re-find patron %) meta-lineas)]
    (if (seq coincidencias)
      (Integer/parseInt (last (first coincidencias)))
      1)))

;; lee y separa una receta en título, info adicional, ingredientes e instrucciones
(defn leer-receta [ruta]
  (let [lineas (leer-archivo ruta)
        [titulo & resto] (drop-while str/blank? lineas)
        ;; busca encabezado de ingredientes (permite "Ingredients", "Ingredients:", etc.)
        [meta resto1] (split-with #(not (re-find #"(?i)^ingredients[:]? *$" %)) resto)
        [_ & resto2] resto1
        ;; busca encabezado de instrucciones (permite "Instructions", "Instructions:", etc.)
        [ingredientes resto3] (split-with #(not (re-find #"(?i)^instructions[:]? *$" %)) resto2)
        [_ & instrucciones] resto3
        porciones (extraer-porciones (concat [titulo] meta))]
    {:titulo (str/trim titulo)
     :meta (remove str/blank? meta)
     :ingredientes (remove str/blank? ingredientes)
     :instrucciones (remove str/blank? instrucciones)
     :porciones porciones}))


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
  (let [{:keys [titulo meta ingredientes instrucciones porciones]} (leer-receta ruta-original)
        porciones-original (max 1 porciones)
        porciones-nuevas   (:porciones-nueva opciones)
        factor (double (/ porciones-nuevas porciones-original))
        temp-metric? (:temperatura opciones)
        conversion-activa? (:convertir opciones)
        total-calorias (atom 0)]

    ;; función interna para procesar y resaltar una línea de ingrediente
 (defn procesar-ingrediente [linea]
   (let [regex #"^\s*(\d+(?:\s+\d+/\d+)?|\d+/\d+)?\s*([a-zA-Z]+)?\s+(.*)"
         [_ cantidad unidad bruto] (re-matches regex linea)
         escalar (when cantidad (escalar-cantidad cantidad factor))
         esc (when escalar (redondear escalar))
 
         ;; normaliza unidad
         unidad-norm (-> (or unidad "")
                         str/lower-case
                         (str/replace #"tablespoons?" "tbsp")
                         (str/replace #"teaspoons?" "tsp")
                         (str/replace #"ounces?" "oz")
                         (str/replace #"cups?" "cup")
                         (str/replace #"pints?" "pint")
                         (str/replace #"pounds?" "lb")
                         (str/replace #"cloves?" "clove")
                         (str/replace #"eggs?" "egg")
                         (str/replace #"dashes?" "dash")
                         (str/replace #"s$" ""))
 
         ;; limpia nombre del ingrediente
         ingrediente-limpio
         (-> bruto
             str/lower-case
             ;(str/replace #"(?i)chopped|fresh|large|small|diced|sliced|minced|unsalted|extra-virgin|clove[s]?|kosher|dark|white wine|sea|sifted|for dusting" "")
             (str/replace #"[,*\(\)].*" "")
             (str/replace #"\s+" " ")
             str/trim)
 
         info-metrico (if (and escalar unidad-norm ingrediente-limpio)
                       (obtener-info escalar unidad-norm ingrediente-limpio)
                       {:gramos 0 :calorias 0})
        
        resultado (case (:convertir opciones)
                   :toMetric (let [gramos (double (:gramos info-metrico)) calorias (double (:calorias info-metrico))]
                                (str (span esc :quantity) " "
                                     (span unidad-norm :unit) " "
                                     (span bruto :ingredient) " ("
                                     (format "%.1f g, %.1f cal" gramos calorias) ")"))

                   :toImp (if-let [conv (gramos-a-imperial (:gramos info-metrico) ingrediente-limpio)]

                   (let [cantidad-imp (redondear (:cantidad conv))]
                                    (str (span cantidad-imp :quantity) " "
                                         (span (:unidad conv) :unit) " "
                                         (span bruto :ingredient)))
                                  (str (span esc :quantity) " "
                                       (span unidad-norm :unit) " "
                                       (span bruto :ingredient)))
                   :none (str (span esc :quantity) " "
                              (span unidad-norm :unit) " "
                              (span bruto :ingredient)))]
 
     (swap! total-calorias + (:calorias info-metrico))
 
     ;; genera línea HTML
     (str
      (when esc (str (span esc :quantity) " "))
      (when (not (str/blank? unidad-norm)) (str (span unidad-norm :unit) " "))
      (when bruto (span bruto :ingredient))
      (when (and conversion-activa? (> (:gramos info-metrico) 0))
        (str " (" (format "%.1f g, %.1f cal"
                          (double (:gramos info-metrico))
                          (double (:calorias info-metrico))) ")")))))





    ;; función para resaltar temperaturas dentro de cada paso
    (defn procesar-instruccion [linea]
      (resaltar-temperaturas linea temp-metric?))

    ;; generar HTML final
    (let [html (str "<html><head><meta charset='UTF-8'><title>" titulo "</title></head><body>"
                    "<h1>" titulo "</h1>"

                    "<h3>Meta</h3><ul>"
                    (apply str (map #(str "<li>" % "</li>") meta))
                    "</ul>"

                    "<h3>Ingredientes (originalmente para " porciones-original " porciones, ajustado para " porciones-nuevas " porciones)</h3><ul>"
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
         archivos (take 100 (cycle archivos-originales))

         ;; Filtra archivos que contienen el texto buscado
         archivos-filtrados
         (filter (fn [archivo]
                   (let [ruta-in (.getPath archivo)
                         receta (leer-receta ruta-in)
                         texto (str/join " " (concat [(:titulo receta)]
                                                     (:meta receta)
                                                     (:ingredientes receta)
                                                     (:instrucciones receta)))]
                     (or (= filtro "all")
                         (str/includes? (str/lower-case texto) filtro))))
                 archivos)

         ;; Función pura para procesar un archivo
         procesar (fn [archivo]
                    (let [ruta-in (.getPath archivo)
                          nombre (.getName archivo)
                          nombre-html (str/replace nombre #"\.txt$" (str "-" (System/nanoTime) ".html"))
                          ruta-out (str salida-dir "/" nombre-html)]
                      (guardar-html ruta-out ruta-in opciones)))]

     ;; si no hay recetas filtradas, avisamos
     (if (empty? archivos-filtrados)
       (println (str " No se encontraron recetas que coincidan con el filtro: \"" filtro "\""))
       (let [tiempo-secuencial
             (tiempo-ejecucion #(doall (map procesar archivos-filtrados)))

             tiempo-paralelo
             (tiempo-ejecucion #(doall (pmap procesar archivos-filtrados)))

             hilos (.availableProcessors (Runtime/getRuntime))
             speedup (if (zero? tiempo-paralelo) 0 (/ tiempo-secuencial tiempo-paralelo))
             eficiencia (if (zero? hilos) 0 (/ speedup hilos))]

         (println "Recetas filtradas que se procesaron: " (count archivos-filtrados))
         (println "Tiempo secuencial: " tiempo-secuencial "ms")
         (println "Tiempo paralelo: " tiempo-paralelo "ms")
         (println "Speedup: " speedup)
         (println "Eficiencia: " eficiencia)))))

; evalúa el pararelismo para hacer varias iteraciónes con diferentes hilos y despues para hacer los grafos
(defn evaluar-paralelismo []
  (let [directorio "resources"
        opciones (settings/procesar-opciones "opciones1.txt")
        filtro (str/lower-case (:filtra opciones))
        archivos-originales (filter (fn [f]
                                      (and (.isFile f)
                                           (str/ends-with? (.getName f) ".txt")))
                                    (.listFiles (io/file directorio)))
        archivos (take 100 (cycle archivos-originales)) ;; exactamente 100 archivos
        archivos-filtrados (filter (fn [archivo]
                                     (let [ruta-in (.getPath archivo)
                                           receta (leer-receta ruta-in)
                                           texto (str/join " " (concat [(:titulo receta)]
                                                                       (:meta receta)
                                                                       (:ingredientes receta)
                                                                       (:instrucciones receta)))]
                                       (or (= filtro "all")
                                           (str/includes? (str/lower-case texto) filtro))))
                                   archivos)

        ;; solo procesa
        simular-procesar (fn [archivo]
                           (let [ruta-in (.getPath archivo)]
                             ;; PARA QUE NO SE GENEREN 1000 ARCHIVOS SJSJ
                             (leer-receta ruta-in)))

        n-archs (count archivos-filtrados)
        hilos-a-probar [1 2 4 6 8 12 16]
        tiempo-secuencial (tiempo-ejecucion #(doall (map simular-procesar archivos-filtrados)))]

    (println "\nTiempo secuencial base: " (format "%.2f" tiempo-secuencial) "ms")
    (println "Hilos\tTiempo(ms)\tSpeedup\tEficiencia")

    (doseq [n hilos-a-probar]
      (when (<= n n-archs)
        (let [partes (partition-all (int (Math/ceil (/ (double n-archs) n))) archivos-filtrados)
              futuros (map (fn [sublista]
                             (future (doall (map simular-procesar sublista))))
                           partes)
              tiempo (tiempo-ejecucion #(doall (map deref futuros)))
              speedup (/ tiempo-secuencial tiempo)
              eficiencia (/ speedup n)]
          (println n "\t" (format "%.2f" tiempo)
                   "\t\t" (format "%.2f" speedup)
                   "\t" (format "%.2f" eficiencia)))))))







;; función principal 
(defn -main []
  (println "Procesando recetas...")
  (procesar-recetas-paralelo)
  (println "Evaluando paralelismo...")
  (evaluar-paralelismo)
  )
