(ns evidencia2receta.settings
  (:require [clojure.string :as str]))

;; Lee las líneas del archivo y las convierte a pares clave-valor
(defn leer-opciones [archivo]
  (let [lineas (-> archivo slurp str/split-lines)
        pares (map #(str/split % #":") lineas)]
    (reduce (fn [acc [clave valor]]
              (assoc acc
                     (keyword (str/trim clave))
                     (let [val (str/trim (or valor ""))]
                       (cond
                         (re-matches #"\d+" val) (Integer/parseInt val)
                         (str/blank? val) nil
                         :else val))))
            {}
            pares)))

;; Sistema: métrico o tazas
(defn metrOtz [opcion]
  (cond
  (= opcion "metric") :toMetric
  (= opcion "cup") :toImp
  (= opcion "imperial") :toImp
  :else :none
)
)
;; Temperatura: Celsius o Farenheit
(defn temperatura [opcion]
  (= opcion "C"))


;; Procesa todas las opciones y devuelve el mapa esperado por core.clj
(defn procesar-opciones [archivo]
  (let [opciones (leer-opciones archivo)]
    {:temperatura     (temperatura (:temp opciones))
     :convertir       (metrOtz (:sistema opciones))
     :porciones-nueva (:porciones opciones)
     :filtra          (:filtra opciones)}))
