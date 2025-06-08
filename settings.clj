(ns Evidencia2Receta.settings
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; settings para procesar
;;
;; lee el archivo options.txt con formato:
;;
;; sistema: metrico o tazas
;; temperatura: Celsius o Fahrenheit
;; porciones: 6
;; filtro: dessert o all, etc.
;;
;; resulta en un mapa como:
;; {:sistema :metric, :temperatura :celsius, :porciones 6, :filtro "dessert"}

(defn leer-opciones [ruta]
  (let [lineas (with-open [rdr (io/reader ruta)]
                 (doall (line-seq rdr)))
        limpio (remove str/blank? lineas)
        opciones (reduce
                  (fn [acc linea]
                    (let [[clave valor] (str/split linea #":\s*" 2)]
                      (if (and clave valor)
                        (assoc acc
                               (keyword (str/lower-case (str/trim clave)))
                               (str/trim valor))
                        acc)))
                  {}
                  limpio)]
    ;; procesar valores específicos
    (-> opciones
        (update :porciones #(Integer/parseInt %))
        (update :sistema #(keyword (str/lower-case %)))         ; :metrico o :tazas
        (update :temperatura #(keyword (str/lower-case %))))))