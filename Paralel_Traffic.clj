(ns prueba.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn limpiar-linea [linea]; Defines the `limpiar-linea` function that takes a line as input
  (-> linea; Uses the threading macro (->) to chain functions
      (str/replace #"[()]" ""); Replaces all parentheses in the line with an empty string
      (str/trim))); Trims the line to remove leading and trailing whitespaces

(defn parsear-linea [linea]; Defines the `parsear-linea` function that takes a line as input
  (if (empty? (limpiar-linea linea)); Checks if the cleaned line is empty
    []; Returns an empty list
    (map #(Integer/parseInt %) (str/split (limpiar-linea linea) #"\s+")))); Otherwise, splits the cleaned line into tokens using whitespace as the delimiter and converts each to an integer

(defn leer-archivo [ruta]; Defines the `leer-archivo` function that takes a file path as input
  (try; Tries to execute the following block of code
    (with-open [rdr (io/reader ruta)]; Opens a reader for the specified file path
      (doall (map parsear-linea (line-seq rdr)))); Reads all lines from the file, parses each line, and returns a list of parsed lines
    (catch Exception e; Catches an exception during file reading
      (println (str "Error al leer el archivo " ruta ": " (.getMessage e))); Prints an error message with the file path and exception message
      []))); Returns an empty list in case of error

(defn procesar-lineas-sentido-helper [linea indices crucero-id linea-num]
  ; Defines helper function `procesar-lineas-sentido-helper` to process a line with specific indices
  (if (>= (count linea) (apply max indices)) ; Checks if the line is long enough for the indices
    (map #(nth linea %) indices) ; Extracts values at the specified indices if the line is long enough
    (do ; If the line is too short
      (println (str "Linea demasiado corta (procesar-lineas-sentido-helper) en el crucero " crucero-id " en la linea " linea-num ": " linea " con indices: " indices)) ; Prints an error message
      (println (str "Longitud de la linea: " (count linea))) ; Prints the length of the line
      nil))) ; Returns nil

(defn procesar-lineas-sentido [coches crucero-id]
  ; Defines `procesar-lineas-sentido` to process car arrival data
  (mapcat (fn [linea-num linea]
            (if (empty? linea) ; Checks if the line is empty
              (do
                (println (str "Linea vacía (procesar-lineas-sentido-helper) en el crucero " crucero-id " en la linea " linea-num ": " linea)) ; Prints an error message
                []) ; Returns an empty list
              (remove nil? ; Removes any nil values from the result
                      [(procesar-lineas-sentido-helper linea [1 2 3 4] crucero-id linea-num) ; Processes the line with various sets of indices
                       (procesar-lineas-sentido-helper linea [2 3 4 5] crucero-id linea-num)
                       (procesar-lineas-sentido-helper linea [3 4 5 6] crucero-id linea-num)
                       (procesar-lineas-sentido-helper linea [4 5 6 7] crucero-id linea-num)
                       (procesar-lineas-sentido-helper linea [5 6 7 8] crucero-id linea-num)])))
          (range) coches)) ; Applies the function to each line in 'coches'

(defn procesar-lineas-helper [linea indices crucero-id linea-num]
  ; Helper function similar to `procesar-lineas-sentido-helper` but for logic lines
  (if (>= (count linea) (apply max indices)) ; Checks if the line is long enough for the indices
    (map #(nth linea %) indices) ; Extracts values at the specified indices
    (do ; If the line is too short
      (println (str "Linea demasiado corta (procesar-lineas-helper) en el crucero " crucero-id " en la linea " linea-num ": " linea " con indices: " indices)) ; Prints an error message
      (println (str "Longitud de la linea: " (count linea))) ; Prints the length of the line
      nil))) ; Returns nil

(defn procesar-lineas [logicas crucero-id]
  ; Processes logic lines and applies helper to each
  (mapcat (fn [linea-num linea]
            (if (empty? linea) ; Checks if the line is empty
              (do
                (println (str "Linea vacía (procesar-lineas-helper) en el crucero " crucero-id " en la linea " linea-num ": " linea)) ; Prints an error message
                []) ; Returns an empty list
              (remove nil?
                      [(procesar-lineas-helper linea [8 9 10] crucero-id linea-num) ; Processes the line with various sets of indices
                       (procesar-lineas-helper linea [9 10 11] crucero-id linea-num)
                       (procesar-lineas-helper linea [10 11 12] crucero-id linea-num)
                       (procesar-lineas-helper linea [11 12 13] crucero-id linea-num)
                       (procesar-lineas-helper linea [12 13 14] crucero-id linea-num)])))
          (range) logicas)) ; Applies the function to each line in 'logicas'

(def resultados-totales (atom {})) ; Defines an atom to store total results
(def veces-total (atom 1)) ; Defines an atom to count the total number of iterations
(def tiempos-muertos (atom {})) ; Defines an atom to store dead times by crossroad
(def total-tiempos (atom {})) ; Defines an atom to store the total times by crossroad
(def total-llamadas (atom {})) ; Defines an atom to count total calls by crossroad
(def tiempos-muertos-detallado (atom {})) ; Defines an atom to store detailed dead times by light and lane
(def tiempos-detallados (atom {})) ; Defines an atom to store detailed times by crossroad
(def tiempo-muerto-acumulado (atom {})) ; Defines an atom to store accumulated dead time by crossroad

(defn verificar-tiempos-muertos [tiempo-actual tiempo-anterior semaforo carril crucero-id funcionamiento]; Defines the function to check and update dead times
  (let [tiempo-limite (+ (first funcionamiento) (second funcionamiento))]; Calculates the time limit as the sum of the first two elements of `funcionamiento`
    (if (> (- tiempo-anterior tiempo-actual) tiempo-limite); If the time difference exceeds the limit
      (do
        (swap! tiempos-muertos update crucero-id (fnil inc 0)); Increments the dead time count for the crossroad
        (swap! tiempos-muertos-detallado update-in [crucero-id semaforo carril] (fnil inc 0)); Increments the detailed dead time count for the traffic light and lane
        (swap! tiempo-muerto-acumulado update crucero-id (fnil + 0) (- tiempo-anterior tiempo-actual))); Updates the accumulated dead time for the crossroad
      (@tiempos-muertos crucero-id)))) ; Returns the current dead time count for the crossroad

(defn convertir-a-positivo [valor]; Defines the `convertir-a-positivo` function with the parameter `valor`
  (if (neg? valor); If the value is negative
    (* -1 valor); Returns the value multiplied by -1 (makes it positive)
    valor)) ; Otherwise, returns the value as it is

(defn calcular-tiempo-espera [tiempo-llegada tiempo-ciclo-rojo coches-pasados tiempo-restV tiempo-espR]; Defines the function to calculate the waiting time
  (let [tiempo-espera-rojo (- tiempo-ciclo-rojo tiempo-llegada); Calculates the red light waiting time
        tiempo-espera-total (convertir-a-positivo (+ tiempo-espera-rojo tiempo-espR))]; Calculates the total waiting time and converts it to a positive value
    (cond
      (and (= coches-pasados 0) (<= tiempo-restV 0) (<= tiempo-llegada tiempo-ciclo-rojo)); If no cars passed and certain conditions hold
      (convertir-a-positivo (+ tiempo-ciclo-rojo (* 2 coches-pasados))); Returns the red light cycle time multiplied by 2 times the number of passed cars

      (= coches-pasados 0); If no cars passed
      tiempo-espera-total ; Returns the total waiting time

      :else; Otherwise
      (convertir-a-positivo (+ tiempo-espera-total (* 2 coches-pasados)))))); Returns the total waiting time plus 2 times the number of passed cars

(defn iterar-coches [coches-pasados resultados tiempo-restV funcionamiento tiempo-espR tiempo-espera-total semaforo carril crucero-id tiempo-anterior]
  ; Defines the function `iterar-coches` to iterate through car data and calculate waiting times
  (if (empty? coches-pasados) ; Checks if the list of passed cars is empty
    resultados ; If empty, returns the results
    (let [tiempo-llegada (first coches-pasados) ; Gets the arrival time of the first car
          tiempo-ciclo-completo (+ (first funcionamiento) (second funcionamiento) (nth funcionamiento 2)) ; Calculates the total cycle time
          tiempo-ciclo-rojo (first funcionamiento) ; Gets the red light cycle time
          tiempo-actualizado (if (or (<= tiempo-restV 0)
                                     (< (* @veces-total tiempo-ciclo-completo)
                                        (+ tiempo-llegada tiempo-espera-total)))
                               tiempo-ciclo-completo tiempo-ciclo-rojo) ; Determines the updated time based on conditions
          nuevo-tiempo-restV (if (<= tiempo-restV 0)
                               (+ (second funcionamiento) (nth funcionamiento 2))
                               (- tiempo-restV 2)) ; Calculates the new remaining green light time
          tiempo-espera (calcular-tiempo-espera tiempo-llegada tiempo-actualizado (count resultados) tiempo-restV tiempo-espR)] ; Calculates the waiting time
      (verificar-tiempos-muertos tiempo-espera tiempo-anterior semaforo carril crucero-id funcionamiento) ; Verifies and updates dead times if necessary
      (recur (rest coches-pasados) ; Recursively calls `iterar-coches` with updated parameters
             (conj resultados tiempo-espera) ; Adds the waiting time to the results
             nuevo-tiempo-restV funcionamiento tiempo-espR tiempo-espera-total semaforo carril crucero-id tiempo-espera)))) ; Passes the current waiting time as `tiempo-anterior`

(defn Tiempos1 [funcionamiento tiempos semaforo carril crucero-id]; Defines the function `Tiempos1` with the parameters `funcionamiento`, `tiempos`, `semaforo`, `carril`, and `crucero-id`
  (if (or (< (count funcionamiento) 3) (empty? tiempos)) ; Checks if `funcionamiento` is less than 3 or `tiempos` is empty
    '() ; If true, returns an empty list
    (let [resultados (iterar-coches tiempos '() (+ (second funcionamiento) (nth funcionamiento 2)) funcionamiento 0 0 semaforo carril crucero-id 0)]
      ; Calls `iterar-coches` with appropriate parameters and stores the result in `resultados`
      (reset! veces-total 1) ; Resets the `veces-total` counter to 1
      (swap! tiempos-muertos update crucero-id (constantly 0)) ; Resets the `tiempos-muertos` counter for the `crucero-id` to 0
      (swap! resultados-totales update crucero-id (fnil conj []) resultados) ; Updates `resultados-totales` with new results for `crucero-id`
      resultados))) ; Returns the results

(defn actualizar-tiempos [tiempos semaforo carril crucero-id]; Defines the function `actualizar-tiempos`
  (if (not (or (empty? tiempos) (empty? (@resultados-totales crucero-id)))); Verifies if `tiempos` is not empty and `resultados-totales` for the `crucero-id` is not empty
    (do
      (swap! total-tiempos update crucero-id (fnil + 0) (apply + tiempos)) ; Updates `total-tiempos` by adding the current times
      (swap! total-llamadas update crucero-id (fnil inc 0)) ; Increments the `total-llamadas` count for the `crucero-id`
      (swap! tiempos-detallados update-in [crucero-id semaforo carril] (fnil conj []) tiempos)) ; Updates `tiempos-detallados` for the `crucero-id`
    (println (str "No existe ese carril o sentido asi que no se tomara en cuenta para el promedio en el crucero " crucero-id)))) ; If not valid, prints an error message

(defn procesar-carril [llegadas logica semaforo carril crucero-id]; Defines the function `procesar-carril`
  (let [tiempos (Tiempos1 logica llegadas semaforo carril crucero-id)]; Calls `Tiempos1` and stores the result in `tiempos`
    (actualizar-tiempos tiempos semaforo carril crucero-id) ; Calls `actualizar-tiempos` with appropriate parameters
    tiempos)) ; Returns the times

(defn leer-archivos [crucero-id]; Defines the function `leer-archivos`
  (let [archivo-port1 (str "C:\\Users\\diego\\IMECO\\Clojure\\prueba\\resources\\Auxiliares\\formato-crucero" crucero-id ".txt"); Constructs the file path for the format file
        archivo-port2 (str "C:\\Users\\diego\\IMECO\\Clojure\\prueba\\resources\\Auxiliares\\coches-crucero" crucero-id ".txt")]; Constructs the file path for the cars file
    (if (and (.exists (io/file archivo-port1)) (.exists (io/file archivo-port2))); Verifies that both files exist
      (let [logicas (leer-archivo archivo-port1); Reads the format file and stores it in `logicas`
            coches (leer-archivo archivo-port2)]; Reads the cars file and stores it in `coches`
        [logicas coches]); Returns a list of `logicas` and `coches`
      (do
        (println (str "Alguno de los archivos del crucero " crucero-id " no existe.")); Prints an error message if one of the files does not exist
        [[] []])))) ; Returns two empty lists

(defn mostrar-promedio-general [crucero-id]; Defines the function to calculate and display the general average
  (let [total-carros (reduce + (map count (@resultados-totales crucero-id))); Calculates the total number of cars
        total-tiempo (reduce + (map #(reduce + %) (@resultados-totales crucero-id)))]; Calculates the total time for all cars
    (if (zero? total-carros); Checks if the total number of cars is zero
      (throw (ArithmeticException. "No se puede dividir por cero: No hay carros")); Throws an exception if there are no cars
      (/ total-tiempo total-carros)))) ; Otherwise, calculates and returns the average waiting time

(defn calcular-promedio-por-semaforo-carril [tiempos-detallados crucero-id]; Defines the function to calculate the average time per traffic light and lane
  (into {} ; Converts the result into a map
        (for [[sem carriles] (get tiempos-detallados crucero-id)] ; Iterates over each traffic light and its lanes
          [sem (into {} ; Converts the result into a map
                     (for [[carril tiempos] carriles]; Iterates over each lane and its times
                       [carril (if (seq tiempos); Checks if the times list is not empty
                                 (/ (reduce + (flatten tiempos)) (count (flatten tiempos))); Calculates the average time
                                 0)]))]))); Assigns 0 if the list is empty

(defn contar-tiempos-muertos-totales [tiempos-muertos-detallado crucero-id]; Defines the function to count total dead times
  (reduce + 0; Sums all values in the result of the following expression, starting from 0
          (for [[_ carriles] (get tiempos-muertos-detallado crucero-id)]; Iterates over the lanes
            (reduce + (vals carriles))))); Sums all values for each lane

(defn contar-tiempo-muerto-acumulado [tiempo-muerto-acumulado crucero-id]; Defines the function to count the accumulated dead time
  (get tiempo-muerto-acumulado crucero-id 0)) ; Gets the accumulated dead time for the crossroad, returning 0 if it doesn't exist

(defn guardar-tiempos-en-archivo [ruta tiempos-detallados crucero-id]; Defines the function to save times to a file
  (let [contenido (with-out-str (prn (get tiempos-detallados crucero-id)))]; Converts the times for the `crucero-id` into a string and stores it in `contenido`
    (spit ruta contenido))); Writes the content to the specified path

(defn procesar-crucero [crucero-id] ; Defines the function to process a crossroad
  (let [[logicas coches] (leer-archivos crucero-id); Reads the logic and car files for the crossroad
        resultados-logicas (procesar-lineas logicas crucero-id); Processes the logic lines
        resultados-coches (procesar-lineas-sentido coches crucero-id); Processes the car lines
        archivo-tiempos (str "C:\\Users\\diego\\IMECO\\Clojure\\prueba\\resources\\Auxiliares\\tiempos_de_llegada-crucero" crucero-id ".txt")]; Constructs the file path for the arrival times
    (doseq [i (range (min (count resultados-logicas) (count resultados-coches)))]; Iterates over the smaller count of logic or car lines
      (let [logica (nth resultados-logicas i); Gets the logic at position `i`
            coche (nth resultados-coches i)]; Gets the car at position `i`
        (procesar-carril coche logica (int (/ i 5)) (mod i 5) crucero-id))); Processes the lane with the parameters

    (guardar-tiempos-en-archivo archivo-tiempos @tiempos-detallados crucero-id); Saves the times to the corresponding file
    {:crucero-id crucero-id, :promedio (mostrar-promedio-general crucero-id), :tiempo-muerto (contar-tiempo-muerto-acumulado @tiempo-muerto-acumulado crucero-id)})); Returns a map with the details

(defn contar-coches-en-archivo [ruta] ; Defines the function to count cars in a file
  (try
    (with-open [rdr (io/reader ruta)]; Tries to open the file at the specified path
      (reduce + 0 (map count (map parsear-linea (line-seq rdr))))); Reads and parses each line, counts the elements in each line, and sums these counts
    (catch Exception e
      (println (str "Error al leer el archivo " ruta ": " (.getMessage e))) ; If an error occurs, prints an error message
      0))); Returns 0 if an error occurs

(defn mostrar-estadisticas [crucero-id]; Defines the function to display statistics for a crossroad
  (let [ruta-coches (str "C:\\Users\\diego\\IMECO\\Clojure\\prueba\\resources\\Auxiliares\\coches-crucero" crucero-id ".txt"); Constructs the file path for the car file
        total-coches (contar-coches-en-archivo ruta-coches)]; Counts the total number of cars from the file
    (println "Cantidad de coches que pasaron en el crucero:" total-coches)
    (println "Cantidad de coches que pasaron por semaforo y carril:" (get @tiempos-detallados crucero-id))
    (println "Cantidad total de veces en el crucero que un semaforo estuvo en verde y no paso ningun vehiculo:" (contar-tiempos-muertos-totales @tiempos-muertos-detallado crucero-id))
    (println "Veces de tiempo muerto por semaforo y carril:" (get @tiempos-muertos-detallado crucero-id))
    (println "Tiempo promedio total de los coches en el crucero:" (mostrar-promedio-general crucero-id))
    (println "Tiempo promedio por semaforo y carril:" (calcular-promedio-por-semaforo-carril @tiempos-detallados crucero-id))))

(defn solicitar-crucero-id []; Defines the function to request a crossroad ID
  (println "\n---Estadisticas de Crucero Especifico---\n")
  (println "Si quieres ver la estadistica de un crucero, pon su identificador (0 para salir):"); Prompts the user to input a crossroad ID
  (Integer. (read-line))) ; Reads and returns the user's input as an integer

(defn convertir-tiempo [nano-segundos]; Defines the function to convert nanoseconds to seconds
  (let [segundos (/ nano-segundos 1e9)]; Converts nanoseconds to seconds
    (format "%.5f segundos" segundos))) ; Formats the seconds with 5 decimal places

(defn main []; Defines the main function
  (let [cruceros-ids (range 1 41); Creates a list of crossroad IDs from 1 to 40
        tiempo-inicial (System/nanoTime); Gets the initial time in nanoseconds
        tiempos-promedios (doall (pmap procesar-crucero cruceros-ids)); Processes all crossroads in parallel and stores the results
        tiempo-final (System/nanoTime); Gets the final time in nanoseconds
        tiempos-promedios-ordenados (sort-by :promedio tiempos-promedios); Sorts the results by average waiting time
        tiempos-muertos-ordenados (sort-by :tiempo-muerto tiempos-promedios); Sorts the results by accumulated dead time
        tiempo-total (- tiempo-final tiempo-inicial)]; Calculates the total execution time
    (println "\n---Estadisticas Generales---\n"); Prints the general statistics header
    (println "Tiempo promedio de espera en la totalidad de los vehiculos de la simulacion:"
             (/ (reduce + (map :promedio tiempos-promedios)) (count tiempos-promedios))); Prints the average waiting time for all vehicles
    (println "\nTop 4 cruceros con mayor tiempo de espera promedio:"); Prints the header for the top 4 crossroads with the highest average waiting time
    (doseq [{:keys [crucero-id promedio]} (take-last 4 tiempos-promedios-ordenados)]; Iterates over the last 4 elements in the sorted results
      (println (str "Crucero " crucero-id ": " promedio))); Prints the crossroad ID and its average waiting time

    (println "\nTop 4 cruceros con menor tiempo de espera promedio:"); Prints the header for the top 4 crossroads with the lowest average waiting time
    (doseq [{:keys [crucero-id promedio]} (take 4 tiempos-promedios-ordenados)]; Iterates over the first 4 elements in the sorted results
      (println (str "Crucero " crucero-id ": " promedio))); Prints the crossroad ID and its average waiting time

    (println "\nTop 4 cruceros con mayor cantidad de tiempo muerto acumulado:"); Prints the header for the top 4 crossroads with the highest accumulated dead time
    (doseq [{:keys [crucero-id tiempo-muerto]} (take-last 4 tiempos-muertos-ordenados)]; Iterates over the last 4 elements in the sorted dead time results
      (println (str "Crucero " crucero-id ": " tiempo-muerto " segundos"))); Prints the crossroad ID and its accumulated dead time

    (println "\nTop 4 cruceros con menor cantidad de tiempo muerto acumulado:"); Prints the header for the top 4 crossroads with the lowest accumulated dead time
    (doseq [{:keys [crucero-id tiempo-muerto]} (take 4 tiempos-muertos-ordenados)]; Iterates over the first 4 elements in the sorted dead time results
      (println (str "Crucero " crucero-id ": " tiempo-muerto " segundos"))); Prints the crossroad ID and its accumulated dead time

    (println "\nTiempo que se tardo el programa: " (convertir-tiempo tiempo-total)); Prints the total execution time in seconds

    (loop []; Starts a loop to allow the user to request multiple times
      (let [crucero-id (solicitar-crucero-id)]; Requests the crossroad ID from the user
        (if (not= crucero-id 0); If the ID is not 0
          (do
            (mostrar-estadisticas crucero-id); Displays the statistics for the crossroad
            (recur)); Repeats the loop
          (println "Saliendo del programa...")))))); If the ID is 0, exits the program

(defn -main [& _] ; Calls the `main` function when the program starts
  (main))