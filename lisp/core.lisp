;codigo lisp 
;REQUERIMIENTO 1
;;======================================
;; FUNCIÓN: Trancisicion 
;; NATURALEZA: pura 
;; ESTRATEGIA: condicional
;; IMPACTO: no destructivo
;; =======================================
(defun transicion (color-actual cambiar-a)
  (cond
    ((and (equal color-actual 'en-rojo)
          (equal cambiar-a 'amarillo))
     (list color-actual "cambiar-a-amarillo"))
    ((and (equal color-actual 'en-amarillo)
          (equal cambiar-a 'verde))
     (list color-actual "cambiar-a-verde"))
    ((and (equal color-actual 'en-verde)
          (equal cambiar-a 'rojo))
     (list color-actual "cambiar-a-rojo"))
    (t
     (list color-actual 'accion-por-defecto))))

;REQUERIMIENTO 2
;;===============================
;;FUNCIÓN: timer
;;NATURALEZA: pura 
;;ESTRATEGIA: condicional
;;IMPACTO; No destructivo
;;===============================
(defun timer (epoch)
  ; calcular en qué segundo del ciclo estoy
  (let ((posicion (mod epoch 216)))
    ; decidir el color según esa posición
    (cond
      ((< posicion 90) 'rojo)
      ((< posicion 96) 'amarillo)
      (t 'verde)
    )
  )
)

;REQUERIMIENTO 3
;; ========================================================================
;; FUNCIÓN: registrar-cambio 
;; NATURALEZA: Impura 
;; ESTRATEGIA: Directa / Ejecución Secuencial
;; IMPACTO: No destructiva

(defun registrar-cambio (epoch color-actual cambiar-a)
  (format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%" epoch color-anterior color-nuevo))

;REQUERIMIENTO 4a
;; ========================================================================
;; FUNCIÓN: Duración-ciclo
;; NATURALEZA: Recursiva 
;; ESTRATEGIA: Recorrido recursivo a través de la lista
;; IMPACTO: No destructiva
;; ========================================================================


(defun duracion-ciclo (tiempos-fase)
    (cond
      ((endp tiempos-fase) 0) ;Caso base
      ((numberp (first tiempos-fase)) ;Verifica si es un numero
        (+ (first tiempos-fase) (duracion-ciclo (cdr tiempos-fase)))) ;Suma el primero de la lista con el resto de la lista
      (t (duracion-ciclo (cdr tiempos-fase)))
    )
  )

;REQUERIMIENTO 4b
;; ========================================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura 
;; ESTRATEGIA: Función Predicado / Condicional
;; IMPACTO: No destructiva
;; ========================================================================
(defun recomendacion-ciclo (duracion)
  "Evalúa la duración de un ciclo bajo los estándares de la psicología del conductor (35s - 150s)."
  (cond
    ((< duracion 35) 
     "No recomendado: El ciclo es demasiado corto para la psicología del conductor.")
    ((> duracion 150) 
     "No recomendado: El ciclo es demasiado largo (excede los 150s) y genera impaciencia.")
    (t 
     "Recomendado: La duración se encuentra en el rango óptimo (35-150 segundos).")))

;; REQUERIMIENTO 5
;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: Cálculo Directo
;; IMPACTO: No destructiva
;; ========================================================

(defun ciclos-por-tiempo (minutos)
  (let ((total (* 60 minutos))
        (duracion (+ 90 6 120)))
    (truncate total duracion)))

;;REQUERIMIENTO 6.
;; ========================================================
;; FUNCIÓN: informe-distribucion
;; NATURALEZA: Pura
;; ESTRATEGIA: Orden Superior
;; IMPACTO: No destructiva
;; ========================================================

(defun informe-distribucion (rojo amarillo verde)
 (let* ((tiempo-combinado 3600)
       (duracion-del-ciclo (+ rojo amarillo verde))
       (ciclos-completos (floor (/ tiempo-combinado duracion-del-ciclo)))
       (resto-segundos (- tiempo-combinado (* ciclos-completos duracion-del-ciclo))))

       (t-rojo (* ciclos-completos rojo))
       (t-amarillo (* ciclos-completos amarillo))
       (t-verde (* ciclos-completos verde)) 
       
       (restos-color-rojo (min resto-segundos rojo))
       (resto1 (- resto-segundos restos-color-rojo))
       (restos-color-amarillo (min resto1 amarillo))
       (resto2 (- resto1 restos-color-amarillo))
       (restos-color-verde (min resto2 verde))
 
       (total-rojo (+ t-rojo restos-color-rojo))
       (total-amarillo (+ t-amarillo restos-color-amarillo))
       (total-verde (+ t-verde restos-color-verde))
       
    (mapcar #'(lambda (color tiempo-final) 
                (list color (* (/ tiempo-final tiempo-combinado) 100.0) '%))
               '(rojo amarillo verde)
                (list total-rojo total-amarillo total-verde))))


         ;REQUERIMIENTO 7.
    ;; ========================================================================
    ;; FUNCIÓN: duracion-color
    ;; NATURALEZA: Pura
    ;; ESTRATEGIA: Conditional / Case statement
    ;; IMPACTO: No destructiva
    ;; ========================================================================

  (defun duracion-color (estado)
    "Duración en segundos de cada estado del semáforo.
    Entrada: estado (símbolo)
     Salida: entero (segundos)"
     (case estado
      (en-rojo 90)
       (en-verde 120)
        (en-amarillo 6)
         (amarillo-intermitente 3)
          (otherwise 0)))
  
  ;; ========================================================================
    ;; FUNCIÓN: siguiente-estado
    ;; NATURALEZA: Pura
    ;; ESTRATEGIA: Conditional / Case statement
    ;; IMPACTO: No destructiva
  ;; ========================================================================

(defun siguiente-estado (estado-actual)
  "Calcula el siguiente estado en la secuencia con intermitencia.
   Entrada: estado-actual (símbolo)
   Salida: símbolo del siguiente estado"
  (case estado-actual
    (en-rojo 'amarillo-intermitente)
    (amarillo-intermitente 'en-verde)
    (en-verde 'amarillo-intermitente)
    (en-amarillo 'amarillo-intermitente)
    (otherwise 'en-rojo)))

