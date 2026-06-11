codigo lisp REQUERIMIENTO 1
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

REQUERIMIENTO 2
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
      (t 'verde))))

REQUERIMIENTO 3
;; ========================================================================
;; FUNCIÓN: registrar-cambio 
;; NATURALEZA: Impura 
;; ESTRATEGIA: Directa / Ejecución Secuencial
;; IMPACTO: No destructiva

(defun registrar-cambio (epoch color-actual cambiar-a)
  (format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%" epoch color-anterior color-nuevo))

REQUERIMIENTO 4a
;; ========================================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Orden Superior 
;; IMPACTO: No destructiva 
;; ========================================================================

(defun duracion-ciclo (tiempos-fase)
  (reduce #'+ tiempos-fase))

REQUERIMIENTO 4b
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


;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: Cálculo Directo
;; IMPACTO: No destructiva
;; ========================================================
(defun ciclos-por-tiempo (minutos rojo amarillo verde)
  (let ((total (* 60 minutos))
        (duracion (+ rojo amarillo verde)))
    (truncate total duracion)))
