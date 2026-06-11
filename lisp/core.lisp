;Código del TP1
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

