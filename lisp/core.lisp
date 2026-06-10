
;; FUNCIÓN: registrar-cambio
;; NATURALEZA: Impura (Produce efectos secundarios al imprimir en la terminal)
;; ESTRATEGIA: Directa / Ejecución Secuencial
;; IMPACTO: No destructiva
;; ========================================================================
(defun registrar-cambio (epoch color-actual cambiar-a)
  (format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%" epoch color-anterior color-nuevo))
