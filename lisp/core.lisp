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
      (t 'verde)
    )
  )
)

 ;;scala Trancisicion
      ;; colorActual, cambiarA, list son los argumentos que se utliza con la forma del argumento
def transicion(colorActual: String, cambiarA: String): List[String] = { 
        ;; ya que en scala no hay cond utilizamos if
  if (colorActual == "en-rojo" && cambiarA == "amarillo") {
    List(colorActual, "cambiar-a-amarillo")
  } else if (colorActual == "en-amarillo" && cambiarA == "verde") {
    List(colorActual, "cambiar-a-verde")
  } else if (colorActual == "en-verde" && cambiarA == "rojo") {
    List(colorActual, "cambiar-a-rojo")
  } else {
    List(colorActual, "accion-por-defecto")
  }
}
 def main(args: Array[String]): Unit = {

    println(transicion("en-rojo", "amarillo"))

  }
