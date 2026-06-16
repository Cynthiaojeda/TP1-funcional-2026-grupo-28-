;; REQUERIMIENTO 1
;;scala Transicion
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
;REQUERIMIENTO 2
;Scala transición
def timer(epoch: Int): String = { ; Define la funcion 
  val posicion = epoch % 216 ; Crea la varariable "posicion" que contiene el valo de epoch dividido 216
  
  if (posicion < 90) "rojo"
  else if (posicion < 96) "amarillo"
  else "verde"
}
