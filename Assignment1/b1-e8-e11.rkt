;;Plantilla b1-e8-e11
;;AUTOR: 
;;Copyright (C) 2008  Patricia Mayo Tejedor

;;This program is free software: you can redistribute it and/or modify
;;it under the terms of the GNU General Public License as published by
;;the Free Software Foundation, either version 3 of the License, or
;;(at your option) any later version.

;;This program is distributed in the hope that it will be useful,
;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;GNU General Public License for more details.

;;You should have received a copy of the GNU General Public License
;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


; Ejercicio 7
(define direccionEjemplos1 "/Users/patriciamayotejedor/Documents/Private Development/UNED/Machine Learning/MachineLearning/ejemplos.scm")

;;Ejercicio 8
(he-tardado 20 'b1-e8)
;; Se debe especificar el camino completo al archivo, por ejemplo: (define ejemplos (leer-ejemplos "/Users/Patricia/Documents/UNED/Machine Learning/tema1/ejemplos.scm"))
;; >(leer-ejemplos direccionEjemplos1)
(define (leer-ejemplos archivo)
(let*()
  (call-with-input-file archivo read)))

;;Ejercicio 9
;; Para llamar a la función se hace necesario que el ejemplo sea en sí una lista,
;; por lo que la función se usaría de la siguiente manera: (anadir-ejemplo ejemplos '(niebla 3 bajando estable 90 si -))
; >(anadir-ejemplo ejemplos '(frio 4 bajando subiendo 23 no +))
(he-tardado 60 'b1-e9)
(define (anadir-ejemplo ejemplos ejemplo)
  (let* ()
    (append ejemplos (list ejemplo))))

;;Ejercicio 10
;;> (atributo 'perspectiva ejemplos)
;'(lluvia variable bueno frio niebla calorSeco)
(he-tardado 100 'b1-e10)
(define (atributo nombre-atributo ejemplos)
(let* ((atributos (list-ref ejemplos 0))
       (atributoEncontrado (assoc nombre-atributo atributos)))
    (list-ref atributoEncontrado 1)
  ))
;;Por que debemos escribir 'perspectiva y no simplemente perspectiva.
;;Para llamar a la función tenemos que pasar el nombre del atributo con comilla,
;;porque buscamos el valor del string en sí, y no el valor que pudiese guardar como variable.

;;Ejercicio 11
;; Se ha dado por entendido que ambas listas contienen la misma definición de atributos.
(he-tardado 40 'b1-e11)
(define (mezclar ejemplos1 ejemplos2)
  (let*((atributos (list-ref ejemplos1 0))
        (ejemplosJuntos (append (list-tail ejemplos1 1) (list-tail ejemplos2 1))))
    (append (list atributos) ejemplosJuntos)))
