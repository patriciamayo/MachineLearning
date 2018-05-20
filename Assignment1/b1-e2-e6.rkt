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

(require srfi/1)

;; Los ejercicios del tema 1 no sabia que teniamos una plantilla
;; Por lo que no podido ir mirando el tiempo, mas o menos pongo lo que creo que me costo

;;Ejercicio 2
(he-tardado 30 'b1-e2)
;; Para realizar este ejercicio lo primero ha sido aprender como se define una lista o funciones en Scheme/Racket.
;; (define <nombreDeFuncion> <funcion>)
;; Por lo que crear la lista nos queda: (define lista (list 1 2 3 4))
;; La función en sí se declara: (lambda (<parametroEntrada>) (<cuerpo/Salida>))
;; El <parametroEntrada> será la lista que creemos y para el <cuerpo/Salida> usamos la función map,
;; que aplica un procedimiento determinado a cada uno de los elementos de una lista, devolviendo una lista nueva:
;; (map <funcion> <lista>)La función recibe un número y lo devuelve sumando uno:
;; (lambda (number) (+ 1 number))

;> (siguiente '(1 3 5))
;'(2 4 6)
(define (siguiente lista)
 (let*()
   (map (lambda (number) 
           (+ 1 number)) lista )
   ))

;;Ejercicio 3
(he-tardado 20 'b1-e3)
;; De forma similar al ejercicio anterior, esta vez iterando en dos listas al mismo tiempo.

;> (sumas '(1 3 5) '(2 4 6))
;'(3 7 11)
(define (sumas lista1 lista2)
 (let*()
   (map
     (lambda (numero1 numero2) 
       (+ numero1 numero2)) lista1 lista2)))

;;Ejercicio 4
(he-tardado 20 'b1-e4)
;; En el capítulo 6 sobre recursión en el libro “Teach Yourself Scheme in Fixnum Days” (Sitaram, 2003),
;; se explica paso a paso el ejemplo de factorial
;> (factorial 3)
;6
(define (factorial numero)
  (let*()
    (if (= numero 0) 1 
        (* numero (factorial(- numero 1))))))

;;Ejercicio 5
(he-tardado 180 'b1-e5)
;; Este ejercicio me ha costado bastante, porque al principio no entendía que tenía que hacer ni cuál era el objetivo.
;; Como consejo para futuros años, explicaría mejor las instrucciones y el ejemplo, que debería funcionar sin tener que retocarlo.
;; Lo primero ha sido coger todas las frecuencias y sumarlas, para ver el total de la distribución.
;; Con la variable random cogemos una muestra aleatoria, y al multiplicarla por el total de frecuencias
;; (que hemos sumado antes) convertimos el valor (entre 0 y 1) a uno equivalente en nuestra distribución (en nuestro caso entre 0 y 6).
;; Por último solo queda buscar en que rango cae en la función inversa.

;;            a   0 ≤x ≤1
;; F^(-1)(u)  b   1 <x ≤3
;;            c   3 <x ≤6

; > (obtener-al-azar0 '((a . 1)(b . 2)(c . 3)))
;'b
(define (obtener-al-azar0 lista)
 (let* ()
   (define frecuencias (map (lambda (par) (cdr par)) lista))
      (define total (apply + frecuencias))
      (define variableAleatoria (random))
      (define frecuenciaAleatoria (* total variableAleatoria))
      (define encontrarLimite
        (lambda (index acumulada lista frecuencia)
          (if (>= acumulada frecuencia)
              (car (list-ref lista index))
              (encontrarLimite (+ index 1)
                               (+ acumulada (cdr (list-ref lista (+ index 1))))
                               lista
                               frecuencia))))
      (encontrarLimite 0 (cdr (list-ref lista 0)) lista frecuenciaAleatoria)
   ))

;;Ejercicio 6
(he-tardado 90 'b1-e6)
;; Se comprueba si el parámetro lista es correcto cogiendo el primer elemento y viendo si es de tipo par.
;; Sí es tipo par entonces la lista es correcta tal cual. Si no lo es, rehacemos la lista para que contenga pares,
;; asignando a cada elemento la frecuencia 1.

;> (obtener-al-azar '(a b c))
;'c
(define (obtener-al-azar lista)
 (let*()
   (define listaCorrecta
             (lambda (lista)
          	 (if (and (pair? (list-ref lista 0)) (real? (cdr (list-ref lista 0))))
              		lista
              		(map (lambda (elemento) (cons elemento 1)) lista))))
      	   (obtener-al-azar0 (listaCorrecta lista))
   ))
