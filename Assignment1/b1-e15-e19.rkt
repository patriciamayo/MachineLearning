;;Plantilla b1-e15-e19.scm
;;Autor: Patricia Mayo Tejedor

;;Ejercicio 15
(he-tardado 100 'b1-e15)
;; Si hay errores y es natural que hayan, el intérprite por extension simplemente ha entendido
;; que todos los casos son de la clase mayoritaria (en nuestros ejemplos sería la clase “+”),
;; por lo que el resto de clases han salido como errores.

;; He tardado sobre todo por hacer que funcionase el agoritmo A0 y po rnetender bien el proceso
;(define esencia (A0 ejemplos))
;(define casos (list-tail ejemplos 1))
;(define ejemplos-sin-clase (map (lambda(x) (drop-right x 1)) casos))
;(define extension (map (lambda(x) (A0i esencia x)) ejemplos-sin-clase))


;;Ejercicio 16
(he-tardado 100 'b1-e16)
;;> (precision (cdr ejemplos) (cdr extension))
;;0.6
;;> (error (cdr ejemplos) (cdr extension))
;;0.4

; Precision es el numero de aciertos dividido por el numero total de casos
(define precision
  (lambda (casosEjemplos casosExtension)
  ;(define casosEjemplos (cdr ejemplos))
  ;(define casosExtension (cdr extension))
  (define numeroCasos (length casosEjemplos))
  (define numeroAciertos (length (filter (lambda (caso) caso) (map (lambda (casoEjemplo casoExtension)
                                    (eq? (last casoEjemplo) (last casoExtension))) casosEjemplos casosExtension))

                                 ))
  (exact->inexact (/ numeroAciertos numeroCasos))))

; Error es el numero de errores dividido por el numero total de casos, o lo que es lo mismo, 1 - precision
(define error
  (lambda (casosEjemplos casosExtension)
  (define precisionTotal (precision casosEjemplos casosExtension))
  (- 1 precisionTotal)))

;;Ejercicio 17
(he-tardado 90 'b1-e17)

;; Estos nuevos ejemplos no estaban entre los conceptos aprendidos,
;; he puesto aposta más ejemplos con días malos (al contrario que en los primeros ejemplos
;; donde predominaban los días buenos), por lo que la precisión no ha sido la misma.
;; Aún así no ha estado mal del todo, y en un set más grande seguramente la precisiones
;; se equilibrarían.

;;============== Ejemplos Nuevos ==============
;;(lluvia 2 bajando bajando 75 si -)
;;(lluvia 28 estable bajando 65 no -)
;;(variable 22 subiendo bajando 30 no +)
;;(niebla 8 bajando estable 60 no -) 
;;(bueno 26 estable subiendo 43 si +)

;;============== Preparar ejemplos ==============
;(define ejemplos2 (leer-ejemplos direccionEjemplos2))
;(define ejemplos-sin-clase2 (map (lambda(x) (drop-right x 1)) ejemplos2))
;; Pasar ejemplos por el interprete A0i
;(define extension2 (map (lambda(x) (A0i esencia x)) ejemplos-sin-clase2))

;;============== Definir precision y error ==============
;(define precisionEjemplos2 (precision (cdr ejemplos2) (cdr extension2)))
;(define errorEjemplo2 (error (cdr ejemplos2) (cdr extension2)))


;;============== PRECISION ==============
;> (precision (cdr ejemplos) (cdr extension)) ; ejemplos1
;0.6
;> precisionEjemplos2
;0.4

;;Ejercicio 18
(he-tardado 120 'b1-e18)
;> (A1 ejemplos)
;'((+ . 12) (- . 8))
;(define esenciaA1 (A1 ejemplos))
;(define extensionA1i (map (lambda(x) (A1i esenciaA1 x)) ejemplos-sin-clase))
;(define precisionEjemplosA1 (precision (cdr ejemplos) (cdr extensionA1i)))
;> precisionEjemplosA1
;3/5

(define (A1 ejemplos)
 (let*
     (;;Asignacion de VARIABLES locales.
       ;;==================================
      (casos (list-tail ejemplos 1))
      (tiposDeClases (atributo 'clase ejemplos))

      ; Devuelve una lista con tantos elementos como clases hay. Cada elemento es un par, con la clase y la frecuencia en la que aparece en la lista 
      (paresClaseFrecuencia
       (lambda (tiposDeClases clasesAgrupadas)
         (map (lambda (tipo grupo)(cons tipo (length grupo))) tiposDeClases clasesAgrupadas)))
      )
   ; Devuelve una lista con tantos elementos como clases hay. Cada elemento contiene todos los casos de esa clase
   (define separarClases
           (lambda (index tiposDeClases casos agrupacion)
             (if (= index (length tiposDeClases))
                 agrupacion
                 (let ((claseAgrupada  (filter (lambda (caso)
                                                 (eqv? (list-ref caso (- (length caso) 1)) (list-ref tiposDeClases index)))
                                               casos)))
                   (separarClases (+ index 1) tiposDeClases casos (append agrupacion (list claseAgrupada)))))))
   (define casosAgrupadosPorClase (separarClases 0 tiposDeClases casos '()))
   (paresClaseFrecuencia tiposDeClases casosAgrupadosPorClase))
  )

; A1i
(define (A1i paresClaseFrecuencia ejemplo-sin-clase)
  (let* ()
  (append ejemplo-sin-clase (list (obtener-al-azar0 paresClaseFrecuencia)))))

;;Ejercicio 19
(he-tardado 40 'b1-e19)
;; Aunque no se han apreciado diferencias en la precisión cuando se ha comparado con los ejemplos de entrenamiento,
;; se puede ver que sí que ha aumentado la precisión (de 2/5 a 3/5) cuando se han introducido casos nuevos.
;; El algoritmo A1/A1i es más general y aplicable incluso con casos nuevos no entrenados por lo que si se considera rentable.

;; Pasar ejemplos por el interprete A1i ==================================
;  (define extensionA1iEjemplos2 (map (lambda(x) (A1i esenciaA1 x)) ejemplos-sin-clase2))
;; Definir precision y error ==================================
;  (define precisionA1Ejemplos2 (precision (cdr ejemplos2) (cdr extensionA1iEjemplos2)))
;  (define errorA1Ejemplo2 (error (cdr ejemplos2) (cdr extensionA1iEjemplos2)))

;> precisionA1Ejemplos2
;3/5