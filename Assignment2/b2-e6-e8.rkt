;;Plantilla b2-e6-e8.scm
;;Autor: Patricia Mayo Tejedor


;;Ejercicio 6
(he-tardado 300 'b2-e6)

;; IMPORTANTE: no sabia si se tenia que tener en cuenta la amplitud del rango
;; en el caso de test numericos, mismo numeros en el rango, pero si es cerrado abarca mas que abierto
;; Si se debe tener en cuenta descomentar la parte del segun el rango

;; Este ejercicio ha sido muy muy complicado y lioso
;; Los ejemplos no aclaran mucho cuantas categorias hay por lo que hay veces que no se entiende
;; Habia un adjunto de aclaraciones en "Material de Estudio" que me termino por liar muchisimo mas
;; He ido haciendo cambios segun continuaba luego con otros ejercicios

; A mi entender hay cuatro categorias, de general a mas especifica:
;    (*) que acepta todo
;    (valor) que acepta sólo el valor
;    (min max) que acepta un rango de valores entre min y max
;    () que no acepta nada

;>  (test-CL>= '() '())
;#t
;>  (test-CL>= '() '(lluvia))
;#f
;>  (test-CL>= '(lluvia) '())
;#t
;>  (test-CL>= '(lluvia) '(soleado))
;#t
;> (test-CL>= '(lluvia) '(*))
;#f
;> (test-CL>= '(*) '(lluvia))
;#t
;> (test-CL>= '(*) '(*))
;#t
;> (test-CL>= '(25 30) '(26))
;#t
;> (test-CL>= '(25 30) '(21 25))
;#t
;> (test-CL>= '(26) '(25 30))
;#f

(define atributoNominal
    (lambda (t1 t2)
      (cond
            [(not (pair? t1)) #t]
            [(not (pair? t2)) #t]
            [(and
              (and (not (number? (car t1)))
                   (not (list?  (car t1))))
              (and (not (number? (car t2)))
                   (not (list?  (car t2))))) #t]
            [else #f])))

(define (test-CL>= t1 t2)
  (let*
      (
       (esTestNominal (atributoNominal t1 t2))
       ; Despejar caso infinito
       (despejarInfinitos
        (lambda (rango)
          (if (equal? rango '(-inf.0 +inf.0)) (list '*) rango)
          ))
       ; Quitar 0.1 a los limites abiertos
       (valor
        (lambda (limite)
          (if (number? limite) (- limite 0.1) (car limite))
          ))
      )
  (define pasaTestNominal
   (lambda (t1 t2)
     (cond
            ; Primero nos aseguramos que podemos usar car
            ;[(and (pair? t1) (pair? t2) (symbol? (car t1)) (symbol? (car t2))) #t]
            ; Si pasa el test nominal significa que es igual o mas general
            [(testNominal t1 t2) #t] 
            [(eq? t2 empty) #t]
            [(equal? t2 (list '*)) #f]
            [(and (pair? t1) (pair? t2) (symbol? (car t1)) (symbol? (car t2))) #t]
            [else #f])
     ))
       (define pasaTestNumerico
         (lambda (t1 t2)
           (cond
             ; Controlando casos con'(*)
             [(equal? (despejarInfinitos t1) (list '*)) #t]
             [(equal? (despejarInfinitos t1) (despejarInfinitos t2)) #t]
             [(equal? (despejarInfinitos t2) (list '*)) #f]
             ; Segun longitud
             [(> (length t1) (length t2)) #t]
             [(< (length t1) (length t2)) #f]
             ;[(and (number? (car t1)) (number? (car t2))) #t]



             ;; ============= IMPORTANTE ==========
             ; Si esto deberia dar false 
             ;> (test-CL>= '(25 30) '(25 (30)))
             ;#f
             ;descomentar la siguiente parte de amplitud de rango
             ;; ======================================


             
             ; Segun amplitud de rango
             ;[(> (- (valor (list-ref t1 1)) (valor (car t1)))
             ;    (- (valor (list-ref t2 1)) (valor (car t2)))) #t]
             ;[else #f]
             [else #t]
          )))
      (if esTestNominal (pasaTestNominal t1 t2) (pasaTestNumerico t1 t2))
    ))


;;Ejercicio 7
(he-tardado 60 'b2-e7)
;; Tenia errores en el ejercicio anterior asique he tenido que rehacerlo
;> (concepto-CL>= '((lluvioso)(2 10)) '((soleado)(10 20)))
;#t
;> (concepto-CL>= '((lluvioso)(2 35)) '((soleado)(23)))
;#t
;> (concepto-CL>= '((soleado)(23)) '((lluvioso)(2 35)))
;#f
(define (concepto-CL>= c1 c2)
    (andmap (lambda (concepto1 concepto2)
                                (test-CL>= concepto1 concepto2)) c1 c2))

;;Ejercicio 8
(he-tardado 30 'b2-e8)
;; De nuevo me he dado cuenta que tenia mal el ejercicio 6
;; Ya que para evaluar el grado de generalizacion estaba mirando la amplitud del rango

;> (cmp-concepto-CL '((lluvioso)(2 35)) '((soleado)(23)))
;1
;> (cmp-concepto-CL '((lluvioso)(2 10)) '((soleado)(10 20)))
;0
;> (cmp-concepto-CL '((soleado)(23)) '((lluvioso)(2 35)))
;-1

(define (cmp-concepto-CL c1 c2)
    (cond
      [(and (concepto-CL>= c1 c2) (not (concepto-CL>= c2 c1))) 1]
      [(and (concepto-CL>= c1 c2) (concepto-CL>= c2 c1)) 0]
      [(and (concepto-CL>= c2 c1) (not (concepto-CL>= c1 c2))) -1] 
      )
  )


