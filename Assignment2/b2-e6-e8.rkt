;;Plantilla b2-e6-e8.scm
;;Autor: <Nombre y apellidos>

;;Ejercicio 6
(he-tardado 210 'b2-e6)
;;<comentarios>
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
       (pasaTestNominal
          (cond
            ; Primero nos aseguramos que podemos usar car
            [(and (pair? t1) (pair? t2) (symbol? (car t1)) (symbol? (car t2))) #t]
            ; Si pasa el test nominal significa que es igual o mas general
            [(testNominal t1 t2) #t] 
            [(eq? t2 empty) #t]
            [else #f]))
       (pasaTestNumerico
        (cond
          ; Controlando casos con'(*)
          [(equal? (despejarInfinitos t1) (list '*)) #t]
          [(equal? (despejarInfinitos t1) (despejarInfinitos t2)) #t]
          [(equal? (despejarInfinitos t2) (list '*)) #f]
          ; Segun longitud
          [(> (length t1) (length t2)) #t]
          [(< (length t1) (length t2)) #f]
          [(and (number? (car t1)) (number? (car t2))) #t]
          ; Segun amplitud de rango
          [(> (- (valor (list-ref t1 1)) (valor (car t1)))
              (- (valor (list-ref t2 1)) (valor (car t2)))) #t]
          [else #f]
          )
        )
      )
      (if esTestNominal pasaTestNominal pasaTestNumerico)
    ))

;;Ejercicio 7
(he-tardado <minutos> 'b2-e7)
;;<comentarios>
(define (concepto-CL>= c1 c2)
  <codigo>)

;;Ejercicio 8
(he-tardado <minutos> 'b2-e8)
;;<comentarios>
(define (cmp-concepto-CL c1 c2)
  <codigo>)


