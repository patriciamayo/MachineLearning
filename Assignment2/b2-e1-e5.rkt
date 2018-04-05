;;Plantilla b2-e1-e5.scm
;;Autor: Patricia Mayo Tejedor

;;Ejercicio 1
(he-tardado 240 'b2-e1)
;;<comentarios>
(define testNominal
    (lambda (atributoTest atributoEjemplo)
      (cond
        [(eq? atributoTest empty) #f]
        [(equal? atributoTest (list '*)) #t]
        [(eq? (car atributoTest) atributoEjemplo) #t]
        [else #f])
      ))


(define testNumerico
    (lambda (limites valorEjemplo)
      (define inferior (car limites))
      (define superior (list-ref limites 1))
      ; Test limite inferior
      (define limiteInferior
        (lambda (limite valor)
          (cond
            [(number? limite) (> valor limite)]
            [(list? limite) (>= valor (car limite))]
            [else #f])))
      ; Test limite superior
      (define limiteSuperior
        (lambda (limite valor)
          (cond
            [(number? limite) (< valor limite)]
            [(list? limite) (<= valor (car limite))]
            [else #f])))
      
      (and (limiteInferior inferior valorEjemplo) (limiteSuperior superior valorEjemplo))))

(define (match-CL concepto-CL ejemplo-sin-clase)
    (let* ()
      (if (not(= (length concepto-CL) (length ejemplo-sin-clase))) #f
      (andmap
       (lambda (atributoTest atributoEjemplo atributoTipo)
         (define tipoDeTest (list-ref atributoTipo 1))
         (cond
            [(eq? tipoDeTest 'numerico) (testNumerico atributoTest atributoEjemplo)]
            [(list? tipoDeTest) (testNominal atributoTest atributoEjemplo)]
            [else #f]))
       concepto-CL
       ejemplo-sin-clase
       (drop-right atributos 1)))))

;;Ejercicio 2
(he-tardado 60 'b2-e2)
;; Al principio no entendia lo que se pedia en el ejercicio
;; Despues he tenido que arreglar el Ejercicio 1 porque no manejaba bien '(*)
;; Problemas tambien cuando no encuentra ningun ejemplo con test positivo, ya que findf devolvia false en vez de una lista
(define (CLi concepto-CL ejemplo-sin-clase)
 (let*
     ( ;; Variables 
      (casos (cdr ejemplos))
      (ejemploQuePasaTest (findf
                          (lambda (caso)
                            (define casoSinClase (drop-right caso 1))
                            (match-CL concepto-CL casoSinClase)) casos))
      (tiposDeClases (atributo 'clase ejemplos)))
   ;; Devolver clase segun el ejemplo que ha pasado el test
   (define clase
       (if (list? ejemploQuePasaTest)
           (list-ref ejemploQuePasaTest (- (length ejemploQuePasaTest) 1))
           (obtener-al-azar tiposDeClases)))
   (append ejemplo-sin-clase (list clase))))

;;Ejercicio 3
(he-tardado 15 'b2-e3)
;; No se puede dividir por cero como en la practica. Se h aoptado por representar los infinitos con inf.0
(define CLgeneral '((*) (-inf.0 +inf.0) (*) (*) (-inf.0 +inf.0) (*)))
(define CLespecifico '(() (1 0) () () (1 0) ()))
(define CLcercano '((*) (18 +inf.0) (subiendo) (*) (5 60) (no)))

;;Ejercicio 4
(he-tardado 7 'b2-e4)
;;<comentarios>
(define (concepto-CL-mas-general metadatos)
    (map
       (lambda (dato)
         (cond
            [(number? dato) (list '-inf.0 '+inf.0)]
            [else (list '*)]))
       metadatos))

;;Ejercicio 5
(he-tardado 2 'b2-e5)
;;<comehtarios>
 (define (concepto-CL-mas-especifico metadatos)
    (map
       (lambda (dato)
         (cond
            [(number? dato) (list 1 0)]
            [else '()]))
       metadatos))
