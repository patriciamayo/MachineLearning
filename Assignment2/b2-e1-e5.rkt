;;Plantilla b2-e1-e5.scm
;;Autor: Patricia Mayo Tejedor

;;Ejercicio 1
(he-tardado 240 'b2-e1)
;; He creado funciones diferentes para cada tipo de test: Numerico y Nominal
;; y asi dividir mejor el trabajo de match-CL. En el caso de test numerico se crean dos sub tests
;; para abarcar ambos limites (superior e inferior). Ademas h etenido que estandarizar los limites,
;; por ejemplo '(*) realmente significa (-inf.0 +inf.0)

;> (match-CL '((soleado)(*)(10 40)(si)) '(soleado 30 40 si))
;#f
;> (match-CL '((soleado)(*)(10 (40))(si)) '(soleado 30 40 si))
;#t

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
      (define estandarizarLimites
        (lambda (limites)
          (cond
            [(equal? limites (list '*))'(-inf.0 +inf.0)]
            [(= (length limites) 0) '(2 1)]
            [(= (length limites) 1) (list limites limites)]
            [else limites]
            )))
      (define limitesEstandarizados (estandarizarLimites limites))
      (define inferior (car limitesEstandarizados))
      (define superior (list-ref limitesEstandarizados 1))
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
       (lambda (atributoTest atributoEjemplo)
         (cond
            [(number? atributoEjemplo) (testNumerico atributoTest atributoEjemplo)]
            [else (testNominal atributoTest atributoEjemplo)]))
       concepto-CL
       ejemplo-sin-clase
       ))))

;;Ejercicio 2
(he-tardado 60 'b2-e2)
;; Al principio no entendia lo que se pedia en el ejercicio
;; Despues he tenido que arreglar el Ejercicio 1 porque no manejaba bien '(*)
;; Problemas tambien cuando no encuentra ningun ejemplo con test positivo, ya que findf devolvia false en vez de una lista


; > (CLi '((*) (5 40) (subiendo) (estable) (10 60) (si)) '(bueno 20 subiendo estable 50 si ))
; '(bueno 20 subiendo estable 50 si +)


;; ======= PRIMERA IMPLEMENTACION MAL HECHA =========
(define (CLiMAL concepto-CL ejemplo-sin-clase)
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

;;  ======= EDIT  =======
;; lo habia hecho mal, ya que al CLi no le pasamos en ningun parametro el total de los ejemplos
;; la correcta implementacion seria comparandolo con la funcion match. Aunque me surge otro problema
;; Para el conjunto poker o lymphography, que tienen mas de dos clases, no funciona correctamente

(define (CLi concepto-CL ejemplo-sin-clase)
(let* ()
  (if (match-CL concepto-CL ejemplo-sin-clase)
      (append ejemplo-sin-clase (list '+))
      (append ejemplo-sin-clase (list '-))
      )))


;;Ejercicio 3
(he-tardado 15 'b2-e3)
;; No se puede dividir por cero como en la practica. Se ha optado por representar los infinitos con inf.0
(define CLgeneral '((*) (-inf.0 +inf.0) (*) (*) (-inf.0 +inf.0) (*)))
(define CLespecifico '(() (1 0) () () (1 0) ()))
(define CLcercano '((*) (18 +inf.0) (subiendo) (*) (5 60) (no)))

;;Ejercicio 4
(he-tardado 7 'b2-e4)
;> (define metadatos (car ejemplos))
;> (concepto-CL-mas-general metadatos)
;'((*) (*) (*) (*) (*) (*))
(define (concepto-CL-mas-general metadatos)
    (map
       (lambda (dato)
         (cond
            [(number? dato) (list '-inf.0 '+inf.0)]
            [else (list '*)]))
       metadatos))

;;Ejercicio 5
(he-tardado 2 'b2-e5)
;> (concepto-CL-mas-especifico metadatos)
;'(() () () () () () ())
 (define (concepto-CL-mas-especifico metadatos)
    (map
       (lambda (dato)
         (cond
            [(number? dato) (list 1 0)]
            [else '()]))
       metadatos))
