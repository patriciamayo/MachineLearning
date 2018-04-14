;;Plantilla b2-e9-e14.scm
;;Autor: Patricia Mayo Tejedor

;;Ejercicio 9
(he-tardado 90 'b2-e9)
;; Un poco lioso al principio ya que he estado haciendolo muy complicado hasta que he encontrado list-set
(define tiposDeAtributo
  (lambda (nombre atributos)
    (define atributoEncontrado (assoc nombre atributos))
    (list-ref atributoEncontrado 1)))

(define (especializaciones-atributo-nominal2 concepto-CL indice metadatos)
  (let*
      ((atributoReferenciado (list-ref concepto-CL indice))
       (tiposDeAtributo (list-ref (list-ref metadatos indice) 1)))
    (define crearListaEspecializaciones
      (lambda (concepto-CL indice tiposDeAtributo listaEspecializada)
        (if (eq? (length tiposDeAtributo) (length listaEspecializada))
            listaEspecializada
            (let* (
                  (indiceDelTipo (length listaEspecializada))
                  (conceptoEspecializado (list-set concepto-CL indice (list (list-ref tiposDeAtributo indiceDelTipo))))
                  )
              (crearListaEspecializaciones
               concepto-CL
               indice
               tiposDeAtributo
               (append listaEspecializada (list conceptoEspecializado)))))))
    (if (equal? atributoReferenciado (list '*))
        (crearListaEspecializaciones concepto-CL indice tiposDeAtributo '())
        (list (list-set concepto-CL indice empty)))))


;;Ejercicio 10
(he-tardado 10 'b2-e10)
;; Ha sido bastante rapido ya que es practicamente eigual que el ejercicio 9
(define (generalizaciones-atributo-nominal concepto-CL indice metadatos)
  (let*
      ((atributoReferenciado (list-ref concepto-CL indice))
       (tiposDeAtributo (list-ref (list-ref metadatos indice) 1)))
    (define crearListaGeneralizaciones
      (lambda (concepto-CL indice tiposDeAtributo listaGeneralizada)
        (if (eq? (length tiposDeAtributo) (length listaGeneralizada))
            listaGeneralizada
            (let* (
                  (indiceDelTipo (length listaGeneralizada))
                  (conceptoGeneralizado (list-set concepto-CL indice (list (list-ref tiposDeAtributo indiceDelTipo))))
                  )
              (crearListaGeneralizaciones
               concepto-CL
               indice
               tiposDeAtributo
               (append listaGeneralizada (list conceptoGeneralizado)))))))
    (if (equal? atributoReferenciado empty)
        (crearListaGeneralizaciones concepto-CL indice tiposDeAtributo '())
        (list (list-set concepto-CL indice (list '*))))))

;;Ejercicio 11
(he-tardado 90 'b2-e11)
;; He tenido que modificar el ejercicio 1.
;; Se agradecerian mas ejemplos por ejemplo con (*) o con (20)
;; Espero haber entendido bien el ejercicio
(define ejemploEsPositivo
    (lambda (ejemplo)
      (eq? (last ejemplo) '+)))

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

; Estandarizar limites
(define estandarizarLimites
        (lambda (limites)
          (cond
            [(equal? limites (list '*))'(-inf.0 +inf.0)]
            [(= (length limites) 0) '(2 1)]
            [(= (length limites) 1) (list limites limites)]
            [else limites]
            )))

(define (generalizaciones-atributo-numerico concepto-CL indice ejemplo)
  (let*
      ((limiteEnConcepto (list-ref concepto-CL indice))
       (valorEnEjemplo (list-ref ejemplo indice)))
    (define (generalizarConcepto limiteEnConcepto valorEnEjemplo)
      (let* (
             (limitesEstandarizados (estandarizarLimites limiteEnConcepto))
             (inferior (car limitesEstandarizados))
             (superior (list-ref limitesEstandarizados 1)))
        (cond
          [(eq? limiteEnConcepto empty) (list valorEnEjemplo)]
          [(not (limiteInferior inferior valorEnEjemplo)) (list (list valorEnEjemplo) superior)]
          [(not (limiteSuperior superior valorEnEjemplo)) (list inferior (list valorEnEjemplo))]
          [else (list '*)]
          )
        ))
    (cond
      [(not (ejemploEsPositivo ejemplo)) concepto-CL]
      [(testNumerico limiteEnConcepto valorEnEjemplo) concepto-CL]
      [else (list-set concepto-CL indice (generalizarConcepto limiteEnConcepto valorEnEjemplo))]
      )
    ))

;;Ejercicio 12
(he-tardado 30 'b2-e12)
;; No me ha llevado mucho tiempo hacerlo ya que es parecido al ejercicio 11
;; Me queda la duda de que hacer en este caso
;; > (especializaciones-atributo-numerico '((*)(20)(20)(si)) 1 '(soleado 25 40 si -))
;; '((*) (20) (20) (si))
(define (especializaciones-atributo-numerico concepto-CL indice ejemplo)
    (let*
      ((limiteEnConcepto (list-ref concepto-CL indice))
       (valorEnEjemplo (list-ref ejemplo indice))
       (limitesEstandarizados (estandarizarLimites limiteEnConcepto))
       (inferior (car limitesEstandarizados))
       (superior (list-ref limitesEstandarizados 1)))
    (cond
      [(ejemploEsPositivo ejemplo) (list concepto-CL)]
      [(not (testNumerico limiteEnConcepto valorEnEjemplo)) (list concepto-CL)]
      [(eq? limiteEnConcepto empty) (list concepto-CL)]
      [else (list
             (list-set concepto-CL indice (list inferior valorEnEjemplo))
             (list-set concepto-CL indice (list valorEnEjemplo superior))
             )])
    ))

;;Ejercicio 13
(he-tardado 40 'b2-e13)
;; Poned ejemplos de lo que se espera por favor
;; Se pasa mas tiempo entendiendo que se espera del ejercicio que haciendolo
(define (generalizaciones-CL concepto-CL metadatos ejemplo)
   (define crearGeneralizaciones
     (lambda (concepto-CL metadatos ejemplo indice listaGeneralizaciones)
       (if (eq? indice (- (length ejemplo) 1))
           listaGeneralizaciones
           (let* (
                  (atributoConcepto (list-ref concepto-CL indice))
                  (atributoEjemplo (list-ref ejemplo indice))
                  (esNominal (list? (list-ref (list-ref metadatos indice) 1))))
             
             (if esNominal
                 (crearGeneralizaciones
                  concepto-CL
                  metadatos
                  ejemplo
                  (+ indice 1)
                  (append listaGeneralizaciones
                          (generalizaciones-atributo-nominal concepto-CL indice metadatos))
                 )
                 (crearGeneralizaciones
                  concepto-CL
                  metadatos
                  ejemplo
                  (+ indice 1)
                  (append listaGeneralizaciones
                          (list (generalizaciones-atributo-numerico concepto-CL indice ejemplo))))))
       )))
    (crearGeneralizaciones concepto-CL metadatos ejemplo 0 '()))

;;Ejercicio 14
(he-tardado 20 'b2-e14)
;; Parecido al ejercicio 13 
(define (especializaciones-CL concepto-CL metadatos ejemplo)
 (define crearEspecializaciones
     (lambda (concepto-CL metadatos ejemplo indice listaEspecializaciones)
       (if (eq? indice (- (length ejemplo) 1))
           listaEspecializaciones
           (let* (
                  (atributoConcepto (list-ref concepto-CL indice))
                  (atributoEjemplo (list-ref ejemplo indice))
                  (esNominal (list? (list-ref (list-ref metadatos indice) 1))))
             
             (if esNominal
                 (crearEspecializaciones
                  concepto-CL
                  metadatos
                  ejemplo
                  (+ indice 1)
                  (append listaEspecializaciones
                          (especializaciones-atributo-nominal concepto-CL indice metadatos))
                 )
                 (crearEspecializaciones
                  concepto-CL
                  metadatos
                  ejemplo
                  (+ indice 1)
                  (append listaEspecializaciones
                          (especializaciones-atributo-numerico concepto-CL indice ejemplo)))))
       )))
    (crearEspecializaciones concepto-CL metadatos ejemplo 0 '()))