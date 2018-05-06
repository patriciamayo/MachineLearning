#lang racket
(require srfi/1)

(begin

; Ejercicio 2
(define siguiente
  (lambda (lista)
    (map (lambda (number) 
           (+ 1 number)) lista )))

; Ejercicio 3
(define sumas
  (lambda (lista1 lista2)
    (map
     (lambda (numero1 numero2) 
       (+ numero1 numero2)) lista1 lista2)))

; Ejercicio 4
(define factorial 
  (lambda (n) 
    (if (= n 0) 1 
        (* n (factorial(- n 1))))))

; Ejercicio 5
(define obtener-al-azar0
  (lambda (lista)
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

; Ejercicio 6
(define obtener-al-azar
   (lambda (lista)
     	 (define listaCorrecta
             (lambda (lista)
          	 (if (and (pair? (list-ref lista 0)) (real? (cdr (list-ref lista 0))))
              		lista
              		(map (lambda (elemento) (cons elemento 1)) lista))))
      	   (obtener-al-azar0 (listaCorrecta lista))))

; Ejercicio 7
(define direccionEjemplos1 "/Users/patriciamayotejedor/Documents/Private Development/UNED/Machine Learning/MachineLearning/ejemplos.scm")


; Ejercicio 8
(define leer-ejemplos
  (lambda (archivo)
    (call-with-input-file archivo read)
    ))
(define ejemplos (leer-ejemplos direccionEjemplos1))

; Ejercicio 9
(define anadir-ejemplo
  (lambda (lista ejemplo)
    (append lista (list ejemplo))))

; Ejercicio 10
(define atributo
  (lambda (nombre lista)
    (define atributos (list-ref lista 0))
    (define atributoEncontrado (assoc nombre atributos))
    (list-ref atributoEncontrado 1)
    ))

; Ejercicio 11
(define mezclar
  (lambda (lista1 lista2)
    (define atributos (list-ref lista1 0))
    (define ejemplosJuntos
      (append (list-tail lista1 1) (list-tail lista2 1)))
    (append (list atributos) ejemplosJuntos)
    ))

; Ejercicio 12
(define separar
  (lambda (proporcion lista)
    (define casos (list-tail lista 1))
    (define tamanoSublista1 (exact-round (* proporcion (length casos))))
    (define crearSublista2
      (lambda (contador casos)
        (if (= contador 0)
            casos
            (crearSublista2 (- contador 1)
                           (remq (obtener-al-azar casos) casos))
            )
        ))
    (define sublista2 (crearSublista2 tamanoSublista1 casos))
    (define sublista1 (set-subtract casos sublista2))
    (list sublista1 sublista2)
    ))

; Ejercicio 13
(define folds
  (lambda (numeroFolds lista)
    (define casos (list-tail lista 1))
    (define tamanoSublistas (quotient (length casos) numeroFolds))
    (define sublistasCon1Mas (modulo (length casos) numeroFolds))
    (define crearSublista
      (lambda (contador casos sublista)
        (if (= contador 0)
            sublista
            (let ((quitar (obtener-al-azar casos)))
                (crearSublista (- contador 1)
                           (remq quitar casos)
                           (append sublista (list quitar)))))
        ))
    ; define cuantos extra elementos tendra la sublista
    (define elementosExtras
                  (lambda (sublistasCon1Mas)
                    (if (> sublistasCon1Mas 0) 1
                        0)))
    (define crearFolds
      (lambda (numeroFolds tamanoSublistas sublistasCon1Mas foldsHechos casos)
        (if (= 0 numeroFolds)
            foldsHechos
            (let ((tamanoSublistas tamanoSublistas)
                  (elementosExtras (elementosExtras sublistasCon1Mas)))
              (define sublista (crearSublista (+ tamanoSublistas elementosExtras) casos '()))
              (crearFolds (- numeroFolds 1)
                          tamanoSublistas
                          (- sublistasCon1Mas elementosExtras)
                          (append foldsHechos (list sublista))
                          (set-subtract casos sublista))))
        ))
    (crearFolds numeroFolds tamanoSublistas sublistasCon1Mas '() casos)
    ))

; Ejercico 14
(define stratify
  (lambda (numeroFolds ejemplos)
    (define casos (list-tail ejemplos 1))
    (define tamanoSublistas (quotient (length casos) numeroFolds))
    (define sublistasCon1Mas (modulo (length casos) numeroFolds))
    (define tiposDeClases (atributo 'clase ejemplos))

    ; PREPARACION
    ; Devuelve una lista con tantos elementos como clases hay. Cada elemento contiene todos los casos de esa clase
    (define separarClases
           (lambda (index tiposDeClases casos agrupacion)
             (if (= index (length tiposDeClases))
                 agrupacion
                 (let ((claseAgrupada  (filter (lambda (caso)
                                                 (eqv? (list-ref caso (- (length caso) 1)) (list-ref tiposDeClases index)))
                                               casos)))
                   (separarClases (+ index 1) tiposDeClases casos (append agrupacion (list claseAgrupada)))))))

    ; Devuelve una lista con tantos elementos como clases hay. Cada elemento es un par, con la clase y la frecuencia en la que aparece en la lista 
    (define paresClaseFrecuencia
      (lambda (tiposDeClases clasesAgrupadas)
        (map (lambda (tipo grupo)(cons tipo (length grupo))) tiposDeClases clasesAgrupadas)))

    ; MANIPULACION DE CASOS
    ; Elimina el caso dado de las clases agrupadas
    (define quitarCaso
      (lambda (casoParaEliminar clasesAgrupadas)
        (map (lambda (casosDeClase)
               (remq casoParaEliminar casosDeClase))
             clasesAgrupadas)))

    ; Encuentra todos los casos de una clase. Si no quedan mas casos de esa clase, escoge de otra al azar
    (define encontrarCasosDeClase
      (lambda (clase clasesAgrupadas clasesConFrecuencia)
        (define casosDeClaseEncontrados
          (filter
           (lambda (casosDeClase)
             (if (= (length casosDeClase) 0)
                 #f
                 (let* ([primerCaso (list-ref casosDeClase 0)]
                        [claseDelPrimerCaso (list-ref primerCaso (- (length primerCaso) 1))])
                        (eq? clase claseDelPrimerCaso))))
          clasesAgrupadas))
        (if (= (length casosDeClaseEncontrados) 0)
          (encontrarCasosDeClase (obtener-al-azar0 clasesConFrecuencia) clasesAgrupadas clasesConFrecuencia)
          (list-ref casosDeClaseEncontrados 0 ))))
    
    ; Devuelve un elemento al azar de la clase dada
    (define obtenerAlAzarDeClase
      (lambda (clase clasesAgrupadas)
        (define encontrarCasosDeClase
          (filter (lambda (casosDeClase)
                    (define primerCaso (list-ref casosDeClase 0))
                    (define claseDelPrimerCaso (list-ref primerCaso (- (length primerCaso) 1)))
                    (eq? clase claseDelPrimerCaso))
                  clasesAgrupadas))
        (obtener-al-azar (list-ref encontrarCasosDeClase 0 ))))
    
    ; CREACION DE FOLDS
    ; Define cuantos extra elementos tendra una sublista (cuando no se puedan dividir exactos)
    (define elementosExtras
                  (lambda (sublistasCon1Mas)
                    (if (> sublistasCon1Mas 0) 1
                        0)))
    
    ; Dado el tamanio deseado, se crea una sublista con proporcion ademas de devolver los casos sobrantes
    (define crearSublista
      (lambda (numeroElementos casosAgrupadosPorClase paresClaseFrecuencia sublista)
        (if (= numeroElementos 0)
            (list sublista casosAgrupadosPorClase)
            (let* ([claseElegida (obtener-al-azar0 paresClaseFrecuencia)]
                   [casoElegido (obtener-al-azar (encontrarCasosDeClase claseElegida casosAgrupadosPorClase paresClaseFrecuencia))])
                (crearSublista (- numeroElementos 1)
                           (quitarCaso casoElegido casosAgrupadosPorClase) 
                           paresClaseFrecuencia 
                           (append sublista (list casoElegido)))))
        ))

    ; Se crean los folds de manera recursiva
    (define crearFolds
      (lambda (numeroFolds tamanoSublistas sublistasCon1Mas foldsHechos casosAgrupadosPorClase paresClaseFrecuencia)
        (if (= 0 numeroFolds)
            foldsHechos
            (let ((tamanoSublistas tamanoSublistas)
                  (elementosExtras (elementosExtras sublistasCon1Mas)))
              (define separarSublistaDeCasos (crearSublista (+ tamanoSublistas elementosExtras) casosAgrupadosPorClase paresClaseFrecuencia '()))
              (define sublista (list-ref separarSublistaDeCasos 0))
              (define casosSobrantes (list-ref separarSublistaDeCasos 1))
              (crearFolds (- numeroFolds 1)
                          tamanoSublistas
                          (- sublistasCon1Mas elementosExtras)
                          (append foldsHechos (list sublista))
                          casosSobrantes
                          paresClaseFrecuencia)))
        ))

    (define casosAgrupadosPorClase (separarClases 0 tiposDeClases casos '()))
    (define clasesConFrecuencia (paresClaseFrecuencia tiposDeClases casosAgrupadosPorClase))
    (crearFolds numeroFolds tamanoSublistas sublistasCon1Mas '() casosAgrupadosPorClase clasesConFrecuencia)
  )
)

; A0
(define (A0 ejemplos)
  (let*
      (;;Asignacion de VARIABLES locales.
       ;;==================================
       (atributos (car ejemplos))
       (casos (cdr ejemplos))
       (indice-clase ;el ndice de 'clase en la lista atributos.
        (index-where (map first atributos) (lambda(x) (eq? x 'clase))))
       (clases-posibles
        (second ;solo interesa el conjunto de valores.
         (list-ref atributos indice-clase)))
       ;;variable que mantiene la cuenta de las apariciones de cada clase.
       (clases-contabilizadas
        ;;Como primer paso, se inicializa a 0 la cuenta de cada clase.
        (map (lambda(clase) (cons clase 0)) clases-posibles))
       (concepto (list ));variable sin asignacion, de momento
       ;;Asignacion de FUNCIONES locales.
       ;;==================================
       ;;funcion que admite como parametro un ejemplo, el cual utiliza
       ;;para actualizar la contabilizacion de clases.
       (actualizar-contabilizacion
        (lambda(ejemplo)
          (let ((clase-del-ejemplo (list-ref ejemplo indice-clase)))
            (set! clases-contabilizadas
                  (map (lambda(x)
                         (if (eq? (car x) clase-del-ejemplo)
                             ;then
                             (cons (car x) (+ (cdr x) 1))
                             ;else
                             x))
                       clases-contabilizadas
                       )))))
       );fin de las asignaciones let*
    ;;Ahora, por cada ejemplo de entrenamiento,
    ;; se actualiza la contabilizacion de clases.
    (for-each actualizar-contabilizacion ejemplos)
    ;;Finalmente se escoge la clase que mas veces ha aparecido:
    ;;;primero, se obtiene el numero maximo;
    (set! concepto (apply max (map cdr clases-contabilizadas)))
    ;;;segundo, se obtiene la clase con ese numero maximo
    ;;; (almacenado temporalmente en la variable concepto).
    (set! concepto (first (find (lambda(x) (= (cdr x) concepto)) clases-contabilizadas)))
    ;;Y por ultimo se devuelve el concepto inducido por el algoritmo.
    concepto
    ))

; A0i
(define (A0i concepto ejemplo-sin-clase)
  (append ejemplo-sin-clase (list concepto)))

;  Ejercicio 15
(define esencia (A0 ejemplos))
(define casos (list-tail ejemplos 1))
(define ejemplos-sin-clase (map (lambda(x) (drop-right x 1)) casos))
(define extension (map (lambda(x) (A0i esencia x)) ejemplos-sin-clase))

;  Ejercicio 16
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

;  Ejercicio 17
(define direccionEjemplos2 "/Users/patriciamayotejedor/Documents/Private Development/UNED/Machine Learning/MachineLearning/ejemplos2.scm")
;; Preparar ejemplos
(define ejemplos2 (leer-ejemplos direccionEjemplos2))
(define ejemplos-sin-clase2 (map (lambda(x) (drop-right x 1)) ejemplos2))
;; Pasar ejemplos por el interprete A0i
(define extension2 (map (lambda(x) (A0i esencia x)) ejemplos-sin-clase2))
;; Definir precision y error
(define precisionEjemplos2 (precision (cdr ejemplos2) (cdr extension2)))
(define errorEjemplo2 (error (cdr ejemplos2) (cdr extension2)))

;  Ejercicio 18
; A1
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
  (append ejemplo-sin-clase (list (obtener-al-azar0 paresClaseFrecuencia))))

(define esenciaA1 (A1 ejemplos))
(define extensionA1i (map (lambda(x) (A1i esenciaA1 x)) ejemplos-sin-clase))
(define precisionEjemplosA1 (precision (cdr ejemplos) (cdr extensionA1i)))


; Ejercicio 19
;; Pasar ejemplos por el interprete A1i
(define extensionA1iEjemplos2 (map (lambda(x) (A1i esenciaA1 x)) ejemplos-sin-clase2))
;; Definir precision y error
(define precisionA1Ejemplos2 (precision (cdr ejemplos2) (cdr extensionA1iEjemplos2)))
(define errorA1Ejemplo2 (error (cdr ejemplos2) (cdr extensionA1iEjemplos2)))
)

; Ejercicio 22
(define ejemplosJuntos (mezclar ejemplos ejemplos2))
; Se usan todos los ejemplos para entrenar y para evaluar
(define resustitution
    (lambda (esencia interprete ejemplos)
      (define ejemplos-sin-clase (map (lambda(x) (drop-right x 1)) ejemplos))
      (define cogerEsencia (esencia ejemplos))
      (define cogerExtension (map (lambda(x) (interprete cogerEsencia x)) ejemplos-sin-clase))
      (precision (cdr ejemplos) (cdr cogerExtension))
      ))
; Se usan cada vez solo un ejemplo para evaluar (con todos los ejemplos)
(define leave-one-out
  (lambda (entrenamiento interprete ejemplos)
    (define casos (list-tail ejemplos 1))
    (define evaluarCaso
      (lambda (casoParaEvaluar)
        (define casosDeEntrenamiento (remq casoParaEvaluar ejemplos))
        (define casoSinClase (map (lambda(x) (drop-right x 1)) (list casoParaEvaluar)))
        (define entrenar (entrenamiento casosDeEntrenamiento))
        (define interpretar (map (lambda(x) (interprete entrenar x)) casoSinClase))
        (eq? (last casoParaEvaluar) (last (first interpretar)))
        ))
    (define numeroAciertos (length (filter (lambda (caso)
         (evaluarCaso caso))
       casos)))
    (exact->inexact (/ numeroAciertos (length casos)))))

; Ejercicio 23
(define ejemplosSeparados (separar 0.67 ejemplosJuntos))
(define atributos (car ejemplosJuntos))
(define ejemplosEntrenamiento (append (list atributos) (list-ref ejemplosSeparados 0)))
(define ejemplosEvaluacion (append (list atributos) (list-ref ejemplosSeparados 1)))
(define holdout
    (lambda (entrenamiento interprete ejemplosEntrenamiento ejemplosEvaluacion)
      (define ejemplosEvaluacionSinClase (list-tail (map (lambda(x) (drop-right x 1)) ejemplosEvaluacion) 1))
      (define entrenar (entrenamiento ejemplosEntrenamiento))
      (define interpretar (map (lambda(x) (interprete entrenar x)) ejemplosEvaluacionSinClase))
      (precision ejemplosEvaluacion interpretar)))

; Ejercicio 24
(define cross-validation
  (lambda (entrenamiento interprete ejemplos numeroFolds)
    (define atributos (car ejemplos))
    (define ejemplosEnFolds (folds numeroFolds ejemplos))
    (define evaluarFold
      (lambda (foldParaEvaluar)
        (define foldsDeEntrenamiento (remq foldParaEvaluar ejemplosEnFolds))
        (define ejemplosEntrenamiento (append (list atributos) (append* foldsDeEntrenamiento)))
        ;(define ejemplosEvaluacion (append (list atributos) foldParaEvaluar))
        (holdout entrenamiento interprete ejemplosEntrenamiento foldParaEvaluar)))
    (define sumaPrecisiones (apply + (map (lambda (fold)
         (evaluarFold fold)) ejemplosEnFolds)))
    (exact->inexact (/ sumaPrecisiones numeroFolds))))

(define stratified-cross-validation
  (lambda (entrenamiento interprete ejemplos numeroFolds)
    (define atributos (car ejemplos))
    (define ejemplosEnFolds (stratify numeroFolds ejemplos))
    (define evaluarFold
      (lambda (foldParaEvaluar)
        (define foldsDeEntrenamiento (remq foldParaEvaluar ejemplosEnFolds))
        (define ejemplosEntrenamiento (append (list atributos) (append* foldsDeEntrenamiento)))
        ;(define ejemplosEvaluacion (append (list atributos) foldParaEvaluar))
        (holdout entrenamiento interprete ejemplosEntrenamiento foldParaEvaluar)))
    (define sumaPrecisiones (apply + (map (lambda (fold)
         (evaluarFold fold)) ejemplosEnFolds)))
    (exact->inexact (/ sumaPrecisiones numeroFolds))))


;; BLOQUE 2
;; ===================================================================

; Ejercicio 1
;(match-CL '((*) (5 40) (subiendo) (estable) (10 60) (si)) '(bueno 20 subiendo estable 50 si ))
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


; Ejercicio 2
;(CLi '((*) (5 40) (subiendo) (estable) (10 60) (si)) '(bueno 20 subiendo estable 50 si ))
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

; Ejercicio 3
(define CLgeneral '((*) (*) (*) (*) (*) (*)))
(define CLespecifico '(() (1 0) () () (1 0) ()))
(define CLcercano '((*) (18 +inf.0) (subiendo) (*) (5 60) (no)))

; Ejercicio 4
(define (concepto-CL-mas-general metadatos)
    (drop-right (map
       (lambda (dato)
         (cond
            [(number? dato) (list '-inf.0 '+inf.0)]
            [else (list '*)]))
       metadatos)1))

; Ejercicio 5
 (define (concepto-CL-mas-especifico metadatos)
    (map
       (lambda (dato)
         (cond
            [(number? dato) (list 1 0)]
            [else '()]))
       metadatos))


; Ejercicio 6
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
            [(and (pair? t1) (pair? t2) (symbol? (car t1)) (symbol? (car t2))) #t]
            ; Si pasa el test nominal significa que es igual o mas general
            [(testNominal t1 t2) #t] 
            [(eq? t2 empty) #t]
            [else #f])))
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
             ; Segun amplitud de rango
             ;[(> (- (valor (list-ref t1 1)) (valor (car t1)))
             ;    (- (valor (list-ref t2 1)) (valor (car t2)))) #t]
             ;[else #f]
             [else #t]
          )))
      (if esTestNominal (pasaTestNominal t1 t2) (pasaTestNumerico t1 t2))
    ))

; Ejercicio 7
(define (concepto-CL>= c1 c2)
    (andmap (lambda (concepto1 concepto2)
                                (test-CL>= concepto1 concepto2)) c1 c2))

; Ejercicio 8
(define (cmp-concepto-CL c1 c2)
    (cond
      [(and (concepto-CL>= c1 c2) (not (concepto-CL>= c2 c1))) 1]
      [(and (concepto-CL>= c1 c2) (concepto-CL>= c2 c1)) 0]
      [(and (concepto-CL>= c2 c1) (not (concepto-CL>= c1 c2))) -1] 
      )
  )


; Ejercicio 9
; (especializaciones-atributo-nominal '((bueno) (5 40) (subiendo) (estable) (10 60) (si)) 2 (car ejemplos))
(define tiposDeAtributo
  (lambda (nombre atributos)
    (define atributoEncontrado (assoc nombre atributos))
    (list-ref atributoEncontrado 1)))

(define (especializaciones-atributo-nominal concepto-CL indice metadatos)
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

; Ejercicio 10
; (generalizaciones-atributo-nominal '(() (5 40) (subiendo) (estable) (10 60) (si)) 0 (car ejemplos))
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


; Ejercicio 11
;(generalizacion-atributo-numerico '((soleado)(30)(20)(si)) 1 '(soleado 25 40 si -))
;(generalizacion-atributo-numerico '((soleado)(15 20)(20)(si)) 1 '(soleado 25 40 si +))
;(generalizacion-atributo-numerico '((soleado)(*)(20)(si)) 1 '(soleado 25 40 si +))

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

; Ejercicio 12
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

; Ejercicio 13
; (generalizaciones-CL '(() (5 20) (subiendo) (estable) (10 60) (si)) (car ejemplos) '(bueno 26 estable subiendo 43 si +))
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

; Ejercicio 14
; (especializaciones-CL '((bueno) (5 40) (subiendo) (estable) (10 60) (si)) (car ejemplos) '(bueno 26 estable subiendo 43 si -))
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

; Ejercicio 15
; Devuelve una lista con tantos elementos como clases hay. Cada elemento contiene todos los casos de esa clase
(define separarClases
  (lambda (ejemplos)
    (define tiposDeClases (atributo 'clase ejemplos))
    (define casos (list-tail ejemplos 1))
    (define casosAgrupadosPorClase
      (lambda (index tiposDeClases casos agrupacion)
        (if (= index (length tiposDeClases))
            agrupacion
            (let ((claseAgrupada  (filter
                               (lambda (caso)
                                 (eqv? (list-ref caso (- (length caso) 1)) (list-ref tiposDeClases index)))
                               casos)))
              (casosAgrupadosPorClase (+ index 1) tiposDeClases casos (append agrupacion (list claseAgrupada)))))))
    (casosAgrupadosPorClase 0 tiposDeClases casos '())))

(define PSET (car (separarClases ejemplos)))
(define NSET (list-ref (separarClases ejemplos) 1))

(define (EGS01 PSET NSET CSET HSET)
  (let* ()
    (define HParaEliminar
      (filter (lambda (H)(not (andmap (lambda (P)
                                (match-CL H (drop-right P 1))) PSET))) HSET))
     (define HParaEliminarYAnadir
      (filter (lambda (H)(not (ormap (lambda (N)
                                (match-CL H (drop-right N 1))) NSET))) HSET))
    (define HSETfinal
      (remq* (append HParaEliminar HParaEliminarYAnadir) HSET))
    (define CSETfinal (append HParaEliminarYAnadir CSET))
    (define numeroNegativasAceptadas
      (lambda (concepto)
        (length (filter
                 (lambda (N) (match-CL concepto (drop-right N 1))) NSET))))
    (if (equal? HSETfinal empty)
        CSETfinal
        (let* ()
          (define SPECS
            (append-map
             (lambda (H)
               (define negativasDeH (numeroNegativasAceptadas H))
               (define especialiazaciones
                 (especializaciones-CL H (car ejemplos) '(bueno 26 estable subiendo 43 si +)))
               (filter
                (lambda (especialiazacion)
                  (< (numeroNegativasAceptadas especialiazacion) (numeroNegativasAceptadas H)))
                especialiazaciones)
               ) HSETfinal))
          (define NEWSET
            (filter
              (lambda (S)
                (andmap
                     (lambda (C) (eq? (cmp-concepto-CL S C) -1)) CSETfinal))
            SPECS))
        (EGS01 PSET NSET CSETfinal NEWSET)))))

(define (EGS ejemplos)
  (let* ()
  (define (EGS0 PSET NSET CSET HSET)
  (let* ()
    (define HParaEliminar
      (filter (lambda (H)(not (andmap (lambda (P)
                                (match-CL H (drop-right P 1))) PSET))) HSET))
     (define HParaEliminarYAnadir
      (filter (lambda (H)(not (ormap (lambda (N)
                                (match-CL H (drop-right N 1))) NSET))) HSET))
    (define HSETfinal
      (remq* (append HParaEliminar HParaEliminarYAnadir) HSET))
    (define CSETfinal (append HParaEliminarYAnadir CSET))
    (define numeroNegativasAceptadas
      (lambda (concepto)
        (length (filter
                 (lambda (N) (match-CL concepto (drop-right N 1))) NSET))))
    (if (equal? HSETfinal empty)
        CSETfinal
        (let* ()
          (define SPECS
            (append-map
             (lambda (H)
               (define negativasDeH (numeroNegativasAceptadas H))
               (define especialiazaciones
                 (especializaciones-CL H (car ejemplos) positivoAlAzar))
               (filter
                (lambda (especialiazacion)
                  (< (numeroNegativasAceptadas especialiazacion) (numeroNegativasAceptadas H)))
                especialiazaciones)
               ) HSETfinal))
          (define NEWSET
            (filter
              (lambda (S)
                (andmap
                     (lambda (C) (eq? (cmp-concepto-CL S C) -1)) CSETfinal))
            SPECS))
        (EGS0 PSET NSET CSETfinal NEWSET)))))
  (define PSET (car (separarClases ejemplos)))
  (define NSET (list-ref (separarClases ejemplos) 1))
  (define positivoAlAzar (obtener-al-azar PSET))
  (define listaCSET (EGS0 PSET NSET '() (list (concepto-CL-mas-general (car ejemplos)))))
  (if (equal? listaCSET empty) '() (obtener-al-azar listaCSET))))


; Ejercicio 16
(define direccionAgaricus "/Users/patriciamayotejedor/Documents/Private Development/UNED/Machine Learning/MachineLearning/Assignment2/agaricus-lepiota.scm")
(define agaricus-lepiota (leer-ejemplos direccionAgaricus))
(define direccionIonosphere  "/Users/patriciamayotejedor/Documents/Private Development/UNED/Machine Learning/MachineLearning/Assignment2/ionosphere.scm")
(define ionosphere (leer-ejemplos direccionIonosphere))

; Ejercicio 17
; (score-CL '((*) (*) (*) (*) (*) (*)) PSET NSET)
(define instanciasAceptadas
  (lambda (concepto SET)
        (length (filter
                 (lambda (S) (match-CL concepto (drop-right S 1))) SET))))

(define (score-CL concepto-CL PSET NSET)
 (let* ((totalInstancias (+ (length PSET) (length NSET)))
        (positivasAceptadas (instanciasAceptadas concepto-CL PSET))
        (negativasNoAceptadas (- (length NSET) (instanciasAceptadas concepto-CL NSET))))
   (/ (+ positivasAceptadas negativasNoAceptadas) totalInstancias)))


; Ejercicio 18
; > (HGS0 PSET NSET '() '(((*) (*) (*) (*) (*) (*))))
; '(((*) (*) (subiendo) (*) (*) (*)))
(define (HGS0 PSET NSET CSET HSET)
    (let* ((Beam-Size +)
           (positivoAlAzar (obtener-al-azar PSET)))
      (define getOPENyCLOSE
        (lambda (indice PSET NSET CSET HSET OPEN-SET)
          (if (eq? indice (length HSET))
              (append (list OPEN-SET) (list CSET))
              ; else 
              (let* (;Variables locales
                     (H (list-ref HSET indice))
                     (SPECS (especializaciones-CL H (car ejemplos) positivoAlAzar))
                     (NEW-SET
                      (filter
                       (lambda (S) (> (score-CL S PSET NSET) (score-CL H PSET NSET)))
                       SPECS)))
                (if (eq? NEW-SET empty)
                    (getOPENyCLOSE (+ indice 1) PSET NSET (append CSET (list H)) HSET OPEN-SET)
                    (let* (
                           (eliminarS
                            (filter
                             (lambda (S)
                               (ormap
                                 (lambda (C)
                                   (and
                                    (test-CL>= S C)
                                    (> (score-CL C PSET NSET) (score-CL S PSET NSET))))
                                          CSET)
                               ) NEW-SET))
                           (eliminarC
                            (filter
                             (lambda (S)
                               (ormap
                                 (lambda (C)
                                   (and
                                    (test-CL>= S C)
                                    (<= (score-CL C PSET NSET) (score-CL S PSET NSET))))
                                          CSET)
                               ) NEW-SET)))
                      (getOPENyCLOSE
                       (+ indice 1)
                       PSET
                       NSET
                       (remq* eliminarC CSET)
                       HSET
                       (remq* eliminarS (append NEW-SET OPEN-SET)))
                    )
                )
              )
          )))
      
     (define openyclose (getOPENyCLOSE 0 PSET NSET CSET HSET '()))
     (define OPEN-SET (list-ref openyclose 0))
     (define CLOSE-SET (list-ref openyclose 1))
     ; Encuentra los conceptos con mayor score
     (define scoresMasAltos
       (lambda (numero SET)
         (define scores (map (lambda (S) (score-CL S PSET NSET)) SET))
         (define sortedList (sort scores >))
         (define scoresMasAltos (take sortedList (min numero (length sortedList))))
         (map
          (lambda(score)
            (list-ref SET (index-of scores score))
            ) scoresMasAltos)))
     (if (eq? OPEN-SET empty)
         (scoresMasAltos 1 CLOSE-SET)
         (let* (
                (BEST-SET (scoresMasAltos Beam-Size (append OPEN-SET CLOSE-SET)))
                (CSETFinal (lset-intersection eq? CLOSE-SET BEST-SET))
                (OPEN-SETFinal (lset-intersection eq? OPEN-SET BEST-SET)))
           (HGS0 PSET NSET CSETFinal OPEN-SETFinal)
         ))
      )
  )



(define (HGS ejemplos)
  (define (HGS0 PSET NSET CSET HSET)
    (let* ((Beam-Size 4)
           (positivoAlAzar (obtener-al-azar PSET)))
      (define getOPENyCLOSE
        (lambda (indice PSET NSET CSET HSET OPEN-SET)
          (if (eq? indice (length HSET))
              (append (list OPEN-SET) (list CSET))
              ; else 
              (let* (;Variables locales
                     (H (list-ref HSET indice))
                     (SPECS (especializaciones-CL H (car ejemplos) positivoAlAzar))
                     (NEW-SET
                      (filter
                       (lambda (S) (> (score-CL S PSET NSET) (score-CL H PSET NSET)))
                       SPECS)))
                (if (eq? NEW-SET empty)
                    (getOPENyCLOSE (+ indice 1) PSET NSET (append CSET (list H)) HSET OPEN-SET)
                    (let* (
                           (eliminarS
                            (filter
                             (lambda (S)
                               (ormap
                                 (lambda (C)
                                   (and
                                    (test-CL>= S C)
                                    (> (score-CL C PSET NSET) (score-CL S PSET NSET))))
                                          CSET)
                               ) NEW-SET))
                           (eliminarC
                            (filter
                             (lambda (S)
                               (ormap
                                 (lambda (C)
                                   (and
                                    (test-CL>= S C)
                                    (<= (score-CL C PSET NSET) (score-CL S PSET NSET))))
                                          CSET)
                               ) NEW-SET)))
                      (getOPENyCLOSE
                       (+ indice 1)
                       PSET
                       NSET
                       (remq* eliminarC CSET)
                       HSET
                       (remq* eliminarS (append NEW-SET OPEN-SET)))
                    )
                )
              )
          )))
      
     (define openyclose (getOPENyCLOSE 0 PSET NSET CSET HSET '()))
     (define OPEN-SET (list-ref openyclose 0))
     (define CLOSE-SET (list-ref openyclose 1))
     ; Encuentra los conceptos con mayor score
     (define scoresMasAltos
       (lambda (numero SET)
         (define scores (map (lambda (S) (score-CL S PSET NSET)) SET))
         (define sortedList (sort scores >))
         (define scoresMasAltos (take sortedList (min numero (length sortedList))))
         (map
          (lambda(score)
            (list-ref SET (index-of scores score))
            ) scoresMasAltos)))
     (if (eq? OPEN-SET empty)
         (scoresMasAltos 1 CLOSE-SET)
         (let* (
                (BEST-SET (scoresMasAltos Beam-Size (append OPEN-SET CLOSE-SET)))
                (CSETFinal (lset-intersection eq? CLOSE-SET BEST-SET))
                (OPEN-SETFinal (lset-intersection eq? OPEN-SET BEST-SET)))
           (HGS0 PSET NSET CSETFinal OPEN-SETFinal)
         ))
      )
  )
  (define PSET (car (separarClases ejemplos)))
  (define NSET (list-ref (separarClases ejemplos) 1))
  (define conceptoGeneral (list (concepto-CL-mas-general (car ejemplos))))
  (define positivoAlAzar (obtener-al-azar PSET))
  (define listaCSET (HGS0 PSET NSET '() conceptoGeneral))
  (cond
    [(empty? listaCSET) listaCSET]
    [(equal? listaCSET conceptoGeneral) '()]
    [else (first listaCSET)]
    )
  ;(if (empty? listaCSET) listaCSET (first listaCSET))
)

; Ejercicio 19
; (HGS ionosphere)
; (HGS agaricus-lepiota)

; Ejercicio 23
; (traducir '(perspectiva (soleado nublado lluvioso)) 'lluvioso)
(define (traducir meta-atributo valor)
  (let* ((tipos (list-ref meta-atributo 1))
         (esNominal (list? tipos)))
    (cond
      [esNominal (index-of tipos valor)]
      [else valor])))


; Ejercicio 24
;(nuevo-conceptoUU (car ejemplos) 1)
(define (nuevo-conceptoUU metadatos init)
(let* ((vector (map (lambda (x)(* init (- (* 2 (random)) 1))) metadatos)))
  (list metadatos vector)))


; Ejercicio 25
(define AtributosProfesor '((perspectiva (soleado nublado lluvioso)) (temperatura numerico) (humedad numerico) (viento (si no)) (clase (+ -))))
;(match-LUU (list AtributosProfesor '(0 1 1 0 -30)) '(lluvioso 10 20 si))
(define (productoEscalar vector1 vector2)
  (apply + (map * vector1 vector2)))

(define (traducirEjemplo metadatos ejemplo-sin-clase)
  (map
   (lambda (meta-atributo valor)
     (traducir meta-atributo valor))
   metadatos ejemplo-sin-clase))

(define (match-LUU conceptoUU ejemplo-sin-clase)
  (let* ((metadatos (list-ref conceptoUU 0))
         (vectorPesos (list-ref conceptoUU 1))
         (ejemploTraducido
          (map
           (lambda (meta-atributo valor)
             (traducir meta-atributo valor))
           metadatos ejemplo-sin-clase))
         (producto (productoEscalar vectorPesos (append ejemploTraducido '(1)))))
    (if (eq? producto 0) #t (positive? producto))))

; Ejercicio 26
;(LUUi (list AtributosProfesor '(0 1 1 0 -31)) '(lluvioso 10 20 si))
(define (LUUi conceptoUU ejemplo-sin-clase)
(let* ((pasaUmbral (match-LUU conceptoUU ejemplo-sin-clase)))
  (if pasaUmbral (append ejemplo-sin-clase (list '+)) (append ejemplo-sin-clase (list '-)))))


; Ejercicio 30
(define concepto-UU (nuevo-conceptoUU (car ejemplos) 1))
(define (PRM concepto-UU ejemplos)
  (let* ((n 0.2)
         (ISET (list-tail ejemplos 1)))
  (define recorrerISET
    (lambda (indice ISET H)
      (if (eq? indice (length ISET))
          H
          (let* ((I (list-ref ISET indice))
                 (C (last I))
                 (P (last (LUUi H (drop-right I 1))))
                 (S (cond
                      [(and (eq? P '-) (eq? C '+)) 1]
                      [(and (eq? P '+) (eq? C '-)) -1]
                      [else empty])))
            (if (eq? P C)
                (recorrerISET (+ indice 1) ISET H)
                (let* ((ATTS (first H))
                       (vector (last H))
                       (umbral (last vector)))
                  (define vectorAjustado
                    (map
                     (lambda (W i A)
                            (define V (traducir A i))
                            (+ (* S n V) W))
                     (drop-right vector 1) (drop-right I 1) (drop-right ATTS 1)))
                  (recorrerISET (+ indice 1) ISET (list ATTS (append vectorAjustado (list (+ (* S n) umbral)))))))))))
     (recorrerISET 0 ISET concepto-UU)))

; Ejercicio 31
(define COUNT 1)
(define (PCP ejemplos)
(let* ((H (nuevo-conceptoUU (car ejemplos) 1))
       (contador COUNT)
       (ISET (list-tail ejemplos 1)))
  (define recorrerCOUNT
    (lambda (H ISET COUNT)
      (if (eq? COUNT 0) H
          (let* ((NO-ERRORS (andmap
                             (lambda (I)
                                   (match-LUU H (drop-right I 1)))
                             ISET)))
            (if NO-ERRORS H
                (recorrerCOUNT (PRM H ejemplos) ISET (- COUNT 1)))))))
  (recorrerCOUNT H ISET contador)))


; Ejercicio 32
(define (LMS ejemplos)
(let* ((n 0.2) ; gain
       (a 0.9) ; momentum
       (Minimum-Error 15)
       (conceptoUU (nuevo-conceptoUU (car ejemplos) 0.5))
       (ATTS (first conceptoUU))
       (H (last conceptoUU))
       (Aa (map (lambda (A) 0) ATTS))
       (contador COUNT)
       (ISET (list-tail ejemplos 1)))

  (define recorrerATTS
    (lambda (indice ATTS Aa H)
      (if (eq? indice (length ATTS))
          (list (list Aa) (list H))
          (let* ((A (list-ref ATTS indice))
                 (GRADIENT
                        (apply + (map
                                  (lambda (I)
                                    (define ITraducido (traducirEjemplo ATTS (drop-right I 1)))
                                    (define O (apply + ITraducido))
                                    (define P (productoEscalar H (append ITraducido '(1))))
                                    ;(define d (* P (/ (- 1 P) 100) (/ (- O P) 100)))
                                    (define d (* P (- 1 P) (- O P)))
                                    (define V (traducir A (list-ref I indice)))
                                    (* n d V))
                                  ISET)))
                 (viejoAa (list-ref Aa indice))
                 (nuevoAa (+ GRADIENT (* a viejoAa)))
                 (W (list-ref H indice)))
            (recorrerATTS
             (+ indice 1)
             ATTS
             (list-set Aa indice nuevoAa)
             (list-set H indice (+ nuevoAa W)))))))
  
  (define recorrerCOUNT
    (lambda (H ISET COUNT Aa)
      (if (eq? COUNT 0) H
          (let* (
                 (TOTAL-ERROR
                  (apply + (map
                            (lambda (I)
                              (define ITraducido (traducirEjemplo ATTS (drop-right I 1)))
                              (define O (apply + ITraducido))
                              (define P (productoEscalar H (append ITraducido '(1))))
                              (sqr (/ (- O P) 100)))
                            ISET))))
            (if (< TOTAL-ERROR Minimum-Error)
                H
                (let* ((actualizarAayH (recorrerATTS 0 ATTS Aa H))
                       (nuevoH (list-ref actualizarAayH 1))
                       (nuevoAa (list-ref actualizarAayH 0)))
                  (recorrerCOUNT nuevoH ISET (- COUNT 1) nuevoAa)))))))
  (append (list ATTS) (list (recorrerCOUNT H ISET COUNT Aa)))))


; Ejercicio 34
; (distancia '(soleado 10 20 si) '(soleado 15 21 no -))   ->    5.196152422706632
(define (IB ejemplos) (list-tail ejemplos 1))
(define (distancia ejemplo-sin-clase ejemplo)
  (let* ((ejemplo-y (drop-right ejemplo 1)))
    (define valor
      (lambda (x y)
        (cond
          [(number? x) (expt (- y x) 2)]
          [(eq? x y) 0]
          [else 1])))
    (inexact->exact(sqrt (apply + (map
                    (lambda (x y)
                      (valor x y))
                    ejemplo-sin-clase ejemplo-y))))))

; Ejercicio 35
(define (IBi concepto-IB ejemplo-sin-clase)
(let* ((distancias (map
                    (lambda (ejemplo)
                      (distancia ejemplo-sin-clase ejemplo)) concepto-IB))
       (min-distancia (apply min distancias))
       (index-de-min-distancia (index-of distancias min-distancia))
       (concepto-mas-cercano (list-ref concepto-IB index-de-min-distancia)))
  (append ejemplo-sin-clase (list (last concepto-mas-cercano)))))


; Ejercicio 36
(define (match-IB concepto-IB ejemplo-sin-clase)
  (let* ((concepto-mas-cercano (IBi concepto-IB ejemplo-sin-clase)))
   (eq? '+ (last concepto-mas-cercano))))



; Ejercico 37

(define (nuevo-conceptoNB metadatos)
  (do ((cuentas '(0))
       (valores '())
       (i 0 (+ i 1))
       )
    ((= i (- (length metadatos) 1))
     (let ((c (reverse cuentas)))
       (list (cons '+ c)(cons '- c)));valor devuelto
     )
    (set! valores (cadr (list-ref metadatos i)))
    (cond
      ((eq? valores 'numerico)
       (set! cuentas (cons '(numerico 0 0) cuentas)))
      (else ;nominales
       (set! cuentas
             (cons (map (lambda(x) (cons x 0))
                        valores)
                   cuentas))))))

(define (INB concepto-NB ejemplo)
(let* ((listaPositiva (first concepto-NB))
       (listaNegativa (last concepto-NB)))

  (define valorNumerico
    (lambda (valorNB valorEjemplo)
      (define actualizarSuma (list-set valorNB 1 (+ (list-ref valorNB 1) valorEjemplo))) ; sumar valores
      (list-set actualizarSuma 2 (+ (list-ref valorNB 2) (expt valorEjemplo 2))) ; suma de cuadrados -> distribucion normal
      ))

  (define valorNominal
    (lambda (valorNB valorEjemplo)
      (define index-valorEjemplo (index-where valorNB (lambda (atributoNB) (eq? (car atributoNB) valorEjemplo))))
      (define atributoNB (list-ref valorNB index-valorEjemplo)) ; buscar el par a cambiar
      (list-set valorNB index-valorEjemplo (cons (car atributoNB) (+ (cdr atributoNB) 1))) ; sumar 1 al par
      ))
  
   (define (actualizarNB listaNB ejemplo-sin-clase)
     (let* ((clase (first listaNB))
            (contadorClase (+ (list-ref listaNB 1) 1)))
       (define atributos
         (map
          (lambda (atributoNB atributoEjemplo)
            (if (pair? (first atributoNB))
                (valorNominal atributoNB atributoEjemplo)
                (valorNumerico atributoNB atributoEjemplo)))
          (list-tail listaNB 2) ejemplo-sin-clase))
       (append (list clase) (list contadorClase) atributos)
       ))
  (if (eq? (last ejemplo) '+)
      (list  (actualizarNB listaPositiva (drop-right ejemplo 1)) listaNegativa)
      (list  listaPositiva (actualizarNB listaNegativa (drop-right ejemplo 1))))
  ))



; Ejercicio 38
(define (NB ejemplos)
 (let* ((casos (list-tail ejemplos 1))
        (nuevoNB (nuevo-conceptoNB (car ejemplos))))
   (define recorrerEjemplos
     (lambda (indice concepto-NB casos)
       (if (eq? indice (length casos))
           concepto-NB
           (recorrerEjemplos
            (+ indice 1)
            (INB concepto-NB (list-ref casos indice))
            casos))))
   (recorrerEjemplos 0 nuevoNB casos)))


; Ejercicio 39
(define (media x n) (exact->inexact(/ x n)))

; Sumatorio (a - b)^2 =  Sumatorio (a^2 - 2ab + b^2)
; Sumatorio a^2 = x2
; Sumatorio b^2 = n * m^2
; Sumatorio a = n * m
; b = m
(define (varianza x2 m n)
(let* ((b^2 (* n (expt m 2)))
       (a (* n m)))
  (/ (+ (- x2 (* 2 a m)) b^2)  (- n 1))))

; Ejercicio 40
; (probabilidades '+ (NB ejemplos) '(bueno 27 subiendo estable 54 si))
(define (probabilidades clase concepto-NB ejemplo-sin-clase)
(let* ((listaClaseNB
        (if (eq? clase '+)
            (first concepto-NB)
            (last concepto-NB)))
       (contadorClase (list-ref listaClaseNB 1)))

  (define probabilidadNominal
    (lambda (valorNB valorEjemplo contadorClase)
      (define index-valorEjemplo (index-where valorNB (lambda (atributoNB) (eq? (car atributoNB) valorEjemplo))))
      (define atributoNB (list-ref valorNB index-valorEjemplo))
      (/ (cdr atributoNB) contadorClase)))

  (define (probabilidadNumerica valorNB valorEjemplo contadorClase)
    (let* ((mediaAtributo (media (list-ref valorNB 1) contadorClase))
           (varianzaAtributo (varianza (last valorNB) mediaAtributo contadorClase))
           (raiz (sqrt (* 2 pi varianzaAtributo)))
           (exponenteEuler (/ (expt (- valorEjemplo mediaAtributo) 2) (* 2 varianzaAtributo))))
      (* (/ 1 raiz) (exp (- exponenteEuler)))))
  
  (map
   (lambda (atributoNB atributoEjemplo)
     (if (pair? (first atributoNB))
         (probabilidadNominal atributoNB atributoEjemplo contadorClase)
         (probabilidadNumerica atributoNB atributoEjemplo contadorClase)))
   (list-tail listaClaseNB 2) ejemplo-sin-clase)))


; Ejercicio 41
(define (NBi concepto-NB ejemplo-sin-clase)
(let* ((probabilidadPositiva (apply * (probabilidades '+ concepto-NB ejemplo-sin-clase)))
       (probabilidadNegativa (apply * (probabilidades '- concepto-NB ejemplo-sin-clase))))
  (cond
    [(> probabilidadPositiva probabilidadNegativa) (append ejemplo-sin-clase (list '+))]
    [(< probabilidadPositiva probabilidadNegativa) (append ejemplo-sin-clase (list '-))]
    [else (append ejemplo-sin-clase (obtener-al-azar (list '+ '-)))])
))

(define (match-NB concepto-NB ejemplo-sin-clase)
  (let* ((clasificacion (NBi concepto-NB ejemplo-sin-clase)))
    (eq? '+ (last clasificacion))))


; Ejercicio 42
; ====================== LMS =======================
;> (stratified-cross-validation LMS CLi ejemplos 10)
;0.2
;> (stratified-cross-validation LMS CLi ionosphere 10)
;0.4696825396825397
;> (stratified-cross-validation LMS CLi agaricus-lepiota 10)
;0.5003674888965638
; ====================== HGS =======================
;> (stratified-cross-validation HGS CLi ejemplos 10)
;0.4
;> (stratified-cross-validation HGS CLi ionosphere 10)
;0.45285714285714285
; ====================== PCP =======================
;> (stratified-cross-validation PCP LUUi ejemplos 10)
;0.35
;> (stratified-cross-validation PCP LUUi ionosphere 10)
;0.5643650793650794
;> (stratified-cross-validation PCP LUUi agaricus-lepiota 10)
;0.5140298656681148
; ====================== IB =======================
;> (stratified-cross-validation IB IBi ejemplos 10)
;0.3
;> (stratified-cross-validation IB IBi ionosphere 10)
;0.5752380952380952
;> (stratified-cross-validation IB IBi agaricus-lepiota 10)
;0.5204389568526228
; ====================== NB =======================
;> (stratified-cross-validation NB NBi ejemplos 10)
;0.3
;> (stratified-cross-validation NB NBi ionosphere 10)
;0.0
;> (stratified-cross-validation NB NBi agaricus-lepiota 10)
;0.5171139851792608







;; BLOQUE 3
;; ===================================================================

; Ejercicio 1
;(define concepto-LD '((match-CL ((soleado)(30 40)(50 60)(*)) => +)(match-CL ((*)(*)(*)(*)) => -)))
;(define ejemplo-sin-clase '(soleado 30 40 si))
;(LDi `((match-LUU ,(list AtributosProfesor '(0 1 1 0 -30)) => +) (match-LUU ,(list AtributosProfesor '(0 1 -1 0 -20)) => -)) '(soleado 30 40 si))
;(LDi `((match-LUU ,(list AtributosProfesor '(0 1 1 0 -30)) => +)(match-LUU ,(list AtributosProfesor '(0 1 -1 0 -20)) => -)) '(soleado 10 10 si))
(define (LDi concepto-LD ejemplo-sin-clase)
(let* ((conceptoQuePasa (find
                         (lambda (concepto)
                           (define matchProcedure (first concepto))
                           (define conceptoCL (list-ref concepto 1))
                           ((eval matchProcedure) conceptoCL ejemplo-sin-clase))
                         concepto-LD)))
  (if (boolean? conceptoQuePasa)
      ejemplo-sin-clase
      (append ejemplo-sin-clase (list (last conceptoQuePasa))))))


; Ejercicio 2
; (funcion-match HGS)
;> (eval `(,(funcion-match HGS) '((soleado)(*)) '(soleado si)))
;#t

(define *funciones-match* '((HGS . match-CL)(NB . match-NB)(IB . match-IB)(LMS . match-LUU)(PCP . match-LUU)))

(define (funcion-match algoritmo)
(let* ((funcionEncontrada (find
                         (lambda (funcion)
                           (equal? (eval (car funcion)) algoritmo))
                         *funciones-match*)))
  (cdr funcionEncontrada)))


; Ejercicio 3

(define (NSC0 algoritmo PSET NSET DNF)
(let* ((metadatos (car ejemplos))
       (nuevosEjemplos (append (list metadatos) PSET NSET)))
  (if (empty? PSET)
      DNF
      (let* ((D (algoritmo nuevosEjemplos))
             (nuevoDNF (append DNF (list D)))
             (instanciasCubiertas (filter
                                   (lambda (P)
                                     ((eval (funcion-match algoritmo)) D (drop-right P 1)))
                                   PSET)))
        (if (empty? instanciasCubiertas)
            DNF
            (NSC0 algoritmo (remq* instanciasCubiertas PSET) NSET nuevoDNF)))
  )))

(define (NSC algoritmo ejemplos)
 (let*((clasesSeparadas (separarClases ejemplos))
       (PSET (car clasesSeparadas))
       (NSET (list-ref clasesSeparadas 1)))
   
   (define (NSC0 algoritmo PSET NSET DNF)
     (let* ((metadatos (car ejemplos))
            (nuevosEjemplos (append (list metadatos) PSET NSET)))
       (if (empty? PSET)
           DNF
           (let* ((D (algoritmo nuevosEjemplos))
                  (nuevoDNF (append DNF (list D)))
                  (instanciasCubiertas (filter
                                        (lambda (P)
                                          ((eval (funcion-match algoritmo)) D (drop-right P 1)))
                                        PSET)))
             (if (empty? instanciasCubiertas)
                 DNF
                 (NSC0 algoritmo (remq* instanciasCubiertas PSET) NSET nuevoDNF)))
           )))
   
   (NSC0 algoritmo PSET NSET '())))

; Ejercicio 5

(define casosDeClase
  (lambda (ejemplos clase)
    (filter
     (lambda (ejemplo)
       (equal? clase (last ejemplo)))
     (list-tail ejemplos 1))))

(define (MSC0 algoritmo ejemplos)
(let* ((casos (list-tail ejemplos 1))
       (CSET (atributo 'clase ejemplos))
       (RULES-PER-CLASS (map
               (lambda (CLASS)
                 (define PSET (casosDeClase ejemplos CLASS))
                 (define NSET (remq* PSET casos))
                 (define DNF (NSC0 algoritmo PSET NSET '()))
                 (map
                  (lambda (D)
                    (list D '=> CLASS))
                  DNF))
               CSET)))
  (append* RULES-PER-CLASS)
  ))

(define (MSC algoritmo ejemplos)
(append
 (MSC0 algoritmo ejemplos)
 (list `(match-CL ,(make-list (- (length (car ejemplos)) 1) '(*)) => ,(A0 ejemplos)))))