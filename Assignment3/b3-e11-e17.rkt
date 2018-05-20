;;Plantilla b3-e11-e17.scm
;;Autor: Patricia Mayo Tejedor

;;Ejercicio 11
(he-tardado 40 'b3-e11)
;; Gracias a los ejemplos se ha entendido muy bien :)
; > (dividir-ejemplos '(perspectiva 0 (soleado lluvioso)) '((soleado 10 -)(soleado 25 +)(lluvioso 30 -)))
;'((soleado (soleado 10 -) (soleado 25 +)) (lluvioso (lluvioso 30 -)))
;(dividir-ejemplos '(temperatura 1 numerico 25) '((soleado 10 -)(soleado 25 +)(lluvioso 30 -)))
;'(((>= 25) (soleado 25 +) (lluvioso 30 -)) ((< 25) (soleado 10 -)))
(define (dividir-ejemplos discriminante ejemplos-sin-metadatos)
(let* ((posicion (list-ref discriminante 1))
       (esNumerico (eq? (list-ref discriminante 2) 'numerico)))
  (define DividirNominal
    (lambda ()
      (define valoresPosibles (last discriminante))
      (map
       (lambda (valor)
         (append (list valor) (filter
                      (lambda (ejemplo)
                        (eq? (list-ref ejemplo posicion) valor)) ejemplos-sin-metadatos)))
       valoresPosibles)))

  (define DividirNumerico
    (lambda ()
      (define umbral (last discriminante))
      (define mayor (filter
                      (lambda (ejemplo)
                        (>= (list-ref ejemplo posicion) umbral)) ejemplos-sin-metadatos))
      (define menor (remq* mayor ejemplos-sin-metadatos))
      (list
       (append (list (list '>= umbral)) mayor ) ; la marte mayor que
       (append (list (list '< umbral)) menor )) ; la parte menor que
      ))
  (if esNumerico
      (DividirNumerico)
      (DividirNominal))
  ))

;;Ejercicio 12
(he-tardado 40 'b3-e12)
;; Quizas no muy eficiente en datasets grandes, ya que remove-duplicates es muy costoso
; > (generar-discriminantes '((perspectiva (soleado lluvioso))(temperatura numerico)) '((soleado 10 -)(soleado 25 +)(lluvioso 30 -)))
;'((perspectiva 0 (soleado lluvioso)) (temperatura 1 numerico 10) (temperatura 1 numerico 25) (temperatura 1 numerico 30))
(define (generar-discriminantes metadatos ejemplos-sin-metadatos)
(let* ()
  (define recorrerMetadatos
    (lambda(indice discriminantes)
      (if (eq? (length metadatos) indice)
          discriminantes
          (let* ((atributo (list-ref metadatos indice))
                 (esNominal (list? (list-ref atributo 1))))
            (if esNominal
                (recorrerMetadatos
                 (+ indice 1)
                 (append discriminantes
                         (list (list (first atributo) indice (list-ref atributo 1)))))
                (recorrerMetadatos
                 (+ indice 1)
                 (append discriminantes
                         (remove-duplicates (map
                                             (lambda (ejemplo)
                                               (list (first atributo) indice 'numerico (list-ref ejemplo indice)))
                                             ejemplos-sin-metadatos))))
                ))
          )))
  (recorrerMetadatos 0 '())))

;;Ejercicio 13
(he-tardado 120 'b3-e13)
; Bastantes problemas ya que la funcion A0 requiere ejemplos con metadatos, y necesita el atributo clase para devlver la mas frecuente
; Tampoco he entendido que pide la segunda parte del ejercicio
; EDIT: al final al llegar al ejercicio 17 ya he entendido para que era la segunda parte y he actualidad el ejercicio 14 para
; que use el valor capacidad-de-discriminacion como discriminante
; (capacidad-de-discriminacion1 '(perspectiva 0 (soleado lluvioso)) '((soleado 10 -)(soleado 25 +)(lluvioso 30 -)))      > 2/3
(define (capacidad-de-discriminacion1 discriminante ejemplos-disponibles)
  (let* ((valoresClase (remove-duplicates (map (lambda(ejemplo) (last ejemplo)) ejemplos-disponibles)))
         (ejemplosConMetadatos (append (list (list (list 'clase valoresClase))) ejemplos-disponibles))
         (esencia (A0 ejemplosConMetadatos))
         (ejemplosDivididos (dividir-ejemplos discriminante ejemplos-disponibles))
         (Cv (map
              (lambda(ejemplo)
                (length (filter
                         (lambda (caso)
                           (eq? (last caso) esencia)) (cdr ejemplo)))) ejemplosDivididos))
         (sumatorioCv (apply + Cv)))
    (/ sumatorioCv (length ejemplos-disponibles))))



(define capacidad-de-discriminacion capacidad-de-discriminacion1)

;;Ejercicio 14
(he-tardado 60 'b3-e14)
;; Por algun motivo no me dejaba usar las funciones remove, o remq. Resulta que eq? no es lo mismo que equal?
; (define lista-discriminantes '((perspectiva 0 (soleado lluvioso)) (temperatura 1 numerico 10) (temperatura 1 numerico 25) (temperatura 1 numerico 30)))
; (define ejemplos-disponibles '((soleado 10 -)(soleado 25 +)(lluvioso 30 -)))
;> (mayor-discriminante lista-discriminantes ejemplos-disponibles)
;'((perspectiva 0 (soleado lluvioso)) (temperatura 1 numerico 10) (temperatura 1 numerico 25) (temperatura 1 numerico 30))
(define (mayor-discriminante lista-discriminantes ejemplos-disponibles)
(let* ((valoresDeDiscriminantes (map
                                   (lambda(discriminante)
                                    ((eval capacidad-de-discriminacion) discriminante ejemplos-disponibles))
                                   lista-discriminantes))
       (valorMayor (apply max valoresDeDiscriminantes))
       (discriminanteMayor (list-ref lista-discriminantes (index-of valoresDeDiscriminantes valorMayor)))
       (restoDiscriminantes (filter (lambda(discriminante) (not (equal? discriminante discriminanteMayor))) lista-discriminantes)))

  (append (list discriminanteMayor) restoDiscriminantes)))

;;Ejercicio 15
(he-tardado 500 'b3-e15)
;; Este ejercicio ha sido muy muy complicado, y aun asi creo que no esta bien, porque depende mucho de cual discriminante numerico se coja antes
;; Para un mismo atributo numerico puede haber muchos discriminantes  cuando este toma muchos valores distintos
;; Cada uno de ellos va a suponer una rama nueva hasta que llegue a aquel que realmente discrimine
;; He intentado muchisimo "podar" las ramas pero esque no me sale
;> (define lista-discriminantes '((perspectiva 0 (soleado lluvioso nublado)) (temperatura 1 numerico 10)( temperatura 1 numerico 25) (temperatura 1 numerico 30)))
;> (define ejemplos1 '((soleado 10 -)soleado 25 +)(lluvioso 25 -)(nublado 30 +)))
;> (DDT0 lista-discriminantes ejemplos1)
;'(adc
;  (((soleado (*)) -> (adc (
;                       (((*) ((10) +inf.0)) ->
;                                              (adc (
;                                                   (((*) ((25) +inf.0)) -> (=> +))
;                                                   (((*) (-inf.0 25)) -> (=> -)))))
;                       (((*) (-inf.0 10)) -> ()))))
;   ((lluvioso (*)) -> (=> -))
;   ((nublado (*)) -> (=> +))))
(define (list-equal? lst)
 (andmap (lambda (x) 
        (equal? x (car lst)))
      lst))

(define (DDT0 lista-discriminantes ejemplos-disponibles)
 (let* ()
   (if (empty? ejemplos-disponibles)
       '()
       (if (list-equal? (map (lambda(ejemplo) (last ejemplo)) ejemplos-disponibles)) ; si todos los ejemplos tienen la misma clase
           (list '=> (last (first ejemplos-disponibles)))
       (let* ((discriminantesOrdenados (mayor-discriminante lista-discriminantes ejemplos-disponibles))
              (mayorDiscriminante (first discriminantesOrdenados))
              (restoDiscriminantes (cdr discriminantesOrdenados))
              (posicion (list-ref mayorDiscriminante 1))
              (ejemplosDivididosPorAtributo (dividir-ejemplos mayorDiscriminante ejemplos-disponibles)))
         (define conceptosCLNumerico   
               (lambda ()
               (map
                (lambda(ramaAtributo)
                  (define atributo (first ramaAtributo))
                  (define casoGenerico  (map (lambda(valor) '(*)) (drop-right (first ejemplos-disponibles) 1)))
                  (if (eq? (first atributo) '>=)
                      (list-set casoGenerico posicion (list (cdr atributo) +inf.0))
                      (list-set casoGenerico posicion (append (list -inf.0) (cdr atributo))))
                  )ejemplosDivididosPorAtributo)))
              (define conceptosCLNominal
                (lambda ()
               (map
                (lambda(ramaAtributo)
                  (define atributo (first ramaAtributo))
                  (define casoGenerico  (map (lambda(valor) '(*)) (drop-right (first ejemplos-disponibles) 1)))
                  (list-set casoGenerico posicion atributo)
                  ) ejemplosDivididosPorAtributo)))
         (define conceptosCL
           (if (list? (first (first ejemplosDivididosPorAtributo)))
               (conceptosCLNumerico)
               (conceptosCLNominal)))
         (append '(adc) (list (map (lambda (conceptoCL ramaAtributo)
                                     (append (list conceptoCL) '(->) (list ((eval 'DDT0) restoDiscriminantes (cdr ramaAtributo))))) conceptosCL ejemplosDivididosPorAtributo)))
  )))))

(define (DDT ejemplos)
(let* ((metadatos (first ejemplos))
       (casos (cdr ejemplos))
       (discriminantes (generar-discriminantes metadatos casos)))
  (DDT0 discriminantes casos)
  ))

;;Ejercicio 16
(he-tardado 180 'b3-e16)
;; Espero que este bien, he tenido algunos problemas, por ejemplo con el log de 0, pero luego leyendo el libro me he dado cuenta
;; que si todos los casos son de la misma clase la entropia es 0, por lo que lo he pasado directamente
;; Si hay la misma proporcion de casos en una clase que en la otra entonces la entropia es 1
;; Aunque me haya costado esta muy bien explicado en el pdf ML, mucho mejor y mucho mas claro que la verbosidad matematica que se usa en EML
;; Me gustaria que hubiese ejemplos en el ejercicio para poder ver si lo he hecho bien o no
;> (capacidad-de-discriminacion2 '(perspectiva 0 (lluvia variable bueno frio niebla calorSeco)) (cdr ejemplos))
;0.8332062193464952
(define (capacidad-de-discriminacion2 discriminante ejemplos-disponibles)
(let* (
       (totalEjemplos (length ejemplos-disponibles))
       (clasesPositivas (length (filter (lambda(ejemplo) (equal? (last ejemplo) '+)) ejemplos-disponibles)))
       (clasesNegativas (-  totalEjemplos clasesPositivas))
       (ejemplosDivididosPorAtributo (dividir-ejemplos discriminante ejemplos-disponibles))
       )
  (define E
    (lambda(positivos negativos)
      (if (or (eq? positivos 0) (eq? negativos 0))
          0
          (apply + (map (lambda(clase)
                          (define proporcion (/ clase (+ positivos negativos)))
                          (* (- proporcion) (log proporcion 2)) ) (list positivos negativos))))))
  (define entropiaTotal (E clasesPositivas clasesNegativas))
  (define entropiaAtributo
    (apply + (map
     (lambda(ejemploDeAtributo)
       (define casos (cdr ejemploDeAtributo))
       (define totalEjemplosDeAtributo (length casos))
       (define positivosDeAtributo (length (filter (lambda(ejemplo) (equal? (last ejemplo) '+)) casos)))
       (define negativosDeAtributo (-  totalEjemplosDeAtributo positivosDeAtributo))
       (* (/ totalEjemplosDeAtributo totalEjemplos) (E positivosDeAtributo negativosDeAtributo))
       )
     ejemplosDivididosPorAtributo)))
  (- entropiaTotal entropiaAtributo)
))

;;Ejercicio 17
(he-tardado 300 'b3-e17)
;; El arbol es mas complejo en el caso capacidad-de-discriminacion1 ya que la manera de descriminar se basa en A0 (la clase mayoritaria), por lo que ha medida
;; que avanza el algoritmo quedan menos casos y se vuelve mas ineficaz (mayor-discriminante) no devuelve el correcto
;; Por ejemplo con los ejemplos '((soleado 10 -)(soleado 25 +)), la capacidad-de-discriminacion1 '(temperatura 1 numerico 10) y '(temperatura 1 numerico 25) es 1/2
;; cuando '(temperatura 1 numerico 25) es la realmente relevante

;; En el caso de capacidad-de-discriminacion2 con la entropia se asignan valores que realmente miden la relevancia del algoritmo, proporcionando el
;; discriminante correcto a la primera

;> (define ejemplos-test '(((perspectiva (soleado lluvioso nublado))(temperatura numerico) (clase (+ -))) (soleado 10 -)(soleado 25 +)(lluvioso 30 -)(nublado 30 +)))
;; ===================================  (define capacidad-de-discriminacion capacidad-de-discriminacion1) ==========================
;> (DDT ejemplos-test)
;'(adc
;  (((soleado (*)) -> (adc (
;                       (((*) ((10) +inf.0)) ->
;                                              (adc (
;                                                   (((*) ((25) +inf.0)) -> (=> +))
;                                                   (((*) (-inf.0 25)) -> (=> -)))))
;                       (((*) (-inf.0 10)) -> ()))))
;   ((lluvioso (*)) -> (=> -))
;   ((nublado (*)) -> (=> +))))
;; ===================================  (define capacidad-de-discriminacion capacidad-de-discriminacion2) ==========================
;> (DDT ejemplos-test)
;'(adc (
;      ((soleado (*)) -> (adc (
;                             (((*) ((25) +inf.0)) -> (=> +))
;                             (((*) (-inf.0 25)) -> (=> -)))))
;      ((lluvioso (*)) -> (=> -))
;      ((nublado (*)) -> (=> +))))


;; Comparando los resultados vemos que siguen igual que cualquier otro de los algoritmos, una accuracy que ronda siempre el 50%
;> (stratified-cross-validation DDT JCi lymphography 10)
;0.42142857142857143
;> (stratified-cross-validation DDT JCi agaricus-lepiota 10)
;0.5050465344554923