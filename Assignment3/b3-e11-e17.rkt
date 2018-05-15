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
(he-tardado <minutos> 'b3-e13)
;;<comentarios>
(define (capacidad-de-discriminacion1 discriminante ejemplos-disponibles)

(define capacidad-de-discriminacion capacidad-de-discriminacion1)
<codigo>)

;;Ejercicio 14
(he-tardado <minutos> 'b3-e14)
;;<comentarios>
(define (mayor-discriminante lista-discriminantes ejemplos-disponibles)
<codigo>)

;;Ejercicio 15
(he-tardado <minutos> 'b3-e15)
;;<comentarios>
(define (DDT0 lista-discriminantes ejemplos-disponibles)
<codigo>)

(define (DDT ejemplos)
<codigo>)

;;Ejercicio 16
(he-tardado <minutos> 'b3-e16)
;;<comentarios>
(define (capacidad-de-discriminacion2 discriminante ejemplos-disponibles)
<codigo>)

;;Ejercicio 17
(he-tardado <minutos> 'b3-e17)
;;<comentarios>
