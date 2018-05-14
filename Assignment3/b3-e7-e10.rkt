;;Plantilla b3-e7-e10.scm
;;Autor: Patricia Mayo Tejedor

;;Ejercicio 7
(he-tardado 60 'b3-e7)
; El tiempo incluye leer el tema 7
; > (adc '(soleado 20) '((((*) (-inf.0 30)) -> (=> +)) (((*)((30) +inf.0)) -> (=> -))))   > '(=> +)
; > (adc '(soleado 15) '((((*) (20 30)) -> (=> +)) (((*) ((30) +inf.0)) -> (=> -))))      > '()
(define (adc ejemplo-sin-clase ramas-JC-adc)
(let* ((rama (find
              (lambda (rama)
                (match-CL (first rama) ejemplo-sin-clase))
              ramas-JC-adc)))
  (if (eq? rama #f)
      '()
      (list-ref rama 2))
  ))

;;Ejercicio 8
(he-tardado 20 'b3-e8)
(define multivariadoJC
  '(
    (((soleado)(-inf.0 30)) -> (=> +))
    (((soleado)((30) +inf.0)) -> (=> -))
    (((nublado)(*)) -> (=> -))
    (((lluvioso)(-inf.0 10)) -> (=> -))
    (((lluvioso)((10) +inf.0)) -> (=> +))
    )
 )

;;Ejercicio 9
(he-tardado <minutos> 'b3-e9)
;;<comentarios>
(define (adg ejemplo-sin-clase ramas-JC)
<codigo>)

;;Ejercicio 10
(he-tardado <minutos> 'b3-e10)
;;<comentarios>
(define (JCi concepto-JC ejemplo-sin-clase)
<codigo>)
