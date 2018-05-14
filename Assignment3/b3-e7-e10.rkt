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
(he-tardado 60 'b3-e9)
; He tardado mas de lo esperado por problemas con ` y ,
; (define AtributosProfesor '((perspectiva (soleado nublado lluvioso)) (temperatura numerico) (humedad numerico) (viento (si no)) (clase (+ -))))
; (adg '(soleado 30 40 si) `(((match-LUU (, AtributosProfesor (0 1 1 0 -30))) -> (=> +))((match-LUU (, AtributosProfesor (0 1 -1 0 -20))) -> (=> -)))))
(define (adg ejemplo-sin-clase ramas-JC)
(let* ((rama (find
              (lambda (rama)
                (define concepto (first rama))
                (define matchProcedure (first concepto))
                (define conceptoCL (list-ref concepto 1))
                ((eval matchProcedure) conceptoCL ejemplo-sin-clase))
              ramas-JC)))
  (if (eq? rama #f)
      '()
      (list-ref rama 2))
  ))

;;Ejercicio 10
(he-tardado 60 'b3-e10)
; (JCi JC '(lluvioso 50))
(define (JCi concepto-JC ejemplo-sin-clase)
(let* ()
  (define recorrerJC
    (lambda (concepto)
      (cond
        [(eq? concepto empty) #f]
        [(eq? (first concepto) '=>) (list-ref concepto 1)]
        [else  (recorrerJC ((eval (first concepto)) '(lluvioso 50) (cdr concepto)))]
       )
    ))
  (define clase (recorrerJC concepto-JC))
  (if (eq? clase #f)
      ejemplo-sin-clase
      (append ejemplo-sin-clase (list clase)))))
