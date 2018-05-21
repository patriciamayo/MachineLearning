;;Plantilla b3-e1-e2.scm
;;Autor: Patricia Mayo Tejedor

;;Ejercicio 1
(he-tardado 90 'b3-e1)
;; Al final no se si se quiere que se devuelva una clase por defecto o no en caso de que ningun concepto pase
;; Ha costado entender el concepto de quasiquote y unquote, sobre buscar lo que significaban, ya que poner ` y , no sirve de mucho en google
;; Tambien un poco de lio con => pero despues poniendo ` , se ha resuelto
;; Por ultimo la funcion eval ha sido muy util para leer una variable como procedure y no como symbol


;> (define concepto-LD '((match-CL ((soleado)(30 40)(50 60)(*)) => +)(match-CL ((*)(*)(*)(*)) => -)))
;> (define ejemplo-sin-clase '(soleado 30 40 si))
;  (define AtributosProfesor '((perspectiva (soleado nublado lluvioso)) (temperatura numerico) (humedad numerico) (viento (si no)) (clase (+ -))))

;> (LDi '((match-CL ((soleado)(30 40)(50 60)(*)) => +)(match-CL ((*)(*)(*)(*)) => -))'(soleado 30 40 si))
;'(soleado 30 40 si -)
;> (LDi `((match-LUU ,(list AtributosProfesor '(0 1 1 0 -30)) => +) (match-LUU ,(list AtributosProfesor '(0 1 -1 0 -20)) => -)) '(soleado 30 40 si))
;'(soleado 30 40 si +)
;> (LDi `((match-LUU ,(list AtributosProfesor '(0 1 1 0 -30)) => +)(match-LUU ,(list AtributosProfesor '(0 1 -1 0 -20)) => -)) '(soleado 10 10 si))
;'(soleado 10 10 si)

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


;;Ejercicio 2
(he-tardado 40 'b3-e2)
;; De nuevo prolemas, que he resuelto con la funcion eval, que me han llevado mas tiempo de lo esperado
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








