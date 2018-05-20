;;Plantilla b3-e18-e20.scm
;;Autor: Patricia Mayo Tejedor

;;Ejercicio 18
(he-tardado 45 'b3-e18)
;; Resulta que tenia mal la funcion test-CL>= por lo que he tenido que volver atras y arreglarla, espero no haber roto algo al cambiarla ahora
;> (union-CL1 '((soleado)(*)) '((*)(10 30)))
;'((soleado) (10 30))
;> (union-CL1 '((*)(*)) '((*)(10 30)))
;'((*) (10 30))
(define (union-CL concepto-CL1 concepto-CL2)
(let* ()
  (map
   (lambda(cl1 cl2)
     (if (test-CL>= cl1 cl2)
         cl2
         cl1 )
     ) concepto-CL1 concepto-CL2)
))

;;Ejercicio 19
(he-tardado 200 'b3-e19)
;; He segudio el algoritmo de la tabla de EML, pero no termino de entender como se tienen que ordenar las RULES
;(define JC '(adc (((soleado)(*)) -> (adc (((*)(-inf.0 30)) -> (=> +)) (((*)((30) +inf.0)) -> (=> -)) )) (((nublado)(*)) -> (=> -)) (((lluvioso)(*)) -> (adc (((*)(-inf.0 10)) -> (=> -)) (((*)((10) +inf.0)) -> (=> +)) )) ))
;> (DTL JC)
;'((match-CL ((soleado) (-inf.0 30)) => +)
;  (match-CL ((soleado) ((30) +inf.0)) => -)
;  (match-CL ((nublado) (*)) => -)
;  (match-CL ((lluvioso) (-inf.0 10)) => -)
;  (match-CL ((lluvioso) ((10) +inf.0)) => +))

(define (DTL arbol-decision-clasico)
(let* ()
  (define recorrerArbol
    (lambda(N RULES LHS)
      (if (empty? N)
          '()
          (if (eq? (first N) '=>) ; Ver si es nodo terminal
              (list (list* 'match-CL LHS N ))
              (let* ()
                (apply append (map
                               (lambda(S)
                                 (define T (first S))
                                 (define nuevoLHS
                                   (if (empty? LHS)
                                       T
                                       (union-CL T LHS)
                                       ))
                                 (recorrerArbol (list-ref S 2) RULES nuevoLHS)
                                 ) (cdr N))))))
      ))
  (remove-duplicates (recorrerArbol arbol-decision-clasico '() '()))
))

(define (DTL-DDT ejemplos)
(DTL (DDT ejemplos)))

;;Ejercicio 20
(he-tardado <minutos> 'b3-e20)
;;<comentarios>


