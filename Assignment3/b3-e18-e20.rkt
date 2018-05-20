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
(he-tardado 100 'b3-e20)
;; Interesante que las dos listas de decision tienen la misma cantidad, sin emabrgo el atributo numerico se ha obtenido de manera diferente
;> (define ejemplos-test '(((perspectiva (soleado lluvioso nublado))(temperatura numerico) (clase (+ -))) (soleado 10 -)(soleado 25 +)(lluvioso 30 -)(nublado 30 +)))
;; ===================================  (define capacidad-de-discriminacion capacidad-de-discriminacion1) ==========================
;> (DTL-DDT ejemplos-test)
;'((match-CL ((soleado) ((10) +inf.0)) => +)
;  (match-CL ((soleado) ((10) +inf.0)) => -)
;  (match-CL ((lluvioso) (*)) => -)
;  (match-CL ((nublado) (*)) => +))
;; ===================================  (define capacidad-de-discriminacion capacidad-de-discriminacion2) ==========================
;> (DTL-DDT ejemplos-test)
;'((match-CL ((soleado) ((25) +inf.0)) => +)
;  (match-CL ((soleado) (-inf.0 25)) => -)
;  (match-CL ((lluvioso) (*)) => -)
;  (match-CL ((nublado) (*)) => +))
;
;
;
;; Si ejecuto mis propios ejemplos con capacidad-de-discriminacion1 me he dado cuenta que me salen algunas incosistencias. 
;; por ejemplo el mismo concepto da dos clases: (match-CL ((frio) ((5) +inf.0) (*) (*) (*) (*)) => -) (match-CL ((frio) ((5) +inf.0) (*) (*) (*) (*)) => +)
; Esto no ocurre en en el caso de capacidad-de-discriminacion2, ya que la ramificaciones se reducen considerablemente, evitando precisimante este tipo de errores
;; ===================================  (define capacidad-de-discriminacion capacidad-de-discriminacion1) ==========================
;> (DTL-DDT ejemplos)
;'((match-CL ((lluvia) (*) (*) (*) (*) (*)) => -)
;  (match-CL ((variable) (*) (*) (*) (*) (*)) => +)
;  (match-CL ((bueno) (*) (*) (*) (*) (*)) => +)
;  (match-CL ((frio) ((5) +inf.0) (*) (*) (*) (*)) => -)
;  (match-CL ((frio) ((5) +inf.0) (*) (*) (*) (*)) => +)
;  (match-CL ((frio) (-inf.0 5) (*) (*) (*) (*)) => +)
;  (match-CL ((niebla) (*) (*) (*) (*) (*)) => -)
;  (match-CL ((calorSeco) (*) (*) (*) (*) (*)) => +))
;; ===================================  (define capacidad-de-discriminacion capacidad-de-discriminacion2) ==========================
;> (DTL-DDT ejemplos)
;'((match-CL ((lluvia) (*) (*) (*) (*) (*)) => -)
;  (match-CL ((variable) (*) (*) (*) (*) (*)) => +)
;  (match-CL ((bueno) (*) (*) (*) (*) (*)) => +)
;  (match-CL ((frio) ((6) +inf.0) (*) (*) (*) (*)) => -)
;  (match-CL ((frio) (-inf.0 6) (*) (*) (*) (*)) => +)
;  (match-CL ((niebla) (*) (*) (*) (*) (*)) => -)
;  (match-CL ((calorSeco) (*) (*) (*) (*) (*)) => +))
;
;
;
;; En lo que se refiere a la preciosion, aunque hay aaumentado ligeramente en el caso de lymphography, sigue realmente igual
;; Todas las precisiones con todos los ejemplos con todos los algoritmos han dado siempre valores al rededor del 50%
;> (stratified-cross-validation DTL-DDT LDi lymphography 10)
;0.4742857142857143
;> (stratified-cross-validation DTL-DDT LDi agaricus-lepiota 10)
;0.5043133744145323
