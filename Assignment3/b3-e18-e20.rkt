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
(he-tardado <minutos> 'b3-e19)
;;<comentarios>
(define (DTL arbol-decision-clasico)
<codigo>)

(define (DTL-DDT ejemplos)
<codigo>)

;;Ejercicio 20
(he-tardado <minutos> 'b3-e20)
;;<comentarios>


