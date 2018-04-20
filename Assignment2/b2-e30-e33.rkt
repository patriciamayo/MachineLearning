;;Plantilla b2-e30-e33.scm
;;Autor: Patricia Mayo Tejedor

;;Ejercicio 30
(he-tardado 160 'b2-e30)
;; Los resultados concuerdan, se ha asignado mayor peso a la temperatura, mientras que el % en humedad pesa mucho de manera negativa
;; Jugar con el n devuelve vectores con numeros proporcionales a n, pero todos guardan relacion similar, dando mayor peso a la temperatura y la humedad
;;> (PRM concepto-UU ejemplos)
;;'(((perspectiva (lluvia variable bueno frio niebla calorSeco))
;;   (temperaturaGrados numerico)
;;   (temperaturaTendencia (bajando estable subiendo))
;;   (barometroTendencia (bajando estable subiendo))
;;   (humedadEn% numerico)
;;   (viento (si no))
;;   (clase (+ -)))
;;  (1.1314324763924712 14.606823090142385 1.2694679417762282 0.377506570266878 -17.213593650056858 0.7236111290080276 1.0339599634203298))
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
                  (recorrerISET (+ indice 1) ISET (list ATTS (append vectorAjustado (list (+ (* S n) umbral)))))
                  )
                )
            )
         )
      ))
     (recorrerISET 0 ISET concepto-UU)))

;;Ejercicio 31
(he-tardado 90 'b2-e31)
;; Los resultados cambian bastante dependiendo del H inicializado al azar,
;; pero la mayoria de las veces el resultado se acerca mucho al del ejercicio anterior
;;
;;> (PCP ejemplos)
;;'(((perspectiva (lluvia variable bueno frio niebla calorSeco))
;;   (temperaturaGrados numerico)
;;   (temperaturaTendencia (bajando estable subiendo))
;;   (barometroTendencia (bajando estable subiendo))
;;   (humedadEn% numerico)
;;   (viento (si no))
;;   (clase (+ -)))
;;   (1.3777014924590267 16.110625585496916 1.7645791401696536 1.4685299317013067 -17.06350007243641  0.3475236908264756 1.422552911725595))
(define COUNT 1000)
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
                (recorrerCOUNT (PRM concepto-UU ejemplos) ISET (- COUNT 1)))))))
  (recorrerCOUNT H ISET contador)))

;;Ejercicio 32
(he-tardado <minutos> 'b2-e32)
;;<comentarios>
(define COUNT 1000)
(define (LMS ejemplos)
<codigo>)

;;Ejercicio 33
(he-tardado <minutos> 'b2-e33)
;;<comentarios>



