;;Plantilla b2-e30-e33.scm
;;Autor: Patricia Mayo Tejedor

;;Ejercicio 30
(he-tardado 160 'b2-e30)
;; Los resultados concuerdan, se ha asignado mayor peso a la temperatura, mientras que el % en humedad pesa mucho de manera negativa
;; Jugar con el n devuelve vectores con numeros proporcionales a n, pero todos guardan relacion similar, dando mayor peso a la temperatura y la humedad

;; (define concepto-UU (nuevo-conceptoUU (car ejemplos) 1))
;;> (PRM concepto-UU ejemplos)
;;'(((perspectiva (lluvia variable bueno frio niebla calorSeco))
;;   (temperaturaGrados numerico)
;;   (temperaturaTendencia (bajando estable subiendo))
;;   (barometroTendencia (bajando estable subiendo))
;;   (humedadEn% numerico)
;;   (viento (si no))
;;   (clase (+ -)))
;;  (1.1314324763924712 14.606823090142385 1.2694679417762282 0.377506570266878 -17.213593650056858 0.7236111290080276 1.0339599634203298))
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

;;Ejercicio 31
(he-tardado 90 'b2-e31)
;; Los resultados cambian bastante dependiendo del H inicializado al azar,
;; pero la mayoria de las veces el resultado se acerca mucho al del ejercicio anterior
;;
;; (define COUNT 1000)
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
                (recorrerCOUNT (PRM H ejemplos) ISET (- COUNT 1)))))))
  (recorrerCOUNT H ISET contador)))

;;Ejercicio 32
(he-tardado 400 'b2-e32)
;; Este ejercicio ha sido realmente complicado, cada parametro influencia muchisimo los resultados, he ido jugando con ellos hasta dar con un resultado que tiene sentido
;; Al final lo mejor ha sido aceptar un error del 15% y poner el gain a 0.2
;; Los resultados coinciden con las expectativas, donde la perspectiva y la humedad son los atributos mas relevantes
;;> (LMS ejemplos)
;;'(0.42701965659393204
;;  -0.046724185514876226
;;  0.05610928793221992
;;  0.30324967370273836
;;  0.44549003049310454
;;  0.21926560010929708
;;  0.3616204059722472)
(define COUNT 1000)
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
  (define vectorPesos (recorrerCOUNT H ISET COUNT Aa))
  (if (eq? (length (recorrerCOUNT H ISET COUNT Aa)) 1)
      (append (list ATTS) (recorrerCOUNT H ISET COUNT Aa))
      (append (list ATTS) (list (recorrerCOUNT H ISET COUNT Aa))))
  ;(append (list ATTS) (recorrerCOUNT H ISET COUNT Aa))
  ))

;;Ejercicio 33
(he-tardado 60 'b2-e33)
;; He tardado mucho porque la funcion precision no era la correcta
;; PCP ha sido mas efectivo que LMS, LMS se enfoca en reducir el error, mas que en eliminar clasificaciones errones como el PCP
;; Ademas el LMS es una generaliazacion del PCP, por lo que se entiende que el PCP haya funcionado mejor para este caso
;; He intentado compararlos con el mismo gain n = 0.2
;;> (stratified-cross-validation LMS LUUi ejemplosJuntos 2)
;;0.4423076923076923
;;> (stratified-cross-validation PCP LUUi ejemplosJuntos 2)
;;0.5544871794871795



