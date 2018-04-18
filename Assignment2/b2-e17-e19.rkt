;;Plantilla b2-e17-e19.scm
;;Autor: Patricia Mayo Tejedor

;;Ejercicio 17
(he-tardado 30 'b2-e17)
;; (score-CL '((*) (*) (subiendo) (*) (*) (*)) PSET NSET)
(define instanciasAceptadas
  (lambda (concepto SET)
        (length (filter
                 (lambda (S) (match-CL concepto (drop-right S 1))) SET))))

(define (score-CL concepto-CL PSET NSET)
 (let* ((totalInstancias (+ (length PSET) (length NSET)))
        (positivasAceptadas (instanciasAceptadas concepto-CL PSET))
        (negativasNoAceptadas (- (length NSET) (instanciasAceptadas concepto-CL NSET))))
   (/ (+ positivasAceptadas negativasNoAceptadas) totalInstancias)))

;; Ejercicio 18
(he-tardado 200 'b2-e18)
;; Creo que hay un error con el pseudocodigo del libro, ya que este dice que HGSO devuelva el concepto en CSET con mas score
;; Sin embargo en el enunciado parece como que se pide que se devuelva CSET entero, y ya en HGS elegir uno al azar
;; Lo he dejado como en el libro por ahora
;; Como BeamSize he elegido 4, si pongo infinito entonces se comporta como el EGS.
;; > (HGS0 PSET NSET '() '(((*) (*) (*) (*) (*) (*))))  o   (HGS ejemplos)
;; '(((*) (*) (subiendo) (*) (*) (*)))
;; El resultado concuerda con el concepto buen dia para salir al campo
(define (HGS0 PSET NSET CSET HSET)
    (let* ((Beam-Size 4)
           (positivoAlAzar (obtener-al-azar PSET)))
      (define getOPENyCLOSE
        (lambda (indice PSET NSET CSET HSET OPEN-SET)
          (if (eq? indice (length HSET))
              (append (list OPEN-SET) (list CSET))
              ; else 
              (let* (;Variables locales
                     (H (list-ref HSET indice))
                     (SPECS (especializaciones-CL H (car ejemplos) positivoAlAzar))
                     (NEW-SET
                      (filter
                       (lambda (S) (> (score-CL S PSET NSET) (score-CL H PSET NSET)))
                       SPECS)))
                (if (eq? NEW-SET empty)
                    (getOPENyCLOSE (+ indice 1) PSET NSET (append CSET (list H)) HSET OPEN-SET)
                    (let* (
                           (eliminarS
                            (filter
                             (lambda (S)
                               (ormap
                                 (lambda (C)
                                   (and
                                    (test-CL>= S C)
                                    (> (score-CL C PSET NSET) (score-CL S PSET NSET))))
                                          CSET)
                               ) NEW-SET))
                           (eliminarC
                            (filter
                             (lambda (S)
                               (ormap
                                 (lambda (C)
                                   (and
                                    (test-CL>= S C)
                                    (<= (score-CL C PSET NSET) (score-CL S PSET NSET))))
                                          CSET)
                               ) NEW-SET)))
                      (getOPENyCLOSE
                       (+ indice 1)
                       PSET
                       NSET
                       (remq* eliminarC CSET)
                       HSET
                       (remq* eliminarS (append NEW-SET OPEN-SET)))
                    )
                )
              )
          )))
      
     (define openyclose (getOPENyCLOSE 0 PSET NSET CSET HSET '()))
     (define OPEN-SET (list-ref openyclose 0))
     (define CLOSE-SET (list-ref openyclose 1))
     ; Encuentra los conceptos con mayor score
     (define scoresMasAltos
       (lambda (numero SET)
         (define scores (map (lambda (S) (score-CL S PSET NSET)) SET))
         (define sortedList (sort scores >))
         (define scoresMasAltos (take sortedList (min numero (length sortedList))))
         (map
          (lambda(score)
            (list-ref SET (index-of scores score))
            ) scoresMasAltos)))
     (if (eq? OPEN-SET empty)
         (scoresMasAltos 1 CLOSE-SET)
         (let* (
                (BEST-SET (scoresMasAltos Beam-Size (append OPEN-SET CLOSE-SET)))
                (CSETFinal (lset-intersection eq? CLOSE-SET BEST-SET))
                (OPEN-SETFinal (lset-intersection eq? OPEN-SET BEST-SET)))
           (HGS0 PSET NSET CSETFinal OPEN-SETFinal)
         ))
      )
  )

(define (HGS ejemplos)
  (define (HGS0 PSET NSET CSET HSET)
    (let* ((Beam-Size 4)
           (positivoAlAzar (obtener-al-azar PSET)))
      (define getOPENyCLOSE
        (lambda (indice PSET NSET CSET HSET OPEN-SET)
          (if (eq? indice (length HSET))
              (append (list OPEN-SET) (list CSET))
              ; else 
              (let* (;Variables locales
                     (H (list-ref HSET indice))
                     (SPECS (especializaciones-CL H (car ejemplos) positivoAlAzar))
                     (NEW-SET
                      (filter
                       (lambda (S) (> (score-CL S PSET NSET) (score-CL H PSET NSET)))
                       SPECS)))
                (if (eq? NEW-SET empty)
                    (getOPENyCLOSE (+ indice 1) PSET NSET (append CSET (list H)) HSET OPEN-SET)
                    (let* (
                           (eliminarS
                            (filter
                             (lambda (S)
                               (ormap
                                 (lambda (C)
                                   (and
                                    (test-CL>= S C)
                                    (> (score-CL C PSET NSET) (score-CL S PSET NSET))))
                                          CSET)
                               ) NEW-SET))
                           (eliminarC
                            (filter
                             (lambda (S)
                               (ormap
                                 (lambda (C)
                                   (and
                                    (test-CL>= S C)
                                    (<= (score-CL C PSET NSET) (score-CL S PSET NSET))))
                                          CSET)
                               ) NEW-SET)))
                      (getOPENyCLOSE
                       (+ indice 1)
                       PSET
                       NSET
                       (remq* eliminarC CSET)
                       HSET
                       (remq* eliminarS (append NEW-SET OPEN-SET)))
                    )
                )
              )
          )))
      
     (define openyclose (getOPENyCLOSE 0 PSET NSET CSET HSET '()))
     (define OPEN-SET (list-ref openyclose 0))
     (define CLOSE-SET (list-ref openyclose 1))
     ; Encuentra los conceptos con mayor score
     (define scoresMasAltos
       (lambda (numero SET)
         (define scores (map (lambda (S) (score-CL S PSET NSET)) SET))
         (define sortedList (sort scores >))
         (define scoresMasAltos (take sortedList (min numero (length sortedList))))
         (map
          (lambda(score)
            (list-ref SET (index-of scores score))
            ) scoresMasAltos)))
     (if (eq? OPEN-SET empty)
         (scoresMasAltos 1 CLOSE-SET)
         (let* (
                (BEST-SET (scoresMasAltos Beam-Size (append OPEN-SET CLOSE-SET)))
                (CSETFinal (lset-intersection eq? CLOSE-SET BEST-SET))
                (OPEN-SETFinal (lset-intersection eq? OPEN-SET BEST-SET)))
           (HGS0 PSET NSET CSETFinal OPEN-SETFinal)
         ))
      )
  )
  (define PSET (car (separarClases ejemplos)))
  (define NSET (list-ref (separarClases ejemplos) 1))
  (define positivoAlAzar (obtener-al-azar PSET))
  (define listaCSET (HGS0 PSET NSET '() (list (concepto-CL-mas-general (car ejemplos)))))
  listaCSET
)

;;Ejercicio 19
(he-tardado 20 'b2-e19)
;; Curiosamente con HGS y ionosphere se devuelve el concepto mas general posible, mientras que con el EGS se devolvia vacio
;;> (HGS ionosphere)
;;'(((*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*)))
;; agaricus-lepiota ha tardado muchisimo, caso dos minutos en ejecutarse
;;> (HGS agaricus-lepiota)
;;'(((*) (*) (*) (*) (n) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*)))
