;;Plantilla b2-e15-e16.scm
;;Autor: Patricia Mayo Tejedor

;;Ejercicio 15
(he-tardado 200 'b2-e15)
;; Me ha costado mucho hacer este ejercicio.
;; El pseudocodigo de los dos ultimos for-each del libro no se entiende bien
;; A la hora de crear las especialiazaciones hay que pasar un ejemplo (he cogido uno al azar positivo pero no queda claro de donde sacarlo)
;; Las respuestas concuerdan con lo que se entiende como 'buen dia para salir al campo'

;; > (EGS0 PSET NSET '() (list (concepto-CL-mas-general (car ejemplos))))
;;'(((variable) (*) (*) (*) (*) (*))
;;  ((bueno) (*) (*) (*) (*) (*))
;;  ((calorSeco) (*) (*) (*) (*) (*))
;;  ((*) (*) (subiendo) (*) (*) (*)))

;; > (EGS ejemplos)
;;'((calorSeco) (*) (*) (*) (*) (*))

; Devuelve una lista con tantos elementos como clases hay. Cada elemento contiene todos los casos de esa clase
(define separarClases
  (lambda (ejemplos)
    (define tiposDeClases (atributo 'clase ejemplos))
    (define casos (list-tail ejemplos 1))
    (define casosAgrupadosPorClase
      (lambda (index tiposDeClases casos agrupacion)
        (if (= index (length tiposDeClases))
            agrupacion
            (let ((claseAgrupada  (filter
                               (lambda (caso)
                                 (eqv? (list-ref caso (- (length caso) 1)) (list-ref tiposDeClases index)))
                               casos)))
              (casosAgrupadosPorClase (+ index 1) tiposDeClases casos (append agrupacion (list claseAgrupada)))))))
    (casosAgrupadosPorClase 0 tiposDeClases casos '())))



; He creado la variable ejemplos dentro del algoritmo para poder ejecutarlo solo
; Pero claro siempre se va a ejecutar con mis ejemplos
; Para usar EGS0 por si mismo
(define PSET (car (separarClases ejemplos)))
(define NSET (list-ref (separarClases ejemplos) 1))
(define (EGS0 PSET NSET CSET HSET)
  (let* ()
    ; Para usar EGS0 por si mismo cargar aqui los ejemplos que se quieran usar
    (define ejemplos (leer-ejemplos direccionEjemplos1))
    (define HParaEliminar
      (filter (lambda (H)(not (andmap (lambda (P)
                                (match-CL H (drop-right P 1))) PSET))) HSET))
     (define HParaEliminarYAnadir
      (filter (lambda (H)(not (ormap (lambda (N)
                                (match-CL H (drop-right N 1))) NSET))) HSET))
    (define HSETfinal
      (remq* (append HParaEliminar HParaEliminarYAnadir) HSET))
    (define CSETfinal (append HParaEliminarYAnadir CSET))
    (define numeroNegativasAceptadas
      (lambda (concepto)
        (length (filter
                 (lambda (N) (match-CL concepto (drop-right N 1))) NSET))))
    (if (equal? HSETfinal empty)
        CSETfinal
        (let* ()
          (define SPECS
            (append-map
             (lambda (H)
               (define negativasDeH (numeroNegativasAceptadas H))
               (define especialiazaciones
                 (especializaciones-CL H (car ejemplos) '(bueno 26 estable subiendo 43 si +)))
               (filter
                (lambda (especialiazacion)
                  (< (numeroNegativasAceptadas especialiazacion) (numeroNegativasAceptadas H)))
                especialiazaciones)
               ) HSETfinal))
          (define NEWSET
            (filter
              (lambda (S)
                (andmap
                     (lambda (C) (eq? (cmp-concepto-CL S C) -1)) CSETfinal))
            SPECS))
        (EGS0 PSET NSET CSETfinal NEWSET)))))

(define (EGS ejemplos)
  (let* ()
  (define (EGS0 PSET NSET CSET HSET)
  (let* ()
    (define HParaEliminar
      (filter (lambda (H)(not (andmap (lambda (P)
                                (match-CL H (drop-right P 1))) PSET))) HSET))
     (define HParaEliminarYAnadir
      (filter (lambda (H)(not (ormap (lambda (N)
                                (match-CL H (drop-right N 1))) NSET))) HSET))
    (define HSETfinal
      (remq* (append HParaEliminar HParaEliminarYAnadir) HSET))
    (define CSETfinal (append HParaEliminarYAnadir CSET))
    (define numeroNegativasAceptadas
      (lambda (concepto)
        (length (filter
                 (lambda (N) (match-CL concepto (drop-right N 1))) NSET))))
    (if (equal? HSETfinal empty)
        CSETfinal
        (let* ()
          (define SPECS
            (append-map
             (lambda (H)
               (define negativasDeH (numeroNegativasAceptadas H))
               (define especialiazaciones
                 (especializaciones-CL H (car ejemplos) positivoAlAzar))
               (filter
                (lambda (especialiazacion)
                  (< (numeroNegativasAceptadas especialiazacion) (numeroNegativasAceptadas H)))
                especialiazaciones)
               ) HSETfinal))
          (define NEWSET
            (filter
              (lambda (S)
                (andmap
                     (lambda (C) (eq? (cmp-concepto-CL S C) -1)) CSETfinal))
            SPECS))
        (EGS0 PSET NSET CSETfinal NEWSET)))))
  (define PSET (car (separarClases ejemplos)))
  (define NSET (list-ref (separarClases ejemplos) 1))
  (define positivoAlAzar (obtener-al-azar PSET))
  (define listaCSET (EGS0 PSET NSET '() (list (concepto-CL-mas-general (car ejemplos)))))
  (if (equal? listaCSET empty) '() (obtener-al-azar listaCSET))))



;;Ejercicio 16
(he-tardado 90 'b2-e16)
;; He tenido que resolver otros ejercicios
;; El EGS es muy sensible al ruido, que es la razon mas probable por la que en el caso de ionosphere no se ha podido enocntrar nada
;;> (EGS ionosphere)
;;'()
;; El EGS es exhaustivo y esto tiene un alto coste computacional, en el caso agaricus-lepiota ha tardado bastante mas en resolverlo en comparacion con nuetsros ejemplos
;;> (EGS agaricus-lepiota)
;;'((*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (r) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*))
(define direccionAgaricus "/Users/patriciamayotejedor/Documents/Private Development/UNED/Machine Learning/MachineLearning/Assignment2/agaricus-lepiota.scm")
(define agaricus-lepiota (leer-ejemplos direccionAgaricus))
(define direccionIonosphere  "/Users/patriciamayotejedor/Documents/Private Development/UNED/Machine Learning/MachineLearning/Assignment2/ionosphere.scm")
(define ionosphere (leer-ejemplos direccionIonosphere))