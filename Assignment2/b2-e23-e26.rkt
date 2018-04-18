;;Plantilla b2-e23-e26.scm
;;Autor: Patricia Mayo Tejedor

;;Ejercicio 23
(he-tardado 15 'b2-e23)
;; Tengo que pasorlo con apostrofe delante del valor nominal
;; (traducir '(perspectiva (soleado nublado lluvioso)) 'lluvioso)
(define (traducir meta-atributo valor)
  (let* ((tipos (list-ref meta-atributo 1))
         (esNominal (list? tipos)))
    (cond
      [esNominal (index-of tipos valor)]
      [else valor])))

;;Ejercicio 24
(he-tardado 15 'b2-e24)
;;Si sale el ultimo elemento negativo, entonces supongo que el umbral sera positivo.
(define (nuevo-conceptoUU metadatos init)
(let* ((vector (map (lambda (x)(* init (- (* 2 (random)) 1))) metadatos)))
  (list metadatos vector)))

;;Ejercicio 25
(he-tardado 60 'b2-e25)
;; La sugerencia del producto escalar me ha venido muy bien para entender mejor lo que habia que hacer
(define (productoEscalar vector1 vector2)
  (apply + (map * vector1 vector2)))

(define (match-LUU conceptoUU ejemplo-sin-clase)
  (let* ((metadatos (list-ref conceptoUU 0))
         (vectorPesos (list-ref conceptoUU 1))
         (ejemploTraducido
          (map
           (lambda (meta-atributo valor)
             (traducir meta-atributo valor))
           metadatos ejemplo-sin-clase))
         (producto (productoEscalar vectorPesos (append ejemploTraducido '(1)))))
    (if (eq? producto 0) #t (positive? producto))))

;;Ejercicio 26
(he-tardado 20 'b2-e26)
;;El append ha costado un poco
(define (LUUi conceptoUU ejemplo-sin-clase)
(let* ((pasaUmbral (match-LUU conceptoUU ejemplo-sin-clase)))
  (if pasaUmbral (append ejemplo-sin-clase (list '+)) (append ejemplo-sin-clase (list '-)))))
