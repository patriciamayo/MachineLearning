;;Plantilla b2-e23-e26.scm
;;Autor: Patricia Mayo Tejedor

;;Ejercicio 23
(he-tardado 15 'b2-e23)
;; Tengo que pasarlo con apostrofe delante del valor nominal
;> (traducir '(perspectiva (soleado nublado lluvioso)) 'lluvioso)
;2
(define (traducir meta-atributo valor)
  (let* ((tipos (list-ref meta-atributo 1))
         (esNominal (list? tipos)))
    (cond
      [esNominal (index-of tipos valor)]
      [else valor])))

;;Ejercicio 24
(he-tardado 15 'b2-e24)
;;Si sale el ultimo elemento negativo, entonces supongo que el umbral sera positivo.

;> (nuevo-conceptoUU (car ejemplos) 1)
;'(((perspectiva (lluvia variable bueno frio niebla calorSeco))
;   (temperaturaGrados numerico)
;   (temperaturaTendencia (bajando estable subiendo))
;   (barometroTendencia (bajando estable subiendo))
;   (humedadEn% numerico)
;   (viento (si no))
;   (clase (+ -)))
;  (0.8650313000954946
;   0.06380477856644307
;   -0.8482332537957739
;   0.34748279076917576
;   0.8672124567395523
;   0.9073510702534167
;   0.02861993106849159))
(define (nuevo-conceptoUU metadatos init)
(let* ((vector (map (lambda (x)(* init (- (* 2 (random)) 1))) metadatos)))
  (list metadatos vector)))


;;Ejercicio 25
(he-tardado 60 'b2-e25)
;; La sugerencia del producto escalar me ha venido muy bien para entender mejor lo que habia que hacer

;(define AtributosProfesor '((perspectiva (soleado nublado lluvioso)) (temperatura numerico) (humedad numerico) (viento (si no)) (clase (+ -))))
;> (match-LUU (list AtributosProfesor '(0 1 1 0 -30)) '(lluvioso 10 20 si))
;#t
;> (match-LUU (list AtributosProfesor '(0 1 1 0 -31)) '(lluvioso 10 20 si))
;#f

(define (productoEscalar vector1 vector2)
  (apply + (map * vector1 vector2)))

(define (traducirEjemplo metadatos ejemplo-sin-clase)
  (map
   (lambda (meta-atributo valor)
     (traducir meta-atributo valor))
   metadatos ejemplo-sin-clase))

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

;> (LUUi (list AtributosProfesor '(0 1 1 0 -31)) '(lluvioso 10 20 si))
;'(lluvioso 10 20 si -)
;> (LUUi (list AtributosProfesor '(0 1 1 0 -30)) '(lluvioso 10 20 si))
;'(lluvioso 10 20 si +)
(define (LUUi conceptoUU ejemplo-sin-clase)
(let* ((pasaUmbral (match-LUU conceptoUU ejemplo-sin-clase)))
  (if pasaUmbral (append ejemplo-sin-clase (list '+)) (append ejemplo-sin-clase (list '-)))))
