;;Plantilla b2-e34-e36.scm
;;Autor: Patricia Mayo Tejedor

;;Ejercicio 34
(he-tardado 20 'b2-e34)
;; (distancia '(soleado 10 20 si) '(soleado 15 21 no -))   ->    5.196152422706632
;; Lo he hecho bastante rapido. Me queda en duda si tengo que cuidar mas los datos de entrada. En el ejercicio del pdf pone
;; que los parametros pueden ser sin clase o con clase, y no te dice cual. Sin embargo en la plantilla parece ser que siempre
;; se pone sin clase el primero y con clase el segundo, por lo que no tengo que depurarlos?
(define (IB ejemplos) ejemplos)
(define (distancia ejemplo-sin-clase ejemplo)
  (let* ((ejemplo-y (drop-right ejemplo 1)))
    (define valor
      (lambda (x y)
        (cond
          [(number? x) (expt (- y x) 2)]
          [(eq? x y) 0]
          [else 1])))
    (sqrt (apply + (map
                    (lambda (x y)
                      (valor x y))
                    ejemplo-sin-clase ejemplo-y)))))

;;Ejercicio 35
(he-tardado 30 'b2-e35)
;; Espero haber entendido bien el ejercicio ya que el pdf es diferente a la plantilla.
;; De verdad que creo que se necesitan ejemplos en todos los ejercicios para entender bien que es lo que se pide
;; Lo que he hecho es buscar de entre todos los ejemplos el que sea mas cercano al concepto-IB, y luego agregarle a este la clase del concepto-IB
(define concepto-IB '(lluvia 5 bajando bajando 100 si -))
(define (IBi concepto-IB ejemplos-sin-clase)
(let* ((distancias (map
                    (lambda (ejemplo-sin-clase)
                      (distancia ejemplo-sin-clase concepto-IB)) ejemplos-sin-clase))
       (min-distancia (apply min distancias))
       (index-de-min-distancia (index-of distancias min-distancia))
       (ejemplo-mas-cercano (list-ref ejemplos-sin-clase index-de-min-distancia)))
  (append ejemplo-mas-cercano (list (last concepto-IB)))))


;;Ejercicio 36
(he-tardado 30 'b2-e36)
;; Espero haber entendido bien el ejercicio
;; He hecho que devuelva true si el ejemplo que se pasa es el mas cercano al concepto-IB y que devuelva false en caso contrario
;; Me mosquea porque no le paso por parametro todos los ejemplos-sin-clase que se necesitan para el IBi
;; > (define concepto-IB '(lluvia 5 bajando bajando 100 si -))
;; > (IBi concepto-IB ejemplos-sin-clase)
;; '(lluvia 5 bajando bajando 100 si -)
;; > (match-IB concepto-IB '(lluvia 5 bajando bajando 100 si))
;; #t
;; > (match-IB concepto-IB '(soleado 5 bajando bajando 100 si))
;; #f
(define (match-IB concepto-IB ejemplo-sin-clase)
  (let* ((ejemplo-mas-cercano (IBi concepto-IB ejemplos-sin-clase))
         (ejemplo-con-clase-de-IB (append ejemplo-sin-clase (list (last concepto-IB)))))
   (equal? ejemplo-mas-cercano ejemplo-con-clase-de-IB)))




