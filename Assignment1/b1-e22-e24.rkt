;;Plantilla b1-e22-e24.scm
;;Autor: Patricia Mayo Tejedor

;;Ejercicio 22
(he-tardado 150 'b1-e22)
; Resultados: Como las funciones A1 y A1i se basan en la clasificación cogida al azar los valores cambian, pero se puede ver que se consigue entre similar o mejor precisión.
;(define ejemplosJuntos (mezclar ejemplos ejemplos2))
;;
;; ======== RESUSTITUTION =============
;> (resustitution A0 A0i ejemplosJuntos)
;0.56
;> (resustitution A1 A1i ejemplosJuntos)
;0.56
;> (resustitution A1 A1i ejemplosJuntos)
;0.52
;> (resustitution A1 A1i ejemplosJuntos)
;0.76

;; ======== LEAVE ONE OUT =============
;> (leave-one-out A0 A0i ejemplosJuntos)
;0.56
;> (leave-one-out A1 A1i ejemplosJuntos)
;0.68
;> (leave-one-out A1 A1i ejemplosJuntos)
;0.56
;> (leave-one-out A1 A1i ejemplosJuntos)
;0.48

; Se usan todos los ejemplos para entrenar y para evaluar
(define (resustitution algoritmo interprete ejemplos)
  (let* ((ejemplos-sin-clase (map (lambda(x) (drop-right x 1)) ejemplos))
         (cogerEsencia (algoritmo ejemplos))
         (cogerExtension (map (lambda(x) (interprete cogerEsencia x)) ejemplos-sin-clase)))
      (precision (cdr ejemplos) (cdr cogerExtension))
  ))

; Se usan cada vez solo un ejemplo para evaluar (con todos los ejemplos)
(define (leave-one-out algoritmo interprete ejemplos)
  (let* ((casos (list-tail ejemplos 1)))
    (define evaluarCaso
      (lambda (casoParaEvaluar)
        (define casosDeEntrenamiento (remq casoParaEvaluar ejemplos))
        (define casoSinClase (map (lambda(x) (drop-right x 1)) (list casoParaEvaluar)))
        (define entrenar (algoritmo casosDeEntrenamiento))
        (define interpretar (map (lambda(x) (interprete entrenar x)) casoSinClase))
        (eq? (last casoParaEvaluar) (last (first interpretar)))
        ))
    (define numeroAciertos (length (filter (lambda (caso)
         (evaluarCaso caso))
       casos)))
    (exact->inexact (/ numeroAciertos (length casos)))
    ))



;;Ejercicio 23
(he-tardado 40 'b1-e23)
;(define ejemplosSeparados (separar 0.67 ejemplosJuntos))
;(define atributos (car ejemplosJuntos))
;(define ejemplos-aprender (append (list atributos) (list-ref ejemplosSeparados 0)))
;(define ejemplos-evaluar (append (list atributos) (list-ref ejemplosSeparados 1)))

;> (holdout A0 A0i ejemplosEntrenamiento ejemplosEvaluacion)
;0.625
;> (holdout A1 A1i ejemplosEntrenamiento ejemplosEvaluacion)
;0.5
;> (holdout A1 A1i ejemplosEntrenamiento ejemplosEvaluacion)
;0.625
;> (holdout A1 A1i ejemplosEntrenamiento ejemplosEvaluacion)
;0.375

(define (holdout algoritmo interprete ejemplos-aprender ejemplos-evaluar)
 (let* ((ejemplosEvaluacionSinClase (list-tail (map (lambda(x) (drop-right x 1)) ejemplos-evaluar) 1))
        (entrenar (algoritmo ejemplos-aprender))
        (interpretar (map (lambda(x) (interprete entrenar x)) ejemplosEvaluacionSinClase)))
      (precision ejemplosEvaluacion interpretar)
   ))

;;Ejercicio 24
(he-tardado 120 'b1-e24)
;; Problemas porque habia construido los algoritmos de tal manera que tenia
;; que tenia que pasar los ejemplso con los metadatos
;; por lo que iba adjuntando los atributos manualmente y eso daba problemas luego
;; cuando mas adelante se empiezan a usar otros conjuntos como ionosphere

;> (cross-validation A1 A1i ejemplosJuntos 3)
;0.6388888888888888
;> (cross-validation A0 A0i ejemplosJuntos 25)
;0.56
;> (leave-one-out A0 A0i ejemplosJuntos)
;0.56
(define (cross-validation algoritmo interprete ejemplos n)
  (let* ((atributos (car ejemplos))
         (ejemplosEnFolds (folds n ejemplos)))
    (define evaluarFold
      (lambda (foldParaEvaluar)
        (define foldsDeEntrenamiento (remq foldParaEvaluar ejemplosEnFolds))
        (define ejemplosEntrenamiento (append (list atributos) (append* foldsDeEntrenamiento)))
        ;(define ejemplosEvaluacion (append (list atributos) foldParaEvaluar))
        (holdout algoritmo interprete ejemplosEntrenamiento foldParaEvaluar)))
    (define sumaPrecisiones (apply + (map (lambda (fold)
         (evaluarFold fold)) ejemplosEnFolds)))
    (exact->inexact (/ sumaPrecisiones n))
    ))


;> (stratified-cross-validation A0 A0i ejemplosJuntos 3)
;0.5555555555555555
;> (stratified-cross-validation A1 A1i ejemplosJuntos 3)
;0.4351851851851852
(define (stratified-cross-validation algoritmo interprete ejemplos n)
 (let* ((atributos (car ejemplos))
        (ejemplosEnFolds (stratify n ejemplos)))
    (define evaluarFold
      (lambda (foldParaEvaluar)
        (define foldsDeEntrenamiento (remq foldParaEvaluar ejemplosEnFolds))
        (define ejemplosEntrenamiento (append (list atributos) (append* foldsDeEntrenamiento)))
        ;(define ejemplosEvaluacion (append (list atributos) foldParaEvaluar))
        (holdout algoritmo interprete ejemplosEntrenamiento foldParaEvaluar)))
    (define sumaPrecisiones (apply + (map (lambda (fold)
         (evaluarFold fold)) ejemplosEnFolds)))
    (exact->inexact (/ sumaPrecisiones n))
   ))
