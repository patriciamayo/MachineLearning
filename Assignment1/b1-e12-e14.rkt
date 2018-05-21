;;Plantilla b1-e12-e14.scm
;;AUTOR:
;;Copyright (C) 2009  PAtricia Mayo Tejedor

;;This program is free software: you can redistribute it and/or modify
;;it under the terms of the GNU General Public License as published by
;;the Free Software Foundation, either version 3 of the License, or
;;(at your option) any later version.

;;This program is distributed in the hope that it will be useful,
;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;GNU General Public License for more details.

;;You should have received a copy of the GNU General Public License
;;along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;Ejercicio 12
;; Se ha optado por crear la primera sublista quitando elementos de la lista original.
;; Esto permite asegurarnos de que no haya duplicados (como podría pasarnos si simplemente vamos creamos la lista según obtenemos valores de obtener-al-azar).
;; Después simplemente usamos la función substract que nos devuelve los elementos restantes de la lista original.

;> (separar 0.67 ejemplos)
(he-tardado 180 'b1-e12)
(define (separar proporcion ejemplos)
(let* ((casos (list-tail ejemplos 1))
       (tamanoSublista1 (exact-round (* proporcion (length casos)))))
    (define crearSublista2
      (lambda (contador casos)
        (if (= contador 0)
            casos
            (crearSublista2 (- contador 1)
                           (remq (obtener-al-azar casos) casos))
            )
        ))
    (define sublista2 (crearSublista2 tamanoSublista1 casos))
    (define sublista1 (set-subtract casos sublista2))
    (list sublista1 sublista2)
  ))

;;Ejercicio 13
;; Se ha modificado la función crearSublista del apartado anterior, ya que era ineficiente y
;; necesitaba llevar la cuenta de cuántos elementos quedaban disponibles, en vez del tamaño de la sublista en sí.
;; Usando los procedures de quotient y modulo podemos sacar los tamaños de cada sublista y
;; cuántas listas tendrán que llevar un elemento extra cuando la división no es exacta.
;> (folds 4 ejemplos)
(he-tardado 300 'b1-e13)
(define (folds n ejemplos)
(let* ((casos (list-tail ejemplos 1))
       (tamanoSublistas (quotient (length casos) n))
       (sublistasCon1Mas (modulo (length casos) n)))
  
    (define crearSublista
      (lambda (contador casos sublista)
        (if (= contador 0)
            sublista
            (let ((quitar (obtener-al-azar casos)))
                (crearSublista (- contador 1)
                           (remq quitar casos)
                           (append sublista (list quitar)))))
        ))
    ; define cuantos extra elementos tendra la sublista
    (define elementosExtras
                  (lambda (sublistasCon1Mas)
                    (if (> sublistasCon1Mas 0) 1
                        0)))
    (define crearFolds
      (lambda (numeroFolds tamanoSublistas sublistasCon1Mas foldsHechos casos)
        (if (= 0 numeroFolds)
            foldsHechos
            (let ((tamanoSublistas tamanoSublistas)
                  (elementosExtras (elementosExtras sublistasCon1Mas)))
              (define sublista (crearSublista (+ tamanoSublistas elementosExtras) casos '()))
              (crearFolds (- numeroFolds 1)
                          tamanoSublistas
                          (- sublistasCon1Mas elementosExtras)
                          (append foldsHechos (list sublista))
                          (set-subtract casos sublista))))
        ))
    (crearFolds n tamanoSublistas sublistasCon1Mas '() casos)
  ))

;;Ejercicio 14
;; Este ejercicio me ha costado muchisimo, ademas como se escogen al azar la clase y el ejemplos
;; hay veces que no salen exactamente perfecto, pero normalmente salen los folds proporcionados
;> (stratify 4 ejemplos)
(he-tardado 500 'b1-e14)
(define (stratify n ejemplos)
(let* ((casos (list-tail ejemplos 1))
       (tamanoSublistas (quotient (length casos) n))
       (sublistasCon1Mas (modulo (length casos) n))
       (tiposDeClases (atributo 'clase ejemplos)))
  
    ; PREPARACION
    ; Devuelve una lista con tantos elementos como clases hay. Cada elemento contiene todos los casos de esa clase
    (define separarClases
           (lambda (index tiposDeClases casos agrupacion)
             (if (= index (length tiposDeClases))
                 agrupacion
                 (let ((claseAgrupada  (filter (lambda (caso)
                                                 (eqv? (list-ref caso (- (length caso) 1)) (list-ref tiposDeClases index)))
                                               casos)))
                   (separarClases (+ index 1) tiposDeClases casos (append agrupacion (list claseAgrupada)))))))

    ; Devuelve una lista con tantos elementos como clases hay. Cada elemento es un par, con la clase y la frecuencia en la que aparece en la lista 
    (define paresClaseFrecuencia
      (lambda (tiposDeClases clasesAgrupadas)
        (map (lambda (tipo grupo)(cons tipo (length grupo))) tiposDeClases clasesAgrupadas)))

    ; MANIPULACION DE CASOS
    ; Elimina el caso dado de las clases agrupadas
    (define quitarCaso
      (lambda (casoParaEliminar clasesAgrupadas)
        (map (lambda (casosDeClase)
               (remq casoParaEliminar casosDeClase))
             clasesAgrupadas)))

    ; Encuentra todos los casos de una clase. Si no quedan mas casos de esa clase, escoge de otra al azar
    (define encontrarCasosDeClase
      (lambda (clase clasesAgrupadas clasesConFrecuencia)
        (define casosDeClaseEncontrados
          (filter
           (lambda (casosDeClase)
             (if (= (length casosDeClase) 0)
                 #f
                 (let* ([primerCaso (list-ref casosDeClase 0)]
                        [claseDelPrimerCaso (list-ref primerCaso (- (length primerCaso) 1))])
                        (eq? clase claseDelPrimerCaso))))
          clasesAgrupadas))
        (if (= (length casosDeClaseEncontrados) 0)
          (encontrarCasosDeClase (obtener-al-azar0 clasesConFrecuencia) clasesAgrupadas clasesConFrecuencia)
          (list-ref casosDeClaseEncontrados 0 ))))
    
    ; Devuelve un elemento al azar de la clase dada
    (define obtenerAlAzarDeClase
      (lambda (clase clasesAgrupadas)
        (define encontrarCasosDeClase
          (filter (lambda (casosDeClase)
                    (define primerCaso (list-ref casosDeClase 0))
                    (define claseDelPrimerCaso (list-ref primerCaso (- (length primerCaso) 1)))
                    (eq? clase claseDelPrimerCaso))
                  clasesAgrupadas))
        (obtener-al-azar (list-ref encontrarCasosDeClase 0 ))))
    
    ; CREACION DE FOLDS
    ; Define cuantos extra elementos tendra una sublista (cuando no se puedan dividir exactos)
    (define elementosExtras
                  (lambda (sublistasCon1Mas)
                    (if (> sublistasCon1Mas 0) 1
                        0)))
    
    ; Dado el tamanio deseado, se crea una sublista con proporcion ademas de devolver los casos sobrantes
    (define crearSublista
      (lambda (numeroElementos casosAgrupadosPorClase paresClaseFrecuencia sublista)
        (if (= numeroElementos 0)
            (list sublista casosAgrupadosPorClase)
            (let* ([claseElegida (obtener-al-azar0 paresClaseFrecuencia)]
                   [casoElegido (obtener-al-azar (encontrarCasosDeClase claseElegida casosAgrupadosPorClase paresClaseFrecuencia))])
                (crearSublista (- numeroElementos 1)
                           (quitarCaso casoElegido casosAgrupadosPorClase) 
                           paresClaseFrecuencia 
                           (append sublista (list casoElegido)))))
        ))

    ; Se crean los folds de manera recursiva
    (define crearFolds
      (lambda (numeroFolds tamanoSublistas sublistasCon1Mas foldsHechos casosAgrupadosPorClase paresClaseFrecuencia)
        (if (= 0 numeroFolds)
            foldsHechos
            (let ((tamanoSublistas tamanoSublistas)
                  (elementosExtras (elementosExtras sublistasCon1Mas)))
              (define separarSublistaDeCasos (crearSublista (+ tamanoSublistas elementosExtras) casosAgrupadosPorClase paresClaseFrecuencia '()))
              (define sublista (list-ref separarSublistaDeCasos 0))
              (define casosSobrantes (list-ref separarSublistaDeCasos 1))
              (crearFolds (- numeroFolds 1)
                          tamanoSublistas
                          (- sublistasCon1Mas elementosExtras)
                          (append foldsHechos (list sublista))
                          casosSobrantes
                          paresClaseFrecuencia)))
        ))

    (define casosAgrupadosPorClase (separarClases 0 tiposDeClases casos '()))
    (define clasesConFrecuencia (paresClaseFrecuencia tiposDeClases casosAgrupadosPorClase))
    (crearFolds n tamanoSublistas sublistasCon1Mas '() casosAgrupadosPorClase clasesConFrecuencia)
  ))
