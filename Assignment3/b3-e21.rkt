;;Plantilla b3-e21.scm
;;Autor: Patricia Mayo Tejedor

;;Ejercicio 21
(he-tardado 300 'b3-e21)
; Los interpretes Cli no funcionan con con conjuntos de datos que tienen mas de dos clases distintas
; ====================== LMS =======================
;> (stratified-cross-validation LMS CLi ejemplos 10)
;0.2
;> (stratified-cross-validation LMS CLi ionosphere 10)
;0.4696825396825397
;> (stratified-cross-validation LMS CLi agaricus-lepiota 10)
;0.5003674888965638
;> (stratified-cross-validation LMS CLi poker 10)
;0.0
;> (stratified-cross-validation LMS CLi lymphography 10)
;0.0
; ====================== HGS =======================
;> (stratified-cross-validation HGS CLi ejemplos 10)
;0.4
;> (stratified-cross-validation HGS CLi ionosphere 10)
;0.45285714285714285
;> (stratified-cross-validation HGS CLi agaricus-lepiota 10)
;0.49372133253352235
;> (stratified-cross-validation HGS CLi poker 10)
;0.0
;> (stratified-cross-validation HGS CLi lymphography 10)
;0.0
; ====================== PCP =======================
;> (stratified-cross-validation PCP LUUi ejemplos 10)
;0.35
;> (stratified-cross-validation PCP LUUi ionosphere 10)
;0.5643650793650794
;> (stratified-cross-validation PCP LUUi agaricus-lepiota 10)
;0.5140298656681148
; ====================== IB =======================
;> (stratified-cross-validation IB IBi ejemplos 10)
;0.3
;> (stratified-cross-validation IB IBi ionosphere 10)
;0.5752380952380952
;> (stratified-cross-validation IB IBi agaricus-lepiota 10)
;0.5204389568526228
;> (stratified-cross-validation IB IBi lymphography 10)
;0.46047619047619054
;> (stratified-cross-validation IB IBi poker 10)
; Sin acabar
; ====================== NB =======================
;> (stratified-cross-validation NB NBi ejemplos 10)
;0.3
;> (stratified-cross-validation NB NBi ionosphere 10)
;0.0
;> (stratified-cross-validation NB NBi agaricus-lepiota 10)
;0.5171139851792608
