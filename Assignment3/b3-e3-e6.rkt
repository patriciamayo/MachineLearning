;;Plantilla b3-e3-e6.scm
;;Autor: <Nombre y apellidos>

;;Ejercicio 3
(he-tardado 90 'b3-e3)
;; Creo que he conseguido arreglar el HGS mientras hacia este ejercicio, tenia que devolver un concepto y estaba devolviendo una lista
;; El NSC realmente depende del algoritmo que le pongamos, por lo que no es independiente para que funcione bien o mal
;;> (NSC0 HGS PSET NSET '())
;;'(((*) (*) (subiendo) (*) (*) (*))
;;  ((bueno) (*) (estable) (*) (*) (*))
;;  ((variable) (*) (*) (*) (*) (*))
;;  ((frio) (*) (bajando) (*) (*) (*)))

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


(define (NSC0 algoritmo PSET NSET DNF)
(let* ((metadatos (car ejemplos))
       (nuevosEjemplos (append (list metadatos) PSET NSET)))
  (if (empty? PSET)
      DNF
      (let* ((D (algoritmo nuevosEjemplos))
             (nuevoDNF (append DNF (list D)))
             (instanciasCubiertas (filter
                                   (lambda (P)
                                     ((eval (funcion-match algoritmo)) D (drop-right P 1)))
                                   PSET)))
        (if (empty? instanciasCubiertas)
            DNF
            (NSC0 algoritmo (remq* instanciasCubiertas PSET) NSET nuevoDNF)))
  )))

(define (NSC algoritmo ejemplos)
 (let*((clasesSeparadas (separarClases ejemplos))
       (PSET (car (clasesSeparadas ejemplos)))
       (NSET (list-ref (clasesSeparadas ejemplos) 1)))
  (NSC0 algoritmo PSET NSET '())))

;;Ejercicio 4
(he-tardado 90 'b3-e4)
;; El NSC realmente depende del algoritmo que le pongamos, por lo que no es independiente para que funcione bien o mal
;; El que mejor funciona es el NSC con el HGS, y el PCP.
;; Con LMS hay un problema, y esque para conjuntos muy grandes, al final los calculos tienden a infinito.
;; El HGS es el que devuelve listas de mayor tamano, el problema reside en que es muy exclusivo,
;; si no hay un concepto general que haga match para un positivo pero excluyendo todos los negativos entonces pierde toda su utilidad (ejemplo de ionosphere)
;; El mas preciso entonces es PCP, ya que cuenta con los recursos para ponderar atributos

; ===================== HGS ====================
;> (NSC HGS ejemplos)
;'(((*) (*) (subiendo) (*) (*) (*)) ((bueno) (*) (estable) (*) (*) (*)) ((variable) (*) (*) (*) (*) (*)) ((frio) (*) (bajando) (*) (*) (*)))
;> (NSC HGS ionosphere)
;'()
;> (NSC HGS agaricus-lepiota)
;'(((*) (*) (*) (*) (n) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*))
;  ((*) (*) (*) (t) (*) (*) (*) (*) (*) (*) (c) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*))
;  ((*) (*) (*) (t) (*) (*) (*) (*) (*) (*) (*) (*) (y) (*) (*) (*) (*) (*) (*) (*) (*) (*))
;  ((*) (*) (*) (*) (a) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*))
;  ((*) (*) (*) (*) (l) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*)))


; ===================== PCP ====================

;> (NSC PCP ejemplos)
; '(<metadatos> (0.6943384349398303 0.3771808497732545 0.937608751706458 0.6708869001698856 0.011172280256597888 -0.9686072444241277 -0.895993248179228)))
;> (NSC PCP ionosphere)
; '((<metadatos> (-0.675381044689393 0.37196932019889806 -0.32751786547122236 0.400971309160757 90.05464187290649264 0.01562892736168986 -0.24231832317528945 0.14379877664500518 -0.7067045586203009 -0.032130646353722925 -0.997429599131535 1.1354574230337906 -0.7304497999263311 -0.023076394085932955 0.12383527012056322 0.010743102281213945 0.15643815621170432 0.7992197845591427 0.2331029321033214 0.015698432762077552 -0.13252088000723505 0.43384912195314723 -0.20590183980867216 0.2047146130088493 -0.2416306322686153 0.3570144867606177 -0.9445522912567845 -0.17937718685294474 -0.5776618859008869 -0.41161221001025566 -0.8111433075852357 0.15110801601192436 0.9342664806535106 -0.5098765282211859 -2.5847266109714133))
;   (<metadatos> (-1.0649908942911088 0.8780897196015953 0.3493194331055904 -0.0075351555228048706 0.39929966414645185 -0.17970486659385732 -0.3180626872397472 -0.20508005909372395 -0.7645269669966419 -0.1918842275111114 -0.5961347506542867 1.1894549400460879 -0.4774945927442077 0.23660134057597615 0.29517941915971974 0.4788085162448239 -0.516318642613211 0.2272184125415882 0.026251091253564424 -0.08864521099662684 0.21268060040863562 0.4285799105422456 0.15694157345657234 -0.32726053825290885 -0.23533499930411927 -0.19747260675835554 -1.2160999877063179 -0.26289043450813604 -0.6537538205116826 -0.09514894867219892 -0.3829595366030483 -0.359583992891132 4-0.7866179376059145 0.20977676926042155 -2.5693985533050494)))
; > (NSC PCP agaricus-lepiota)
;'()

; ===================== LMS ====================
;> (NSC LMS ejemplos)
;'((<metadatos> (-0.40025929088097356 0.03442986057172792 -0.39675039623959046 0.20521652644617427 0.18885079614840583 -0.2759503161994893 0.39141963245703004))))
; > (NSC LMS ionosphere)
; '((<metadatos> (0.39477424233058533-0.3153057160283413-0.29066564269793540.43539867819354994-0.06582383408475606-0.327906400478568670.006014185783209003-0.2849644646683262-0.0958208963113712-0.352518906659431-0.12980571039942734-0.428166362237791360.47744931008421276-0.16043553347014610.43633977388000890.03300673441621493-0.355801626575816930.20148988345402663-0.21035441657382030.091723997629851060.3436834757416889-0.10088106779923240.345170575844934270.18038777926020755-0.022313061785212862-0.347952392737869540.4918794995897767-0.38484654367158210.0140256634720923310.089275260122784970.15321736220950533-0.40709216931722386-0.076046560615693310.3856549326368204-0.4715527799173673))
;   (<metadatos>  (-0.43047921604935010.040531882185170340.25630043710360560.144739904232765610.268455131873178270.47953832236676740.2260669660805560.391953269142257140.16365176300508133-0.21264338009753797-0.0141050820084887-0.25258400303718460.47400909606225150.047389355687654120.10951356235398468-0.18263907311226407-0.08560336260253082-0.43291159440893950.40034497256198776-0.243467108728633860.32274419258599930.0010373555626185250.2478231490932441-0.389394653261193930.034491316455927180.153458799682425040.38944270368760514-0.41961142962779320.293238652170086270.30223642309782470.44316115211172025-0.12305880770930833-0.089556799882048340.20485394881330932-0.348538201650592))
;   (<metadatos> (-0.42405898943642850.11534727411163814-0.043003671789719644-0.33778326289237450.33054726681528424-0.16203067770748886-0.29658292506105460.26981505894137836-0.340538970854158-0.48572242563363740.34376417810631665-0.4246558091436532-0.37661825454248976-0.193430455921575150.39389071076392856-0.2678240294818296-0.3165108388835225-0.483437626519013730.24562390802655676-0.44579589290673510.2502186568559801-0.2905222739625333-0.193392459821335850.287621763261344030.41899429404894206-0.20723992006525005-0.15511924337241856-0.28980085190352450.03516378167878531-0.164419164228994890.163899132770258030.32466307644031944-0.406885343285312730.2350376280694797-0.10230852670040291))
;   (<metadatos> (-0.20426481926978618-0.40368080534171485-0.0257484550484639030.2989971146898810.2920119703604118-0.0593926616370849-0.32908228213179730.141133590917053460.45584879997571714-0.16146295182041215-0.43313965995168530.08271455024476781-0.25473549100220716-0.200592681468296230.2760898257667860.104460594879417720.19429222620389963-0.205852214902932870.12923815145193030.33199144295747873-0.4135425125753606-0.07350530412260048-0.3484912201497177-0.37490655225249070.040821422471398530.289522165251106630.446155305905338360.2600360210257332-0.0682082749408020.4178671103241759-0.149476111189236650.25736271253122120.328255404317082-0.02210960178608007-0.006421287622216054))
;   (<metadatos> (-0.217416503984162730.45631101236508487-0.27277543948434557-0.17847108541102746-0.43018326290839320.228703923423405840.49598675737279607-0.0505187200633561150.08687094134957440.299123637196076260.21408232732888410.476072236202430330.113263298654641580.187648051192703450.17214239593726088-0.11070539337273727-0.141063209004036020.19816490686924682-0.4069993034135213-0.188687624467309970.4765984218410384-0.0762850525479975-0.458187512425473560.06605921539951043-0.47778594223304560.2459310815096053-0.48507971779829384-0.0264523824448919530.40947104552061710.3948642458142162-0.020741909582707330.15646769445046804-0.2637751488632595-0.299987429379808050.02726548110861804))
;   (<metadatos> (0.3589181258936810.098738273265203680.002912493098014668-0.18490163433820472-0.4174805478742239-0.0272625548929467660.18435210835776260.2915635664586961-0.3459021681797809-0.4361731437323647-0.061840594015746210.29633457694146603-0.101650343775579580.11686661124887299-0.25424664115610096-0.48239647185859880.243827738034578470.27132011820436110.12159468077395430.083097327333931940.2726926933322289-0.149064396974955330.116712136025569510.05153669107696790.474579406835259230.46022689801808336-0.20057953165856712-0.31518489275091740.4505224467042529-0.2937000761948563-0.097197284506874870.30134919581018227-0.0074139261949100480.178177498295186030.2909606668445792)))
;> (NSC LMS agaricus-lepiota)
; crash

;;Ejercicio 5
(he-tardado 90 'b3-e5)
;; Creo que hay problemas ocn las funciones match, ya que estan definidas de tal manera que si es '+ devuelve true, y sino devuelve false
;; En todos los ejemplos hemos jugado siempre con dos clases asique no se muy bien que esperar
;; Entiendo como el MSC aplica el divide y venceras para soportar varias clases pero me da miedo que yo haya hardcoded las clases + y - en alguna parte
(define casosDeClase
  (lambda (ejemplos clase)
    (filter
     (lambda (ejemplo)
       (equal? clase (last ejemplo)))
     (list-tail ejemplos 1))))

(define (MSC0 algoritmo ejemplos)
(let* ((casos (list-tail ejemplos 1))
       (CSET (atributo 'clase ejemplos))
       (RULES-PER-CLASS (map
               (lambda (CLASS)
                 (define PSET (casosDeClase ejemplos CLASS))
                 (define NSET (remq* PSET casos))
                 (define DNF (NSC0 algoritmo PSET NSET '()))
                 (map
                  (lambda (D)
                    (list D '=> CLASS))
                  DNF))
               CSET)))
  (append* RULES-PER-CLASS)
  ))

(define (MSC algoritmo ejemplos)
(append
 (MSC0 algoritmo ejemplos)
 (list `(match-CL ,(make-list (- (length (car ejemplos)) 1) '(*)) => ,(A0 ejemplos)))))

;;Ejercicio 6
(he-tardado 500 'b3-e6)
;; He tenido que arreglar muchos algoritmos para que funcionase. Por ejemplo con el algoritmo PCP es necesario saber si la clase es positiva o negativa,
;; por lo que se compara con '+ y '-, lo que ha ocasionado problemas al intentar usar los casos de poker y lymphography (tienen mas de dos clases)
;; Errores en el formato de LMS tambien han sido arreglados
;; Por lo general los resultados son muy malos, solo con HGS y LMS ionosphere se alcanza el 60%
;; Los conceptos generados por HGS no garantizan que se cubran todos los positivos y ninguno de los negativos, por lo que es indicado para datasets
;; donde pudiese haber ruido por lo que que el HGS y su busqueda heuristica es ideal para este dataset



;========================= SUBALGORITMOS ====================
(define (MSC-HGS ejemplos)
(let* ()
(append
 (MSC0 HGS ejemplos)
 (list `(match-CL ,(make-list (- (length (car ejemplos)) 1) '(*)) => ,(A0 ejemplos))))))

(define (MSC-LMS ejemplos)
(let* ()
(append
 (MSC0 LMS ejemplos)
 (list `(match-CL ,(make-list (- (length (car ejemplos)) 1) '(*)) => ,(A0 ejemplos))))))

(define (MSC-PCP ejemplos)
 (let* ()
(append
 (MSC0 PCP ejemplos)
 (list `(match-CL ,(make-list (- (length (car ejemplos)) 1) '(*)) => ,(A0 ejemplos))))))


;========================= HGS ====================

> (stratified-cross-validation MSC-HGS LDi ejemplos 10)
0.25
> (stratified-cross-validation MSC-HGS LDi ionosphere 10)
0.618015873015873

> (stratified-cross-validation MSC-HGS LDi agaricus-lepiota 10)
Sin resultados

> (stratified-cross-validation MSC-HGS LDi poker 10)
0.4994002399040385

> (stratified-cross-validation MSC-HGS LDi lymphography 10)
0.47857142857142854


;========================= PCP ====================

> (stratified-cross-validation MSC-PCP LDi ejemplos 10)
0.35

> (stratified-cross-validation MSC-PCP LDi ionosphere 10)
0.3642857142857142

> (stratified-cross-validation MSC-PCP LDi agaricus-lepiota 10)
0.4896598682735596

> (stratified-cross-validation MSC-PCP LDi lymphography 10)
0.5166666666666666

> (stratified-cross-validation MSC-PCP LDi poker 10)
0.39316273490603754

;========================= LMS ====================

> (stratified-cross-validation MSC-LMS LDi ejemplos 10)
0.4

> (stratified-cross-validation MSC-LMS LDi ionosphere 10)
0.5958730158730159

> (stratified-cross-validation MSC-LMS LDi agaricus-lepiota 10)
0.5174802622410459

> (stratified-cross-validation MSC-LMS LDi poker 10)
Sin resultados

> (stratified-cross-validation MSC-LMS LDi lymphography 10)
0.3666666666666666







