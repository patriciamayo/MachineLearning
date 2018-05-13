; Citation Request:
;    This lymphography domain was obtained from the University Medical Centre,
;    Institute of Oncology, Ljubljana, Yugoslavia.  Thanks go to M. Zwitter and 
;    M. Soklic for providing the data.  Please include this citation if you plan
;    to use this database.

; 1. Title: Lymphography Domain

; 2. Sources: 
;     (a) See Above.
;     (b) Donors: Igor Kononenko, 
;                 University E.Kardelj
;                 Faculty for electrical engineering
;                 Trzaska 25
;                 61000 Ljubljana (tel.: (38)(+61) 265-161

;                 Bojan Cestnik
;                 Jozef Stefan Institute
;                 Jamova 39
;                 61000 Ljubljana
;                 Yugoslavia (tel.: (38)(+61) 214-399 ext.287) 
;    (c) Date: November 1988

; 3. Past Usage: (sveral)
;     1. Cestnik,G., Konenenko,I, & Bratko,I. (1987). Assistant-86: A
;        Knowledge-Elicitation Tool for Sophisticated Users.  In I.Bratko
;        & N.Lavrac (Eds.) Progress in Machine Learning, 31-45, Sigma Press.
;        -- Assistant-86: 76% accuracy
;     2. Clark,P. & Niblett,T. (1987). Induction in Noisy Domains.  In
;        I.Bratko & N.Lavrac (Eds.) Progress in Machine Learning, 11-30,
;        Sigma Press.
;        -- Simple Bayes: 83% accuracy
;        -- CN2 (99% threshold): 82%
;     3. Michalski,R., Mozetic,I. Hong,J., & Lavrac,N. (1986).  The Multi-Purpose
;        Incremental Learning System AQ15 and its Testing Applications to Three
;        Medical Domains.  In Proceedings of the Fifth National Conference on
;        Artificial Intelligence, 1041-1045. Philadelphia, PA: Morgan Kaufmann.
;        -- Experts: 85% accuracy (estimate)
;        -- AQ15: 80-82%

; 4. Relevant Information:
;      This is one of three domains provided by the Oncology Institute
;      that has repeatedly appeared in the machine learning literature.
;      (See also breast-cancer and primary-tumor.)

; 5. Number of Instances: 148

; 6. Number of Attributes: 19 including the class attribute

; 7. Attribute information:
;     --- NOTE: All attribute values in the database have been entered as
;               numeric values corresponding to their index in the list
;               of attribute values for that attribute domain as given below.
;
;     1. lymphatics: normal, arched, deformed, displaced
;     2. block of affere: no, yes
;     3. bl. of lymph. c: no, yes
;     4. bl. of lymph. s: no, yes
;     5. by pass: no, yes
;     6. extravasates: no, yes
;     7. regeneration of: no, yes
;     8. early uptake in: no, yes
;     9. lym.nodes dimin: 0-3
;    10. lym.nodes enlar: 1-4
;    11. changes in lym.: bean, oval, round
;    12. defect in node: no, lacunar, lac. marginal, lac. central
;    13. changes in node: no, lacunar, lac. margin, lac. central
;    14. changes in stru: no, grainy, drop-like, coarse, diluted, reticular, 
;                         stripped, faint, 
;    15. special forms: no, chalices, vesicles
;    16. dislocation of: no, yes
;    17. exclusion of no: no, yes
;    18. no. of nodes in: 0-9, 10-19, 20-29, 30-39, 40-49, 50-59, 60-69, >=70
;    19. class: normal find, metastases, malign lymph, fibrosis

; 8. Missing Attribute Values: None

; 9. Class Distribution: 
;     Class:        Number of Instances:
;     normal find:  2
;     metastases:   81
;     malign lymph: 61
;     fibrosis:     4

((
 (lymphatics (normal arched deformed displaced));normal, arched, deformed, displaced
 (block-of-affere (no yes)); no, yes
 (bl-of-lymph-c (no yes)); no, yes
 (bl-of-lymph-s (no yes)); no, yes
 (by-pass (no yes));no, yes
 (extravasates (no yes));no, yes
 (regeneration-of (no yes)); no, yes
 (early-uptake-in (no yes)); no, yes
 (lym-nodes-dimin numerico); 0-3
 (lym-nodes-enlar numerico); 1-4
 (changes-in-lym (bean oval round)); bean, oval, round
 (defect-in-node (no lacunar lac-marginal lac-central)); no, lacunar, lac. marginal, lac. central
 (changes-in-node (no lacunar lac-marginal lac-central)); no, lacunar, lac. margin, lac. central
 (changes-in-stru (no grainy drop-like coarse diluted reticular stripped faint)); no, grainy, drop-like, coarse, diluted, reticular, stripped, faint, 
 (special-forms (no chalices vesicles)); no, chalices, vesicles
 (dislocation-of (no yes)); no, yes
 (exclusion-of-no (no yes)); no, yes
 (no-of-nodes-in (1 2 3 4 5 6 7 8)); 0-9, 10-19, 20-29, 30-39, 40-49, 50-59, 60-69, >=70
 (clase (normal-find metastases malign-lymph fibrosis)); normal find, metastases, malign lymph, fibrosis
 )

 (displaced yes no no no no no yes 1 2 oval lacunar lac-central faint no no yes 2 malign-lymph)
 (deformed yes no no yes yes no yes 1 3 round lacunar lac-marginal coarse chalices yes yes 2 metastases)
 (deformed yes yes yes yes yes yes yes 1 4 round lac-marginal lac-central faint vesicles yes yes 7 malign-lymph)
 (deformed no no no no yes no yes 1 3 round lac-central lac-central coarse vesicles no yes 6 malign-lymph)
 (deformed no no no no no no no 1 2 oval lac-central lac-marginal diluted no yes yes 1 metastases)
 (arched no no no no no no yes 1 3 round lac-marginal lac-marginal reticular vesicles no yes 4 metastases)
 (arched yes no no no no no yes 1 2 round lacunar lac-marginal faint chalices no no 1 metastases)
 (deformed yes no no no yes no yes 1 2 oval lacunar lacunar no vesicles no no 1 metastases)
 (arched yes no no no no no yes 1 3 oval lacunar lacunar faint vesicles no yes 5 malign-lymph)
 (arched no no no no no no yes 1 2 oval lac-marginal lac-marginal diluted vesicles no no 2 malign-lymph)
 (arched yes yes no yes yes no yes 1 3 oval lac-central lac-marginal diluted no yes yes 3 metastases)
 (displaced yes no no no yes no no 1 3 round lac-central lac-marginal faint vesicles yes yes 2 metastases)
 (displaced yes no no no no no yes 1 2 oval lacunar lacunar drop-like chalices no no 1 metastases)
 (displaced no no no no yes no yes 1 4 oval lacunar lac-central stripped vesicles yes yes 2 malign-lymph)
 (deformed yes yes yes yes yes yes yes 3 1 bean lacunar lacunar faint no yes yes 4 fibrosis)
 (arched yes no no no yes no yes 1 2 round lac-marginal lac-marginal diluted vesicles yes yes 2 metastases)
 (deformed yes no no no yes no yes 1 2 round lac-marginal lac-marginal grainy chalices yes yes 3 metastases)
 (arched yes no no no yes no no 1 2 oval lacunar lac-marginal diluted chalices no yes 2 metastases)
 (deformed no no no no no no yes 1 2 oval lacunar lac-marginal grainy no yes yes 1 metastases)
 (arched yes no no no no no yes 1 3 round lac-marginal lac-marginal grainy chalices yes no 1 metastases)
 (deformed no no no no no no yes 1 2 oval lac-central lacunar coarse vesicles yes yes 3 malign-lymph)
 (displaced no no no yes yes no yes 1 3 round lac-marginal lac-central diluted vesicles yes yes 4 malign-lymph)
 (displaced yes no no yes yes no yes 1 4 round lac-central lac-marginal coarse chalices yes yes 2 metastases)
 (deformed yes no no no no no yes 1 2 oval lacunar lacunar diluted no no no 1 metastases)
 (deformed no no no no no no yes 2 1 oval lacunar lacunar faint vesicles no yes 8 malign-lymph)
 (deformed no no no no yes no no 1 3 oval lac-marginal lac-marginal faint vesicles yes yes 2 malign-lymph)
 (displaced no no no no yes no yes 1 3 round lac-central lacunar coarse vesicles yes yes 4 malign-lymph)
 (displaced no no no no no no yes 1 3 oval lac-central lac-central faint vesicles yes yes 1 malign-lymph)
 (displaced yes no no no no no yes 1 2 oval lacunar lac-marginal drop-like chalices yes yes 1 metastases)
 (deformed no no no no no no no 1 2 oval lac-central lacunar faint chalices yes yes 1 malign-lymph)
 (displaced yes no no no yes no yes 1 4 round lac-marginal lac-marginal stripped vesicles yes yes 3 malign-lymph)
 (arched no no no no no no yes 1 3 oval lac-marginal lac-marginal faint vesicles yes yes 3 malign-lymph)
 (arched no no no no no no no 1 2 round lacunar lacunar faint no yes no 1 metastases)
 (arched yes no no no no no no 1 1 oval lacunar lac-marginal drop-like no no no 1 metastases)
 (displaced yes no no yes yes no yes 1 3 round lac-central lac-marginal coarse vesicles yes yes 2 metastases)
 (arched yes yes no yes yes no yes 1 3 round lac-marginal lac-marginal faint vesicles no yes 2 metastases)
 (deformed no no no yes yes yes no 3 1 bean lac-central lacunar diluted vesicles no yes 4 fibrosis)
 (deformed no no no no yes no yes 1 4 round lac-central lacunar coarse vesicles yes yes 6 malign-lymph)
 (arched no no no no no no yes 1 3 round lacunar lac-marginal coarse vesicles yes yes 2 malign-lymph)
 (arched no no no no no no yes 1 2 oval lacunar no stripped no yes yes 2 malign-lymph)
 (arched no no no no yes no yes 1 2 oval lac-marginal lac-marginal drop-like vesicles no yes 1 malign-lymph)
 (arched yes yes no yes yes yes yes 1 4 oval lacunar lacunar coarse vesicles yes yes 6 malign-lymph)
 (deformed yes yes no no yes no no 1 3 oval lac-marginal lac-marginal coarse vesicles no yes 2 metastases)
 (displaced yes no no yes no no yes 1 3 round lac-marginal lac-marginal grainy chalices yes yes 3 metastases)
 (deformed yes yes yes yes yes yes no 2 2 oval lac-central lacunar coarse vesicles yes yes 7 fibrosis)
 (arched no no no no no no yes 1 2 oval lacunar lacunar faint chalices yes yes 1 malign-lymph)
 (deformed yes yes no yes no no yes 1 2 oval lac-marginal lac-marginal faint vesicles no yes 1 metastases)
 (displaced no no no no no no no 2 1 round lac-central lacunar faint no yes yes 1 metastases)
 (deformed yes yes no no yes no no 1 2 round lac-marginal lac-marginal diluted chalices no yes 2 metastases)
 (arched yes yes no no no no yes 1 2 oval lac-central lac-marginal faint chalices yes yes 2 metastases)
 (arched no no no no no no no 1 1 bean lacunar lacunar drop-like no yes yes 1 metastases)
 (deformed yes no no no yes no yes 1 2 oval lacunar lac-central faint vesicles no yes 3 malign-lymph)
 (deformed no no no no no yes no 1 2 oval lac-central lac-marginal diluted chalices no yes 1 metastases)
 (arched yes no no yes yes no no 1 3 round lac-central lac-marginal coarse vesicles no no 1 metastases)
 (arched yes yes no yes yes no no 1 2 oval lac-marginal lac-marginal coarse chalices no yes 1 metastases)
 (displaced no no no no no no yes 1 2 oval lac-marginal lac-marginal drop-like vesicles yes no 1 malign-lymph)
 (arched no no no no no no yes 1 3 oval lac-central lacunar faint vesicles yes yes 4 malign-lymph)
 (displaced yes no no yes yes no yes 1 2 round lacunar lac-marginal grainy vesicles yes yes 4 metastases)
 (displaced yes yes yes yes yes yes yes 1 4 round lac-central lac-central stripped vesicles yes yes 8 malign-lymph)
 (arched yes yes no yes yes no yes 1 3 round lac-marginal lac-marginal faint vesicles yes yes 2 metastases)
 (displaced no no no no no no yes 1 4 oval lacunar lac-central stripped vesicles yes yes 7 malign-lymph)
 (deformed yes yes no yes yes no yes 1 3 oval lacunar lac-marginal coarse no yes yes 1 metastases)
 (deformed no no no no yes no yes 1 4 oval lac-central lac-central grainy vesicles yes yes 3 malign-lymph)
 (displaced yes no no no yes no no 1 2 round lacunar lac-marginal grainy chalices no no 1 metastases)
 (arched yes no no no no no no 1 2 oval lac-marginal lac-marginal diluted chalices yes yes 1 metastases)
 (displaced no no no yes no yes yes 1 3 oval lac-marginal lac-central diluted vesicles yes yes 7 malign-lymph)
 (arched no no no no no no yes 1 4 round lac-central lac-central diluted vesicles yes yes 5 malign-lymph)
 (deformed yes yes no yes yes no no 1 3 round lac-central lac-marginal coarse chalices yes yes 3 metastases)
 (arched no no no no no no no 1 2 round lacunar lac-marginal drop-like chalices yes no 1 malign-lymph)
 (arched no no no no no no yes 1 3 oval lacunar lacunar reticular vesicles yes yes 6 malign-lymph)
 (deformed yes no no no yes no yes 1 3 round lac-marginal lac-marginal coarse vesicles yes yes 1 metastases)
 (deformed no no no no no no no 1 2 oval lacunar lacunar diluted no no yes 2 malign-lymph)
 (displaced yes no no yes yes no no 1 1 round lac-marginal lac-marginal drop-like vesicles yes yes 3 metastases)
 (deformed no no no no no no yes 1 4 round lac-marginal lac-central diluted vesicles yes yes 3 malign-lymph)
 (arched no no no no yes no no 1 2 oval lacunar lacunar drop-like no no no 1 malign-lymph)
 (displaced yes yes no yes yes no yes 1 3 round lac-central lac-marginal faint chalices yes yes 3 metastases)
 (displaced yes yes no no no no yes 1 2 oval lacunar lac-marginal drop-like chalices yes yes 2 metastases)
 (deformed no no no yes no no yes 1 2 round lac-marginal lac-marginal diluted vesicles no no 1 metastases)
 (normal no no no no no no no 1 1 bean no no no no no no 1 normal-find)
 (deformed yes no no no no no yes 1 3 oval lacunar lac-central faint vesicles yes yes 3 malign-lymph)
 (arched yes no no no yes no yes 1 3 round lac-marginal lac-central faint vesicles yes yes 2 malign-lymph)
 (displaced yes no no no no no no 1 4 round lac-marginal lac-marginal coarse chalices yes no 1 metastases)
 (arched no no no no no no yes 1 3 oval lacunar lacunar faint vesicles yes yes 5 malign-lymph)
 (arched yes yes no yes yes yes yes 1 3 round lac-central lac-central diluted vesicles yes yes 5 malign-lymph)
 (displaced no no no no yes no yes 1 4 round lac-central lacunar diluted vesicles yes yes 2 malign-lymph)
 (arched yes no no yes yes no no 1 2 oval lac-central lacunar faint vesicles yes yes 1 metastases)
 (arched yes no no no no no yes 1 2 round lac-marginal lac-marginal diluted chalices no yes 1 metastases)
 (arched yes no no no no no yes 1 2 oval lacunar lac-marginal faint vesicles no yes 1 metastases)
 (arched yes no no no yes no yes 1 3 round lac-central lacunar faint vesicles yes yes 2 malign-lymph)
 (deformed no no no no no no yes 1 3 oval lacunar lacunar faint no no no 1 metastases)
 (displaced yes yes no yes yes no yes 1 1 oval lacunar no drop-like no yes yes 2 malign-lymph)
 (arched yes yes no no yes no no 1 2 round lacunar lac-marginal drop-like chalices no no 1 metastases)
 (deformed yes yes no yes yes no yes 1 2 round lac-marginal lac-central grainy chalices yes no 1 metastases)
 (arched no no no no no no no 1 1 oval lacunar lac-marginal drop-like no yes yes 1 metastases)
 (arched no no no no no no yes 1 3 oval lac-central lac-central coarse vesicles yes yes 5 malign-lymph)
 (arched no no no no no no yes 1 2 oval lac-central lacunar faint vesicles yes yes 3 malign-lymph)
 (deformed yes no no no no no no 1 2 oval lac-marginal lac-marginal drop-like vesicles no no 2 metastases)
 (arched yes no no no yes no yes 1 2 round lacunar lacunar coarse chalices no no 2 metastases)
 (deformed yes no no no no no no 1 2 oval lac-marginal lac-marginal diluted no no no 1 metastases)
 (deformed yes no no no yes no no 1 2 oval lac-central lac-marginal coarse no yes yes 2 metastases)
 (arched no no no no yes no yes 1 3 round lac-central lacunar faint vesicles yes yes 3 malign-lymph)
 (arched yes no no no no no yes 1 2 oval lac-marginal lac-marginal grainy vesicles yes yes 2 metastases)
 (arched no no no no no no yes 1 2 oval lacunar lacunar coarse no yes no 2 metastases)
 (deformed yes no no yes yes no no 1 2 oval lac-central lac-marginal grainy no yes yes 3 metastases)
 (displaced no no no no no no yes 1 3 round lacunar lacunar coarse chalices yes yes 2 metastases)
 (arched yes no no no no no yes 1 4 round lac-central lacunar stripped vesicles yes yes 5 malign-lymph)
 (deformed yes no no no no no yes 1 2 round lac-marginal lac-marginal faint chalices yes yes 1 metastases)
 (arched yes no no no yes no yes 1 4 round lac-central lac-central faint vesicles yes yes 7 malign-lymph)
 (deformed yes no no no yes no no 1 2 round lac-central lacunar coarse no no no 1 metastases)
 (arched yes no no no no no yes 1 2 round lac-marginal lac-marginal diluted vesicles no yes 1 metastases)
 (arched no no no no no no no 1 2 round lac-marginal lac-marginal faint vesicles no no 1 metastases)
 (displaced no no no no yes no yes 1 4 round lac-central lac-marginal drop-like vesicles yes yes 5 malign-lymph)
 (arched no no no no no no no 1 2 oval lacunar lacunar faint no no no 1 metastases)
 (arched no no no no yes no yes 1 2 oval lacunar lacunar faint chalices no yes 2 malign-lymph)
 (deformed no no no no no no yes 1 2 oval lac-central lac-central grainy vesicles yes no 1 malign-lymph)
 (deformed no no no no yes no yes 1 2 oval lac-central lac-marginal faint chalices yes yes 1 malign-lymph)
 (displaced no no no no no no yes 1 3 oval lacunar lac-central drop-like chalices yes no 1 malign-lymph)
 (displaced yes yes no no yes no yes 1 2 oval lac-marginal lac-marginal faint vesicles yes yes 2 metastases)
 (arched no no no no yes no yes 1 3 oval lacunar lacunar coarse vesicles no yes 5 malign-lymph)
 (displaced yes no no yes yes no yes 1 3 round lac-central lacunar diluted vesicles yes yes 3 malign-lymph)
 (arched no no no no no no no 1 1 oval lac-central lac-marginal faint chalices yes yes 2 metastases)
 (arched no no no no no no no 1 2 oval lacunar lac-marginal faint chalices no yes 1 metastases)
 (arched yes yes yes yes yes no no 1 2 oval lac-central lac-marginal faint chalices yes yes 3 metastases)
 (deformed no no no no no no no 1 2 round lac-marginal lac-marginal coarse vesicles no yes 2 metastases)
 (displaced yes no no no yes no no 1 2 oval lac-marginal lac-marginal diluted chalices no yes 1 metastases)
 (deformed no no no no yes no yes 1 4 oval lacunar lacunar coarse vesicles yes yes 7 malign-lymph)
 (arched yes no no no no no yes 1 3 round lac-marginal lac-marginal drop-like chalices no yes 1 metastases)
 (arched yes no no no yes no yes 1 2 oval lac-marginal lac-marginal coarse chalices no yes 1 metastases)
 (arched no no no no yes no yes 1 2 oval lacunar lac-central faint no yes yes 2 malign-lymph)
 (deformed yes no no no yes no yes 1 3 oval lac-central lac-marginal coarse chalices yes yes 2 metastases)
 (arched yes no no no no no yes 1 4 round lac-central lacunar faint chalices no yes 4 metastases)
 (displaced yes yes yes yes yes no yes 1 4 round lac-central lac-central stripped vesicles yes yes 6 malign-lymph)
 (displaced no no no no no no yes 1 4 oval lac-central lacunar faint vesicles yes yes 6 malign-lymph)
 (deformed yes no no no no no yes 1 2 round lac-marginal lac-marginal diluted chalices yes no 1 metastases)
 (arched yes no no no yes no yes 1 2 oval lac-central lac-central diluted vesicles yes yes 1 metastases)
 (deformed yes yes yes yes yes no yes 1 2 round lac-marginal lac-marginal coarse vesicles yes yes 7 metastases)
 (normal no no no no yes no yes 1 2 oval no no grainy no no no 2 normal-find)
 (arched no no no no yes no yes 1 3 round lacunar lacunar coarse vesicles yes yes 6 malign-lymph)
 (arched yes no no no no no no 1 2 round lac-marginal lac-marginal drop-like vesicles no yes 1 metastases)
 (deformed no no no yes yes yes no 3 1 bean lacunar no diluted vesicles no no 7 fibrosis)
 (arched yes no no no no no no 1 2 round lac-marginal lac-marginal grainy chalices yes yes 1 metastases)
 (arched no no no no no no yes 1 2 oval lac-central lac-central faint chalices no yes 2 malign-lymph)
 (arched yes no no no yes no yes 1 2 round lac-marginal lac-marginal diluted vesicles yes yes 1 metastases)
 (deformed yes no no yes yes no yes 1 2 oval lac-central lac-marginal diluted chalices yes yes 4 malign-lymph)
 (arched no no no no no no no 1 1 bean no no drop-like no yes yes 1 metastases)
 (arched yes no no no yes no yes 1 3 round lac-marginal lac-marginal faint vesicles yes yes 4 malign-lymph)
 (arched no no no no no no yes 1 2 oval lac-central lacunar grainy no yes yes 1 metastases)
 (arched yes yes no yes yes no yes 1 3 round lac-central lac-marginal coarse vesicles yes yes 6 metastases)
)