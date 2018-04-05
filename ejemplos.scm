(;principio de la lista de ejemplos.
 (;definicion de los atributos
  (perspectiva (lluvia variable bueno frio niebla calorSeco));nominal
  (temperaturaGrados numerico);numerico
  (temperaturaTendencia (bajando estable subiendo));nominal
  (barometroTendencia (bajando estable subiendo));nominal
  (humedadEn% numerico);numerico
  (viento (si no));nominal
  (clase (+ -));nominal
 );fin definicion de atributos
 
;;Ejemplos
 (lluvia 5 bajando bajando 100 si -)
 (lluvia 17 estable bajando 65 si -)
 (variable 26 subiendo bajando 37 no +)
 (lluvia 12 bajando estable 70 no -)
 (variable 15 estable estable 46 no +)
 (bueno 27 subiendo estable 54 si +)
 (frio 5 bajando subiendo 20 no +)
 (bueno 23 estable subiendo 45 no +)
 (calorSeco 30 subiendo subiendo 10 si +)
 (niebla 8 bajando bajando 95 no -)
 (niebla 6 bajando bajando 88 si -)
 (calorSeco 35 subiendo subiendo 10 si +)
 (calorSeco 32 subiendo bajando 15 no +)
 (bueno 20 subiendo estable 50 si +)
 (bueno 24 estable subiendo 55 no +)
 (frio 6 estable subiendo 40 si -)
 (frio 1 bajando estable 30 no +)
 (bueno 22 estable estable 49 si +)
 (niebla 3 bajando estable 90 si -)
 (lluvia 26 bajando bajando 80 si -)
);fin de la lista de ejemplos