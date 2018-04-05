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
 (lluvia 2 bajando bajando 75 si -)
 (lluvia 28 estable bajando 65 no -)
 (variable 22 subiendo bajando 30 no +)
 (niebla 8 bajando estable 60 no -)
 (bueno 26 estable subiendo 43 si +)
);fin de la lista de ejemplos
