# Rodriguez Fabiana
# Legajo: 809761
# Fecha: 16/04/2026

# Desafío 1: Generador automático de Formas Canónicas (SOP)
# 1. Se crea tabla de verdad para A, B, C
df_logica <- expand.grid(
  A = c(FALSE, TRUE),
  B = c(FALSE, TRUE),
  C = c(FALSE, TRUE)
)

# 2. Se define una columna de salida
# Ley Distributiva, Ley de Morgan
df_logica$Salida <- (df_logica$A | df_logica$B) & !df_logica$C

# 3. Se filtran los resultados TRUE
minterminos <- subset(df_logica, Salida == TRUE)
print(minterminos)

# 4. Expresión SOP (Suma de Productos) de esos minterminos
# Los minterminos donde la Salida es 1 (TRUE):
# m2 (010), m4 (100) y m6 (110)
# Expresión SOP: Σm(2, 4, 6)


# Desafío 2: El Desafío de la Universalidad (NAND)
# 1. Se crea una función NAND
nand_gate <- function(a, b) { !(a & b) }

# 2. Se Construye OR usando solo NAND
# NAND(NOT a, NOT b) equivale a un OR
# Ley de idempotencia, Ley de Morgan
or_nand <- function(a, b) {
  nand_gate(nand_gate(a, a), nand_gate(b, b))
}

# Tabla de verdad
valores_a <- c(TRUE, TRUE, FALSE, FALSE)
valores_b <- c(TRUE, FALSE, TRUE, FALSE)

tabla_comparativa <- data.frame(
  A = valores_a,
  B = valores_b,
  OR_R = valores_a | valores_b,
  OR_NAND = or_nand(valores_a, valores_b)
)

# Se verifica identidad
print(tabla_comparativa)
print(paste("¿Son iguales?: ", all(tabla_comparativa$OR_R == tabla_comparativa$OR_NAND)))


# Desafío 3: El "Kill Switch" y Minimización de Reglas de Negocio
# 1. Definición de variables (Vector de prueba con 10 casos aleatorios)
set.seed(42) # Para reproducibilidad
casos <- 10
ingresos <- sample(c(T, F), casos, replace = TRUE)
antiguedad <- sample(c(T, F), casos, replace = TRUE)
vip <- sample(c(T, F), casos, replace = TRUE)
deuda <- sample(c(T, F), casos, replace = TRUE)

# 2. Función Original
F_original <- ((ingresos & antiguedad) | vip) & !deuda

# 3. Función Simplificada (Hipótesis)
F_minima <- ingresos & !deuda

# 4. Verificación
equivalentes <- identical(F_original, F_minima)
print(paste("¿La simplificación es válida?:", equivalentes))

# --- COMENTARIOS TÉCNICOS ---
# La variable 'deuda' actúa como un "Kill Switch" debido a la Ley de Identidad 
# y la propiedad del Elemento Absorbente en la conjunción (AND). 
# Si 'deuda' es TRUE, '!deuda' es FALSE. 
# Según la regla (X & FALSE = FALSE), no importa qué tan buenos sean los ingresos 
# o si el cliente es VIP; la presencia de deuda anula cualquier aprobación.