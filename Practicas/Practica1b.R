# Notebook computacional en R
# Introducción general al Modelo Clásico de Regresión Lineal
# 
# Instructor: Luis Fernando Escobar — UAGRM, Ingeniería Financiera


## Objetivos de aprendizaje

# - Explicar qué es la econometría y en qué se diferencia la econometría financiera de la econometría "general" (frecuencia de los datos, propiedades estadísticas de los retornos, tamaño muestral).
# - Describir, en sus propias palabras, los pasos para construir un modelo econométrico (teoría → especificación → estimación → evaluación → uso del modelo).
# - Reconocer qué preguntas hacerse al leer un artículo empírico de finanzas (¿qué se estima?, ¿con qué datos?, ¿qué supuestos se hacen?).
# - Calcular e interpretar estadísticas descriptivas (media, varianza, asimetría, curtosis) sobre series financieras reales.
# - Distinguir simple return de log return, y explicar cuándo conviene usar cada uno.
# - Reconocer los tipos de datos que se usan en econometría financiera (series de tiempo, corte transversal, panel) y sus implicaciones.
# - Calcular valor futuro y valor presente, y aplicar álgebra de matrices al retorno y riesgo esperado de un portafolio
# - Estimar una regresión lineal simple en R usando `lm()`.
# - Interpretar el coeficiente de pendiente como la **razón de cobertura óptima (hedge ratio)**.

## Conceptos clave
# - Econometría financiera: aplicación de métodos estadísticos a datos financieros, que suelen tener alta frecuencia, colas pesadas y volatilidad cambiante — a diferencia de muchos datos macroeconómicos.
# - Proceso de modelización: partir de una teoría económica/financiera, traducirla a una ecuación estimable, elegir los datos adecuados, estimar, diagnosticar y solo entonces usar el modelo para inferencia o pronóstico.
# - Retorno simple vs. retorno logarítmico: el retorno simple es la variación porcentual del precio; el retorno logarítmico (continuo) es aproximadamente igual para variaciones pequeñas, pero tiene la ventaja de ser aditivo en el tiempo — por eso se usa casi siempre en investigación académica de finanzas.
# - Asimetría y curtosis: los retornos financieros suelen mostrar colas más pesadas que la distribución normal (exceso de curtosis) — una razón central por la que muchos supuestos "clásicos" deben verificarse, no darse por hecho.
# - Datos de panel: combinan corte transversal y serie de tiempo (ej. varios activos observados a lo largo de varios periodos); requieren técnicas distintas a las de series puras.
# - Razón de cobertura óptima: número de unidades del activo futuro que se deben vender en corto por cada unidad del activo spot que se mantiene en cartera, para minimizar el riesgo de la posición combinada.

# 1) Instalaciones de librerias
# =========================================================

# Instalar solo la primera vez:
# install.packages(c("quantmod", "moments", "dplyr", "ggplot2"))

library(quantmod)
library(moments)
library(dplyr)
library(ggplot2)

# 2) Descarga de datos reales y cálculo de retornos
# =========================================================

getSymbols("^GSPC", src = "yahoo", from = "2018-01-01", auto.assign = TRUE)

precios <- Cl(GSPC)  # precio de cierre

retorno_simple <- dailyReturn(precios, type = "arithmetic")
retorno_log    <- dailyReturn(precios, type = "log")

datos <- merge(retorno_simple, retorno_log)
names(datos) <- c("r_simple", "r_log")
head(datos)

# 3) Estadísticas descriptivas
# =========================================================

resumen <- data.frame(
  media    = mean(datos$r_log, na.rm = TRUE),
  desv_est = sd(datos$r_log, na.rm = TRUE),
  asimetria = skewness(as.numeric(datos$r_log), na.rm = TRUE),
  curtosis  = kurtosis(as.numeric(datos$r_log), na.rm = TRUE)
)
resumen

# Pregunta: una distribución normal tiene curtosis igual a 3. ¿El valor que obtuviste es mayor o menor? ¿Qué implica esto para el uso de pruebas estadísticas que asumen normalidad?

# 4) Visualización de la distribución de retornos
# =========================================================

ggplot(datos, aes(x = r_log)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60, fill = "#2a78d6", alpha = 0.7) +
  stat_function(fun = dnorm,
                args = list(mean = mean(datos$r_log, na.rm = TRUE),
                            sd   = sd(datos$r_log, na.rm = TRUE)),
                color = "#eb6834", linewidth = 1) +
  labs(title = "Distribución de retornos diarios del S&P500",
       subtitle = "Barras: datos reales — Línea: normal con la misma media y desviación",
       x = "Retorno logarítmico diario", y = "Densidad") +
  theme_minimal()

# 5) Valor presente / valor futuro
# =========================================================

valor_futuro <- function(vp, tasa, periodos) vp * (1 + tasa)^periodos
valor_presente <- function(vf, tasa, periodos) vf / (1 + tasa)^periodos

valor_futuro(vp = 1000, tasa = 0.08, periodos = 5)
valor_presente(vf = 1000, tasa = 0.08, periodos = 5)

# 6) Archivos en RData y graficos
# =========================================================

retornos_esperados <- matrix(c(0.10, 0.14, 0.08), nrow = 3)  # 3 activos
w <- matrix(c(0.4, 0.35, 0.25), nrow = 1)

Sigma <- matrix(c(0.05, 0.02, 0.01,
                  0.02, 0.10, 0.03,
                  0.01, 0.03, 0.04),
                nrow = 3, byrow = TRUE)

retorno_portafolio  <- w %*% retornos_esperados
riesgo_portafolio   <- sqrt(w %*% Sigma %*% t(w))

list(retorno_esperado = retorno_portafolio, riesgo = riesgo_portafolio)

#Pregunta: prueba con distintos vectores de pesos `w` (que sumen 1). ¿Es posible reducir el riesgo del portafolio sin sacrificar demasiado retorno? Esta idea es la semilla de la teoría moderna de portafolios (Markowitz)

# 7) Modelo Clásico de Regresión Lineal: razón de cobertura óptima
# =========================================================

# Dataset: por defecto, este notebook descarga datos reales automáticamente (S&P500 spot vs. futuro E-mini) usando `quantmod`, para que corra sin depender de ningún archivo.
# Descargar datos reales de Yahoo Finance

# Spot: índice S&P500. Futuros: E-mini S&P500 futures (proxy negociable).
getSymbols("^GSPC", src = "yahoo", from = "2015-01-01", auto.assign = TRUE)
getSymbols("ES=F",  src = "yahoo", from = "2015-01-01", auto.assign = TRUE)

# Convertir a frecuencia mensual (precios de cierre ajustados)
spot_m    <- to.monthly(GSPC, indexAt = "lastof", OHLC = FALSE)[, "GSPC.Adjusted"]
futuros_m <- to.monthly(`ES=F`, indexAt = "lastof", OHLC = FALSE)[, "ES=F.Adjusted"]

SandPhedge <- merge(spot_m, futuros_m) %>%
  as.data.frame() %>%
  na.omit()
names(SandPhedge) <- c("Spot", "Futuros")
SandPhedge$Fecha <- as.Date(rownames(SandPhedge))

head(SandPhedge)

# Construcción de retornos logarítmicos
SandPhedge <- SandPhedge %>%
  mutate(
    r_spot    = c(NA, 100 * diff(log(Spot))),
    r_futuros = c(NA, 100 * diff(log(Futuros)))
  )

summary(SandPhedge[c("r_spot", "r_futuros")])

# Regresión — relación de corto plazo (retornos)
lm_retornos <- lm(r_spot ~ r_futuros, data = SandPhedge)
summary(lm_retornos)

# Visualización
ggplot(SandPhedge, aes(x = r_futuros, y = r_spot)) +
  geom_point(alpha = 0.6, color = "#2a78d6") +
  geom_smooth(method = "lm", se = TRUE, color = "#eb6834") +
  labs(
    title = "Retornos spot vs. retornos futuros",
    subtitle = "La pendiente de la recta es la razón de cobertura óptima (h*)",
    x = "Retorno futuros (%)", y = "Retorno spot (%)"
  ) +
  theme_minimal()


# Uso de IA 
# Permitido: pedirle a la IA que explique qué hace una función matemática de R que no reconoces (`D()`, `expression()`, `%*%`), o que te ayude a corregir errores de sintaxis en tus propias funciones.


