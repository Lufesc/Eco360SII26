#  El CAPM como Extensión modelo de regresión con dos variables

# Escribir la ecuación del CAPM como una regresión de dos variables (retorno en exceso de una acción sobre el retorno en exceso del mercado).
# Construir retornos en exceso a partir de precios y una tasa libre de riesgo, con el cuidado de igualar las frecuencias (mensual vs. anualizada).
# Interpretar económicamente el alfa (desempeño anormal) y el beta (riesgo sistemático) estimados.
# Probar formalmente si una acción es "agresiva" (β > 1) o "defensiva" (β < 1) frente al mercado.

# Cargar la paqueterias
# install.packages(c("quantmod", "car"))
library(quantmod)
library(car)

# Acción de ejemplo: cambia el ticker por cualquier otra empresa que quieras analizar
getSymbols("F",     src = "yahoo", from = "2010-01-01", auto.assign = TRUE)   # Ford
getSymbols("^GSPC", src = "yahoo", from = "2010-01-01", auto.assign = TRUE)   # S&P500
getSymbols("DGS3MO", src = "FRED", auto.assign = TRUE)                        # T-bill 3 meses (FRED)

precio_accion  <- to.monthly(F,     indexAt = "lastof", OHLC = FALSE)[, "F.Adjusted"]
precio_mercado <- to.monthly(GSPC,  indexAt = "lastof", OHLC = FALSE)[, "GSPC.Adjusted"]
tbill_mensual  <- to.monthly(DGS3MO, indexAt = "lastof", OHLC = FALSE)[, 1] / 12  # anualizada -> mensual

capm <- na.omit(merge(precio_accion, precio_mercado, tbill_mensual))
names(capm) <- c("Accion", "Mercado", "TBill")
capm <- as.data.frame(capm)
head(capm)


#Construir retornos en exceso
capm$r_accion  <- c(NA, 100 * diff(log(capm$Accion)))
capm$r_mercado <- c(NA, 100 * diff(log(capm$Mercado)))
capm <- na.omit(capm)

capm$er_accion  <- capm$r_accion  - capm$TBill
capm$er_mercado <- capm$r_mercado - capm$TBill

head(capm[, c("er_accion", "er_mercado")])

# Diagrama de dispersión antes de estimar

plot(as.numeric(capm$er_mercado), as.numeric(capm$er_accion),
     pch = 19, col = "#2a78d6",
     xlab = "Retorno en exceso del mercado (%)",
     ylab = "Retorno en exceso de la acción (%)",
     main = "CAPM: relación entre exceso de retorno de la acción y del mercado")

# Estimar la ecuación del CAPM

lm_capm <- lm(er_accion ~ er_mercado, data = capm)
summary(lm_capm)


# ¿cuál es el beta estimado? ¿La acción es más o menos riesgosa que el mercado? ¿El alfa es estadísticamente distinto de cero?
## Probar si la acción es "agresiva" (H0: β = 1)
  
linearHypothesis(lm_capm, c("er_mercado = 1"))

## Permitido
# pedirle a la IA que explique por qué se restan tasas anualizadas y mensuales con cuidado, o que te ayude a depurar errores al descargar datos de FRED/Yahoo.

## No permitido
# pedirle a la IA que clasifique la acción como "agresiva" o "defensiva" por ti, o que redacte la interpretación económica de alfa y beta — esa conclusión debe ser tuya, con el resultado del test como evidencia.

# Nota obligatoria si usaste IA
# Completa antes de entregar: 
##"Usé [herramienta de IA] para _______________. Verifiqué la respuesta haciendo _______________."

