
library(readxl)
DF <- read_excel("Datos_peso_y_estatura.xlsx")
View(DF) 

str(DF)

DF1 <- DF

DF$Peso <- as.numeric(DF$Peso)

plot(
  x = DF$Estatura, y = DF$Peso,
  xlab = "Estatura en metros", ylab = "Peso en kilos"
)

ggplot(data = DF, aes(x = Estatura, y = Peso)) +
  geom_point() +
  scale_x_continuous("Estatura en metros", label = scales::comma) +
  ylab("Peso en kilos") 


OLS <- lm(Peso ~ 1+Estatura,data=DF)
summary(OLS)
