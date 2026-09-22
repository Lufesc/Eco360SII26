# Notebook computacional en R
# El notebook se enfoca en construir intuición matemática usando R como calculadora simbólica y numérica — la base que necesitarás para entender de dónde salen las fórmulas de las próximas unidades.
# Instructor: Luis Fernando Escobar — UAGRM, Ingeniería Financiera

# 1) Vectores
# =========================================================

x <- c(1,2,3,4,5,6,7,8,9,10) 
x 

z <- 1:10 
z 

y <-rep(1, times=20)
y

edad_eco360 <- c(20,19,21,22,19,20);edad_eco360

#Nota: En muchos lenguajes de programación, la primera posición de 
#los vectores es 0, como se pudo observar en el caso anterior, 
#en R la primera posición es 1.

# 2) Funciones
# =========================================================

media <- mean(c(1,2,3,4,5));media

#                 Funciones Matemáticas
#sqrt(x)		Raíz de x
#exp(x)		Exponencial de x
#log(x)		Logaritmo natural de x
#log10(x)	Logaritmo base 10
#sum(x)		Suma de los elementos de x
#prod(x)		Producto de los elementos de x
#sin(x)		Seno
#cos(x)		Coseno
#tan(x)		Tangente
#round(x,n)	Redondea a n d?gitos
#cumsum(x)	Calcula las sumas acumuladas (x1,x1+x2,+x1+?+xn)

#                 Funciones Estadísticas
#mean(x)	Media
#sd(x)		Desviación estándar
#var(x)		Varianza
#median(x)	Mediana
#quantiles(x)	Quantiles
#cor(x,y)		Correlación
#max(x)		Valor máximo
#min(x)		Valor mínimo
#range(x)	Retorna el máximo y mínimo
#sort(x)		Ordena los elementos de x
#summary	Resumen de las variables
#choose(n,k)	Combinatoria de n sobre k

# Una función simple: costo total de una estrategia de inversión
costo_total <- function(unidades, precio, comision_fija = 5) {
  unidades * precio + comision_fija
}

costo_total(unidades = 100, precio = 25.4)

# Ejercicio guiado: define tu propia función `retorno_simple(precio_inicial, precio_final)` que calcule el retorno porcentual simple del precio de acciones 


# 3) Matrices
# =========================================================

matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)

#Argumentos		Significado
#data			Es un vector de datos opcional
#nrow			Número deseado de filas
#ncol			Número deseado de columnas
#byrow		Valor lógico. Si es falso (valor por defecto), la matriz se llena por orden columna, de otra manera se llenar? primero por filas.
#dimnames	Utilizado para darles nombres a las filas y a las columnas, respectivamente.

vec <- 1:10
matrix(vec, ncol=5, nrow=2)
matrix(vec, ncol=5, nrow=2, byrow = TRUE)

x <- matrix(c(1,2,3,4,5,6), ncol = 2, nrow=3)
x

class(vec)
class(x)

# Pesos de un portafolio de 3 activos
w <- matrix(c(0.5, 0.3, 0.2), nrow = 1)

# Matriz de varianzas-covarianzas (anualizada, ejemplo ilustrativo)
Sigma <- matrix(c(0.04, 0.01, 0.00,
                  0.01, 0.09, 0.02,
                  0.00, 0.02, 0.16),
                nrow = 3, byrow = TRUE)



# Varianza del portafolio: w %*% Sigma %*% t(w)
varianza_portafolio <- w %*% Sigma %*% t(w)
varianza_portafolio

# 4) Data Frames
# =========================================================

data.frame(w = 1, x = 1:10, y = LETTERS[1:10], z=runif(10))

df <- data.frame(w = 1, x = 1:10, y = LETTERS[1:10], z=runif(10))
df$w
df$y
class(df)

df1 <- data.frame(Letras = LETTERS[1:20], Valores=runif(20))
df1["Valores"]
df1[4,"Letras"]
df1[1,]
df1[,1]
df1[1,1]
df1[15,1]
df1[1,2]
df1[10,2]


# 5) Archivos en RData y graficos
# =========================================================

df = data.frame(runif(20), runif(20), runif(20)) 
names(df) = c("dato1", "dato2", "dato3")
df
plot(df$dato1)

Notas <- c(85,84,84.5,78,76.5,74.5,75)
plot(Notas)

plot(Notas, type="l")

plot(Notas, type="l", ann = F) 
title(main = "Notas del semestre I 2026", xlab="Número de materias", ylab = "Calificaciones")


# Uso de IA 
# Permitido: pedirle a la IA que explique qué hace una función matemática de R que no reconoces (`D()`, `expression()`, `%*%`), o que te ayude a corregir errores de sintaxis en tus propias funciones.


