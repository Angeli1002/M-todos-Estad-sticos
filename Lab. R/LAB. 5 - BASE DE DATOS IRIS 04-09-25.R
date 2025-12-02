################################################################################
# LAB. 5 - BASE DE DATOS IRIS - TAREA HW_02
# FLOR ANGELI CRUZ ROSALES 
# DR. MARCO A. GONZALEZ TAGLE
# 04/09/25
################################################################################

# La base de datos iris es uno de los conjuntos de datos más utilizados en 
# estadística y aprendizaje automático

# El conjunto contiene 150 observaciones correspondientes a tres especies de iris 
# (setosa, versicolor y irginica), con 50 muestras por especie. 

# Para cada flor se registran cuatro variables cuantitativas:
# Sepal.Length: longitud del sépalo (cm)
# Sepal.Width: ancho del sépalo (cm)
# Petal.Length: longitud del pétalo (cm)
# Petal.Width: ancho del pétalo (cm)


# Explorar la base de datos iris usando funciones como head(), summary()
data("iris")
head(iris)
str(iris)
summary(iris)

# Identificar las variables Petal.Length y determina las estadísticas 
# descriptivas para las dos especies

data_sub <- subset(iris, Species %in% c("versicolor", "virginica"))
head(data_sub)
table(data_sub$Species)

# Función tapply para obtener promedio 
tapply(data_sub$Petal.Length, data_sub$Species, mean)
# Setosa versicolor  virginica 
#  NA      4.260      5.552
# Los pétalos de virginica son más largos  1.3cm más

# Función tapply para obtener desviación estándar 
tapply(data_sub$Petal.Length, data_sub$Species, sd)
# Setosa versicolor  virginica 
#    NA 0.4699110  0.5518947 


# Función tapply para obtener varianza
tapply(data_sub$Petal.Length, data_sub$Species, var)
# Setosa versicolor  virginica 
#   NA  0.2208163  0.3045878 
# Mayor variabilidad en la especie virginica 

summary(data_sub$Petal.Length)
tapply(data_sub$Petal.Length, data_sub$Species, summary)

# Defina una pregunta de investigación sobre la variable Petal.Length
# Pregunta: ¿Hay una diferencia en la longitud de los pétalos entre las especies versicolor y virginica?

# Si hay una diferencia en la longitud de los pétalos, para obtener información 
# más confiable si realizamos una prueba de t student 


# Separar los datos por especie usando subset

df_versicolor <- subset(iris, Species == "versicolor")
df_virginica<- subset(iris, Species == "virginica")


# qqnorm  es un gráfico que nos ayudará a revisar la normalidad de los datos

# Función para que la ventana de gráficas permita que nos 
# aparezca dos gráficos par(mfrow) 
# Una fila con dos columnas (1,2)

par(mfrow=c(1,2))
qqnorm(df_versicolor$Petal.Length); qqline(df_versicolor$Petal.Length)
qqnorm(df_virginica$Petal.Length); qqline(df_virginica$Petal.Length)
# El gráfico muestra que una normalidad en nuestros datos ya que
# en la mayoría los puntos estan muy cercanos a la línea recta 

# Pero podemos verificarlo de manera más precisa realizando una prueba de Shapiro-Wilkins 

shapiro.test(df_versicolor$Petal.Length)
# data:  df_versicolor$Petal.Length
# W = 0.966, p-value = 0.1585

shapiro.test(df_virginica$Petal.Length)
# data:  df_virginica$Petal.Length
# W = 0.96219, p-value = 0.1098

# *Los resultados son mayores a p-value 0.05,por lo que se acepta la normalidad*
# Cumple con uno de los tres criterios para realizar la prueba de t student 



# Revisar homogeneidad de varianzas (segundo criterio)
var.test(Petal.Length ~ Species, data= subset(iris, Species %in% c("versicolor", "virginica")))
# El resultado es un p-value= 0.2637 lo que nos indica un valor mayor 
# a p-value=0.05, por lo que no se rechaza mi hipótesis nula 
# Hay homogeneidad de varianzas pues se toman como iguales 

# Se cumplen con los 3 criterios necesarios para realizar una prueba de t-student 

iris_sub <- subset(iris,Species %in% c("versicolor", "virginica"))
t.test(Petal.Length ~ Species, data= iris_sub,
       var.equal = TRUE)
# Resultados= t = -12.604, valor negativo que nos indica que hay una diferencia grande 
# entre las medias
# p-value < 2.2e-16 , valor pequeño y menor a p-value= 0.05 por lo que
# se rechaza la hipótesis nula (H0)


# Intervalo de confianza 
# -1.495426 -1.088574

# Diferencia de las medias: 4.260 - 5.552 
#[1]-1.292 

# La diferencia esta fuera del intervalo de confianza, lo que 
# nos confirma que es una hipótesis alternativa 
# Si no entra en el intervalo de confianza es H1 (alternativa)
# Si esta dentro del intervalo es H0 (nula)


# Se puede afirmar que los pétalos de la especie virginica son más largos 
# que los de la especie versicolor 
# Se acepta la hipótesis alternativa que nos indica que si existe diferencia en la 
# longitud de los pétalos entre las especies versicolor y virginica

# Se determina con el Tamaño de efecto (Cohen´s)

cohens_efecto <- function(x,y) {
  n1 <- length(x); n2 <- length(y)
  s1 <- sd(x); s2 <- sd(y)
  sp <- sqrt(((n1-1)* s1^2 + (n2-1)* s2^2)/(n1+n2 - 2))
  (mean(x)- mean(y))/sp
}

d_cal <- cohens_efecto(df_virginica$Petal.Length, df_versicolor$Petal.Length)
cohens_efecto(df_virginica$Petal.Length, df_versicolor$Petal.Length)

# 2.520756 es mayor al rango de 1.3, por lo que se puede deducir que la 
# diferencia en la longitud del pétalo es estadisticamente significativa 
# y relevante 


#Visualización: boxplot que muestra las diferencias entre especies 

library(ggplot2)
ggplot(iris_sub, aes(x = Species, y = Petal.Length, fill = Species)) +
  geom_boxplot() +
  labs(title = "Comparación de Petal.Length entre especies",
       x = "Especie",
       y = "Longitud del Pétalo (cm)")
