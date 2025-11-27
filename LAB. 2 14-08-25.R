################################################################################
# LAB.2 
# FLOR ANGELI CRUZ ROSALES 
# DR. MARCO A. GONZALEZ TAGLE
# 14/08/25
################################################################################

# Importar nuestra base de datos desde Excel 
temperatura <- read.csv("temperatura.csv")
View(temperatura)

# Algunas funciones 
head(temperatura) #Primeras 6 filas 
dim(temperatura) # Número de filas y columnas 
names(temperatura) # Nombres de las temperaturas 
str(temperatura) # Estructura del objeto 

# Resumen estadístico 
summary(temperatura)

#Modificar nombre de columnas (evitar ñ o nombres complicados)
names(temperatura) <- c("year","Ene", "Feb","Mar","Abr","May","Jun","Jul","Ago",
                        "Sep","Oct","Nov","Dic")
names (temperatura)

temperatura$Ene

# Para crear columnas que no tenemos también se usa el símbolo de $ 
# La función de rowMeans nos permite sacar la media de una fila/filas
# Utiliza el [], y se separa por una coma, antes de la coma se indican las filas 
# después de la coma se indican las columnas, en este caso como se quería abarcar 
# de la columna 2 a la 13 se utilizan los : 

temperatura$Media_anual <- rowMeans(temperatura[,2:13])
head(temperatura)

#Crear objeto con medias mensuales de temperatura
medias_mensuales <- colMeans(temperatura[,2:13])
medias_mensuales

# Crear boxplot, en donde main se refefiere al título de nuestro gráfico,
# ylab es nuestro eje y y col el color del gráfico
boxplot(temperatura$Ene,
        main="Temperatura de enero",
        ylab="C",
        fill="#528B8B")
datos_meses <- temperatura [,2:13]
boxplot(datos_meses,
        main= "Temperatura",
        ylab= "C",
        col="#B4EEB4",
        names= c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))



#https://r-charts.com/es/colores/ 
colors()