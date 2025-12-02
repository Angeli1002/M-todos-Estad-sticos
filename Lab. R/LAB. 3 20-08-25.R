################################################################################
# LAB. 3 
# FLOR ANGELI CRUZ ROSALES 
# DR. MARCO A. GONZALEZ TAGLE
# 20/08/25
################################################################################


temperatura <- read.csv("temperatura.csv",header = TRUE)
View(temperatura)
temperatura <- read.csv("C:/Users/angel/OneDrive/Documents/M-todos-Estad-sticos/temperatura.csv")
View(temperatura)



# Importar datos  --------------------------------------------------------------
if(file.exists("Data/medias_mensuales.csv")) {
  Temp <- read.csv("Data/medias_mensuales.csv", header = TRUE)
} else {
  Temp <- temperatura
}

# Ingresar datos de manera manual  ---------------------------------------------
head(temperatura) #Primeras 6 filas
dim(temperatura) #Numero de filas y columnas
names(temperatura) #Nombre de las columnas
str(temperatura) #Estructuera del obgeto

# Resumen estadistico 
summary(temperatura)

#Modificar nombre de columnas
names(temperatura) <- c("anio","Ene","Feb"," Mar", "Abr"," May ","Jun","Jul",
                        "Ago","Sep","Oct","Nov","Dic")


names(temperatura)

temperatura$Ene
temperatura$Media_anual <- rowMeans(temperatura[,2:13]) 
#row=filas Means=media 
#se abren[,] para crear filas y columnas_ antes de la , seran las filas y despues seran las columnas 
head(temperatura)

#Crear objeto con medidas mensuales de temperatura 
medias_mensuales <- colMeans(temperatura[,2:13])
medias_mensuales

# Graficar datos ---------------------------------------------------------------
boxplot(temperatura$Ene,
        main="Temperatura de Enero",
        ylab="c",
        col="#F0FFF0")

datos_meses <- temperatura[,2:13]
boxplot(datos_meses,
        main="Temperatura",
        ylab="c",
        col="#F0FFF0",
        names=c ("Ene","Feb"," Mar", "Abr"," May ","Jun","Jul","Ago",
                 "Sep","Oct","Nov","Dic"))

# Estadísticas descriptivas  ---------------------------------------------------


edad <- c(18,19,18,18,25,19,18,18,18,17,19,
          19,18,17,19,18,19,19)

#Secuencia (seq), que empiece en el 1, termine en el 18 
#y se vaya de uno en uno 
alumno <- seq(1,18,1)

info <- data.frame(alumno,edad)
info$Altura<-c(174,174,170,160,158,155,188,170,175,170,175,
               170,172,170,174,180,158,164)
# Graficar datos ---------------------------------------------------------------
boxplot(info$Altura,
        #Col es para colorear el gráfico
        col= "#B4EEB4",
        #Main sirve para poner un título 
        main= "Alunos clase 3 semestre")







