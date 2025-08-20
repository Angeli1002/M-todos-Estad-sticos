> temperatura <- read.csv("C:/Repositorio FLOR/temperatura.csv")
>   View(temperatura)

head(temperatura) #Primeras 6 filas
dim(temperatura) #Numero de filas y columnas
names(temperatura) #Nombre de las columnas
str(temperatura) #Estructuera del obgeto

# Resumen estadistico 
summary(temperatura)

#Modificar nombre de columnas
names(temperatura) <- c("anio","Ene","Feb"," Mar", "Abr"," May ","Jun","Jul","Ago","Sep","Oct","Nov","Dic")


names(temperatura)

temperatura$Ene
temperatura$Media_anual <- rowMeans(temperatura[,2:13]) #row=filas Means=media #se abren[,] para crear filas y columnas_ antes de la  , seran las filas y despues seran las columnas 
head(temperatura)


#Crear objeto con medidas mensuales de temperatura 
medias_mensuales <- colMeans(temperatura[,2:13])
medias_mensuales

boxplot(temperatura$Ene,
        main="Temperatura de Enero",
        ylab="c",
        col="violet")



datos_meses <- temperatura[,2:13]
boxplot(datos_meses,
        main="Temperatura",
        ylab="c",
        col="violet",
        names=c ("Ene","Feb"," Mar", "Abr"," May ","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))
















