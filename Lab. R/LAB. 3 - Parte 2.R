################################################################################
# LAB.3 - Parte "2"
# FLOR ANGELI CRUZ ROSALES 
# DR. MARCO A. GONZALEZ TAGLE
# 21/08/25
################################################################################

dbh <- c(16.5, 25.3, 22.1, 17.2, 16.1, 8.1, 34.3, 5.4, 5.7, 11.2, 24.1,
         14.5, 7.7, 15.6, 15.9, 10, 17.5, 20.5, 7.8, 27.3,
         9.7, 6.5, 23.4, 8.2, 28.5, 10.4, 11.5, 14.3, 17.2, 16.8)

url <- "https://repodatos.atdt.gob.mx/api_update/senasica/actividades_inspeccion_movilizacion/29_actividades-inspeccion-movilizacion.csv" 
inspeccion <- read.csv(url)
head (inspeccion)

prof_url_2 <- paste0("https://repodatos.atdt.gob.mx/api_update/senasica/",
                     "actividades_inspeccion_movilizacion/",
                     "29_actividades-inspeccion-movilizacion.csv")

# No olvidar cargar la paquetería
library(repmis)
conjunto <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")
head(conjunto) 

library(readr)
file <- paste0("https://raw.githubusercontent.com/mgtagle/",
               "202_Analisis_Estadistico_2020/master/cuadro1.csv")
inventario <- read_csv(file)

head(inventario)
# Cargar paquete repmis Miscellaneous Tools for Reproducible Research para
# cargar datos en R:source_data, usando la opción de Tools. 
# Posteriormente Install packages, se busca repmis en buscador

################################################################################
# 21/08/2025 P:2 Operaciones con la base de datos
################################################################################

parcelas <- gl(3,7)
parcelas

# Se agrego una cifra de dbh para completar los 21 datos
trees <- seq(1,21)
dbh <- c(16.5, 25.3, 22.1, 17.2, 16.1, 8.1, 34.3, 5.4, 5.7, 11.2, 24.1,
         14.5, 7.7, 15.6, 15.9, 10, 17.5, 20.5, 7.8, 27.3,
         9.7)

trees <- data.frame(trees,dbh,parcelas)
View(trees)
trees

# Agrega el vector dbh como nueva columna en el data frame trees
trees$dbh <- dbh
trees$dbh <- dbh

# El signo de $ informa que necesitamos la columna dbh 
mean(trees$dbh)
sd(trees$dbh)

# Indica la sumatoria de los individuos en el objeto tree con un dbh < a 10
sum(trees$dbh < 10)

which(trees$dbh < 10)

trees.13 <- trees[!(trees$parcela=="2"),]
trees.13

trees.1<-subset(trees,dbh<=10)
head(trees.1)

mean(trees$dbh)

mean(trees.1$dbh)

################################################################################
# 21/08/2025 P:3 Representación gráfica
################################################################################

mamiferos <- read.csv("https://www.openintro.org/data/csv/mammals.csv")

hist(mamiferos$total_sleep)

# Datos

hist(mamiferos$total_sleep,
     xlim = c(0,20), ylim = c(0,14), 
     main = "Total de horas sueño de las 39 especies", 
     xlab = "Horas sueño", 
     ylab = "Frecuencia", 
     las = 1, 
     col = "#E0FFFF") 

data("chickwts")
head(chickwts[c(1:2,42:43, 62:64), ])

feeds <- table(chickwts$feed)
feeds

barplot(feeds)
barplot(feeds, col =  c("#68838B", "#D1EEEE", "#F0FFF0"))
barplot(feeds[order(feeds, decreasing = TRUE)])



# Tabla de frecuencia tipo alineamiento
feeds <- table(chickwts$feed)


# Ordenar de mayor a menor 
barplot(feeds[order(feeds)], horiz = TRUE, 
        col = c("#68838B", "#D1EEEE", "#F0FFF0"),
        main = "Horas de sueño de las especies", 
        xlab = "Número de horas", 
        las = 1)


