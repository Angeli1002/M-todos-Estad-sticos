#PRUEBAS DE T
#Caso de Muetras independientes 
#FACR
#27/08/2025

#IMPORTAR DATOS DE INDICE DE CALIDAD DE PLANTAS

calidad <- read.csv("calidad_plantas.csv",header=T)

calidad$Tratamiento <- as.factor(calidad$Tratamiento)

boxplot(calidad$IE~calidad$Tratamiento,
        col= "#D1EEEE",
        xlab = "tratamientos",
        ylab = "indice de calidad",
        ylim = c(0.4,1.2),
        main ="Vivero Iturbide")


#ESTADISTICA DESCRIPTIVAS
#tapply sirbe para obtener un valor cuamdo contamos 
#con varios grupos 
tapply(calidad$IE,calidad$Tratamiento,mean)
tapply(calidad$IE,calidad$Tratamiento,var)

#observamnos que la varianza del grupo fert 3 veces
#mas grande que el grupo de los datos 

library(ggplot2)

ggplot(calidad, aes(x= IE, color = Tratamiento))+
  geom_density()

#sd= desviacio estandar 
tapply(calidad$IE,calidad$Tratamiento,mean)
tapply(calidad$IE,calidad$Tratamiento,sd)


# SEPARAR LOS DATOS POR TRATAMIENTO 

df_ctlr <- subset(calidad,Tratamiento=="Ctrl")

df_fert <- subset(calidad, Tratamiento!="Ctrl")

#qqnorm revisar normalidad


qqnorm(df_ctlr$IE); qqline(df_ctlr$IE)
qqnorm(df_fert$IE); qqline(df_fert$IE)

par(mfrow = c(1,2))
qqnorm(df_ctlr$IE); qqline(df_ctlr$IE)
qqnorm(df_fert$IE); qqline(df_fert$IE)
par(mfrow = c(1,1))


#prueba de normalidad

shapiro.test(df_ctlr$IE)
shapiro.test(df_fert$IE)

#revisar homogeneidad de varianzas

var.test(df_ctlr$IE, df_fert$IE)
var.test(calidad$IE ~ calidad$Tratamiento)


#aplicar la prueva de T, varianza iguales
#dos colas = two.sided

t.test(calidad$IE ~ calidad$Tratamiento,
       var.equal = T,
       alternative = "two.sided")

#medir el efecto

cohens_efecto <- function(x,y) {
  n1 <- length(x); n2 <- length(y)
  s1 <- sd(x); s2 <- sd(y)
  sp <- sqrt(((n1-1)* s1^2 + (n2-1)* s2^2)/(n1+n2 - 2))
  (mean(x)- mean(y))/sp
}

d_cal <- cohens_efecto(df_ctlr$IE, df_fert$IE)
cohens_efecto(df_ctlr$IE, df_fert$IE)










