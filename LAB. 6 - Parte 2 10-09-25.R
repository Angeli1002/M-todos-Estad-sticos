#########################################################################################################
# LAB. 6 - Parte 2 
# FLOR ANGELI CRUZ ROSALES 
# DR. MARCO A. GONZALEZ TAGLE
# 10/09/25
#########################################################################################################

#Datos de producción de semilla para los años 2012 y 2013 
#Se expresa en kg semilla por árbol 

#Importar datos 
sem<-read.csv("mainproduccion.csv",header = T)
sem$Tiempo<-as.factor(sem$Tiempo)


tapply(sem$Kgsem,sem$Tiempo,mean)

boxplot(sem$Kgsem ~ sem$Tiempo,
        col= "brown",
        xlab="Año",
        ylab="Semilla(kg)")



#La función paired es para las pruebas de t student dependientes 

t2012<-subset(sem,sem$Tiempo=="T2012")
t2013<-subset(sem,sem$Tiempo=="T2013")
t.test(t2012$Kgsem,t2013$Kgsem, paired = T)


#LA DIFERENCIA ENTRE ESOS DOS AÑOS  
#NO FUE UNA DIFERENCIA SIGNIFICATIVA 
#ya que nuestro valor fue mucho mayor a a=0.05


t.test(t2012$Kgsem,t2013$Kgsem, paired = T, alternative="less")