################################################################################
# LAB. 7 - Correlación de Pearson 
# FLOR ANGELI CRUZ ROSALES 
# DR. MARCO A. GONZALEZ TAGLE
# 24/09/2025
################################################################################


# Ambas variables son métricas (medibles)

data("faithful")

# Causa = tiempo 
# Efecto = erupción 
# Correlación con causalidad 

plot(faithful$waiting,faithful$eruptions,
     xlab="Tiempo de espera (min)",
     ylab="Erupción (min)",
     col="#B4EEB4",
     pch=20)

# Función de correlación 
# Correlacionar las dos variables 
# Hipótesis nula= correlación igual a 0 
# Hipótesis alternativa= correlación diferente a 0 


# Pearson solo se utiliza cuando tenemos datos normales
cor.test(faithful$waiting, faithful$eruptions, 
         method = "pearson")
# Correlación = 0.90 *Muy alta / Significativa* 

# Para ver si la distribución de mis datos es normal 
# *Pearson's product-moment correlation*
# data:  faithful$waiting and faithful$eruptions
# t = 34.089, df = 270, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.8756964 0.9210652
# sample estimates:
#       cor 
# 0.9008112 




shapiro.test(faithful$eruptions)
shapiro.test(faithful$waiting)

# Los datos no son normales ya que son valores menores al 0.05 
# Si eso pasa entonces no se utiliza el método de Pearson
# Se utiliza el método de Spearman
# Pearson solo funciona con datos normales 
# Spearman funciona con datos no normales 


# Spearman se utiliza como contraparte de datos NO normales
# (Datos de manera ascendente)
cor.test(faithful$waiting, faithful$eruptions,
         method= "spearman")













# Método de Spearman
rho_value <- cor(faithful$waiting, faithful$eruptions, method = "spearman")
metodo <- "Spearman"

#Correlación (resultado)=0.77 
#Corr: Muy alta / Significativa 

#NOTA=se deben de tomar en cuenta el valor de p 
#para saber que método se utilizará, basandonos en 
#la distribución de los datos (normal o no nomral)
#Después el valor de r, nos indica que tan débil o fuerte es la 

#correlación 

