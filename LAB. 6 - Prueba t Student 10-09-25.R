################################################################################
# LAB. 6 - Prueba t Student con una sola muestra
# FLOR ANGELI CRUZ ROSALES 
# DR. MARCO A. GONZALEZ TAGLE
# 10/09/25 
################################################################################

# Se ingresan los datos 
costal<-c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94, 80.7,
          82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89, 82.95, 73.59,
          77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01, 80.47, 76.23, 78.89,
          77.14, 69.94, 78.54, 79.7, 82.45, 77.29, 75.52, 77.21, 75.99, 81.94,
          80.41, 77.7)

mean(costal)
var(costal)
sd(costal)

boxplot(costal)

# Tanta variabilidad nos idnica que la empresa no tiene un control de 
# calidad de su producto 

t.test(costal,mu=80)
