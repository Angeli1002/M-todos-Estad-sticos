# ------------------------------------------------------------------------------
# FLOR ANGELI CRUZ ROSALES
# Fecha: 01/10/2025
# Lab.7...
# ------------------------------------------------------------------------------

options(repos = c(CRAN = "https://cloud.r-project.org"))

if(!require(corrplot)) install.packages("corrplot")
if(!require(Hmisc)) install.packages("Hmisc")

library(corrplot)
library(Hmisc)


##############################################################################
# EJERCICIO 1: Efímeras vs Velocidad del arroyo
##############################################################################

cat("EJERCICIO 1: Efímeras vs Velocidad del arroyo\n\n")

speed <- c(2, 3, 5, 9, 14, 24, 29, 34)
abundance <- c(6, 3, 5, 23, 16, 12, 48, 43)

cat("Datos de campo:\n")
cat("Velocidades:", speed, "\n")
cat("Efímeras contadas:", abundance, "\n\n")

cat("HIPOTESIS:\n")
cat("   H0: La velocidad NO afecta a las efímeras\n")
cat("   H1: A mas velocidad, MAS efímeras\n\n")

plot(speed, abundance,
     main = "Relacion entre velocidad y abundancia de efímeras",
     xlab = "Velocidad (cm/seg)",
     ylab = "Cantidad de efímeras",
     pch = 16, col = "#556B2F", cex = 1.8)

abline(lm(abundance ~ speed), col = "#CAFF70", lwd = 3)

cat("OBSERVACION:\n")
cat("   Parece haber mas efímeras cuando la velocidad aumenta,\n")
cat("   pero no es una relación perfecta - hay variacion en los datos.\n\n")

shapiro_speed <- shapiro.test(speed)
shapiro_abundance <- shapiro.test(abundance)

cat("PRUEBA DE NORMALIDAD:\n")

cat("   Velocidad: p =", round(shapiro_speed$p.value, 4), 
    ifelse(shapiro_speed$p.value > 0.05, "(Normal)", "(No normal)"), "\n")
cat("   Efímeras: p =", round(shapiro_abundance$p.value, 4),
    ifelse(shapiro_abundance$p.value > 0.05, "(Normal)", "(No normal)"), "\n\n")

if(shapiro_speed$p.value > 0.05 & shapiro_abundance$p.value > 0.05){
  cor_result <- cor.test(speed, abundance, method = "pearson")
  metodo <- "Pearson"
  cat("Usando correlacion de PEARSON (datos normales)\n")
} else {
  cor_result <- cor.test(speed, abundance, method = "spearman") 
  metodo <- "Spearman"
  cat("Usando correlacion de SPEARMAN (datos no normales)\n")
}

cat("\nRESULTADOS:\n")
cat("   Coeficiente r =", round(cor_result$estimate, 3), "\n")
cat("   Valor p =", round(cor_result$p.value, 4), "\n")

if(cor_result$p.value < 0.05) {
  if(cor_result$estimate > 0) {
    cat("   SIGNIFICATIVO: Hay correlacion positiva\n")
    cat("   CONCLUSION: Las efímeras prefieren aguas mas rapidas\n")
  } else {
    cat("   SIGNIFICATIVO: Hay correlacion negativa\n")
    cat("   CONCLUSION: Las efímeras prefieren aguas mas tranquilas\n")
  }
} else {
  cat("   NO SIGNIFICATIVO: No hay correlacion clara\n")
  cat("   CONCLUSION: La velocidad no parece afectar significativamente\n")
}

##############################################################################
# EJERCICIO 2: Analisis de suelo
##############################################################################

cat("EJERCICIO 2: Analisis de propiedades del suelo\n\n")

#data.frame= tabla de datos
# Columna "Gp"
# Columna "Block"
# Columna "pH"
# Columna "N"

suelo <- data.frame(
  Gp = c("T0", "T0", "T0", "T0", "T1", "T1", "T1"),
  Block = c(1, 2, 3, 4, 1, 2, 3),
  pH = c(5.40, 5.65, 5.14, 5.14, 5.14, 5.10, 4.70),
  N = c(0.188, 0.165, 0.260, 0.169, 0.164, 0.094, 0.100),
  Dens = c(0.92, 1.04, 0.95, 1.10, 1.12, 1.22, 1.52),
  P = c(215, 208, 300, 248, 174, 129, 117),
  Ca = c(16.35, 12.25, 13.02, 11.92, 14.17, 8.55, 8.74),
  Mg = c(7.65, 5.15, 5.68, 7.88, 8.12, 6.92, 8.16),
  K = c(0.72, 0.71, 0.68, 1.09, 0.70, 0.81, 0.39),
  Na = c(1.14, 0.94, 0.60, 1.01, 2.17, 2.67, 3.32),
  Conduc = c(1.09, 1.35, 1.41, 1.64, 1.85, 3.18, 4.16)
)

cat("Muestras de suelo:", nrow(suelo), "muestras con 11 variables\n")
cat("Analizando correlaciones con pH...\n\n")

variables <- suelo[, c("pH", "N", "Dens", "P", "Ca", "Mg", "K", "Na", "Conduc")]
resultados <- rcorr(as.matrix(variables), type = "pearson")
cor_matrix <- resultados$r
p_matrix <- resultados$P

cat("CORRELACIONES CON pH\n")
################################################################################
cat("Variable    r     p-value   Significativo\n")

variables_check <- c("N", "Dens", "P", "Ca", "Mg", "K", "Na", "Conduc")
for(variable in variables_check) {
  r_val <- round(cor_matrix["pH", variable], 3)
  p_val <- round(p_matrix["pH", variable], 4)
  signif <- ifelse(p_val < 0.05, "SI", "NO")
  cat(sprintf("pH - %-6s  %5.3f   %6.4f      %s\n", 
              variable, r_val, p_val, signif))
}

cat("\nINTERPRETACION:\n")
if(p_matrix["pH", "Ca"] < 0.05) {
  if(cor_matrix["pH", "Ca"] > 0) {
    cat("   pH y Calcio: Relacion positiva - a mas pH, mas calcio\n")
  }
}
if(p_matrix["pH", "Conduc"] < 0.05) {
  if(cor_matrix["pH", "Conduc"] < 0) {
    cat("   pH y Conductividad: Relacion negativa\n")
  }
}
if(p_matrix["pH", "P"] < 0.05) {
  cat("   pH y Fosforo: Relacion significativa\n")
}

cat("\nGrafico de correlaciones...\n")
orden <- c("K", "pH", "P", "N", "Ca", "Mg", "Dens", "Na", "Conduc")
cor_matrix_ordenada <- cor_matrix[orden, orden]

corrplot(cor_matrix_ordenada, 
         method = "circle",       
         type = "upper",
         order = "original",
         col = colorRampPalette(c("darkred", "white", "darkblue"))(100),
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.9,
         title = "Figura 2: Gráfica de las correlaciones de la base de datos suelo")

##############################################################################
# EJERCICIO 3: Cuarteto de Anscombe
##############################################################################

cat("EJERCICIO 3: Cuarteto de Anscombe\n\n")

cat("Los 4 conjuntos de Anscombe tienen estadisticas similares\n")
cat("pero distribuciones muy diferentes visualmente.\n\n")

graphics.off()  # Cierra todos los gráficos anteriores

par(mfrow = c(2, 2), mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))

data(anscombe)

# Crear los 4 gráficos
for (i in 1:4) {
  x <- anscombe[, i]
  y <- anscombe[, i + 4]
  
  plot(x, y,
       main = paste("Conjunto", i),  
       xlab = paste("x", i),
       ylab = paste("y", i),
       pch = 16,
       col = "#FF69B4", 
       cex = 1.5,
       xlim = c(3, 19),
       ylim = c(3, 13))
  
  abline(lm(y ~ x), col = "purple", lwd = 2)
  
  r <- round(cor(x, y), 3)
  legend("topleft", 
         legend = paste("r =", r), 
         bty = "n",
         text.col = "purple",
         cex = 1.2)
}

# Restaurar configuración normal
par(mfrow = c(1, 1))

cat("ANALISIS DE LOS GRAFICOS:\n")
cat("   1. Relacion lineal clara\n")
cat("   2. Relacion no lineal (curva)\n") 
cat("   3. Presencia de outlier\n")
cat("   4. Un punto influyente domina la correlacion\n\n")

cat("CONCLUSION:\n")
cat("   Es esencial visualizar los datos antes del analisis estadistico\n")



