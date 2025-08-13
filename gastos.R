#Gastos totales
300+240+1527+400+1500+1833
celular <- 300
celular
transporte <- 240
comestibles <- 1527
gimnasio <- 400
alquiler <- 1500
otros <- 1833
total <- celular+transporte+comestibles+gimnasio+alquiler+otros
semestre <- total*5
anual <- total*10

#Valor Absoluto
abs(10)
abs(-4)

#Raiz cuadrada
sqrt(9)

#Logaritmo natural
log(2)

2*9
4+5 # también se puede colocar un comentario

celular <- 300
Celular <--300
CELULAR <- 800

celular+Celular
CELULAR+celular
help(abs)
help(mean)
?abs
?mean
help.search("absolute") #para buscar funciones relacionadas asociadas al documento 

gastos <- c(celular,transporte,comestibles,gimnasio,alquiler,otros)

gastos

barplot(gastos)

sort(gastos)
?sort #help(sort)
gastos_ord <- sort(gastos, decreasing = TRUE)
barplot(gastos_ord)
help("barplot")
barplot(gastos_ord,main = "Gastos mensuales",
        names.arg = c("otros","comestibles",
                      "alquiler","gimnasio",
                      "celular","transporte"))



















