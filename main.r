## ---- eval=FALSE, include=TRUE----------------------------------------------------
## "Protocolo:
##  1. Daniel Felipe Villa Rengifo
##  2. Lenguaje: R
##  3. Tema: Climadiagrama
##  4. Fuentes: Naïve Bayes con R
##     https://rpubs.com/mao045/485540"


## ---------------------------------------------------------------------------------
#  CREAMOS UNA TABLA DE DATOS:

# 1.
tabla_1<-data.frame(hora=c(8,14,24,8,14,24,8,14,24,8,14,24,8,14,24,8,14,24,24,24))

#2.
tabla_1$lugar<-c("casa","restaurante","casa",
                 "trabajo","trabajo","casa",
                 "trabajo","trabajo","casa",
                 "casa","restaurante","casa",
                 "trabajo","trabajo","casa",
                 "casa","restaurante","casa","cine","cine")

#3.
tabla_1$finde<-c(T,T,T,
                 F,F,F,
                 F,F,F,
                 T,T,T,
                 F,F,F,
                 T,T,T,
                 T,F
                )

# Miremos la clase y los tipos de de variables de la tabla creada:
print(str(tabla_1))

# AHora observemos la tabla:
head(tabla_1)

#######AHora fablricaremos una tabla de doble entrada, donde en un lado tendremos los registros de hora y en el otro os registros del lugar:

# creamos la tabla [registros de hora según el lugar]
registrosHL <- table(tabla_1$hora,tabla_1$lugar, dnn = c("Hora", "Lugar"))

write.table(registrosHL, file = "registrosHL.txt", row.names = T)

print(registrosHL)

"Como vemos, es una tabla con 20 regisros y 3 variables en columnas, sobre la que queremos practicar pronósticos bayesianos de probabilidad condicionada."


## ---------------------------------------------------------------------------------
# cargamos la librería
#install.packages("naivebayes")
library(naivebayes)

# creamos el modelo de pronostico

#Crearemos un modelo de pronóstico de la variable dependiente lugar a partir de las variables independientes [hora] y [finde]. Este modelo nos diría por ejemplo la probabilidad de que: sabiendo la hora y si es o no fin de semana, Juan se encuentre en un lugar determinado

m <- naive_bayes(lugar ~ hora+finde, data = tabla_1)


# Probariemos la sigueinte teoria:

#Como vimos en las consideraciones previas, los modelos de pronostico bayesiano,
#y en particular [naivebayes] funcionan muy mal con datos numéricos continuos, y vamos a ver la prueba,
#pues crearemos un modelo con la variable hora "tal cual", y despues haremos el mismo modelo con la variable hora convertida en "factor".



pdf("ModeloNaivebayes1.pdf")

# representamos graficamente el modelo
plot(m)

dev.off()

# ejecutando predict(modelo) tenemos los resultados de pronostico para cada registro de datos

tabla_1$p=predict(m)

head(tabla_1)

write.csv(tabla_1, file = "Base1.csv", row.names = F)

"Aqui presentamos continuidad con las matrices de confusión y modelos de predictivos"
