```{r, eval=FALSE, include=TRUE}
"Protocolo:
 1. Daniel Felipe Villa Rengifo
 2. Lenguaje: R
 3. Tema: Climadiagrama
 4. Fuentes: Naïve Bayes con R 
    https://rpubs.com/mao045/485540"
```

#  Naive Bayes- clasificación bayesiano ingenuo

Naive Bayes es un modelo de predicción basado en la probabilidad Bayesiana. El modelo es muy simple, pero poderoso, en cuanto que es resultado directo de los datos y su tratamiento con simple estadística bayesiana de la probabilidad condicionada. Hay que tener en cuenta que se asume, por simplificación que las variables son todas sucesos independientes.

La función de clasificación ingenua de bayes se encuentra en varias librerías de R en: `naivebayes`, en el paquete `e1071` y en otros.


El modelo bayesiano de probabilidad condicionada se representa como: [`P(A|B) = P(A ∩ B)/ P(B)`], es decir, la probabilidad de que se de el caso `A` dado `B` es igual a la probabilidad de la intersección de `A` con `B`.

Estirando esta formulación llegaríamos al teorema de Bayes cuya expresion más típica es la siguente:

+ [`P(A|B) = P(B|A)*P(A) / P(B)`] <= __Teorema de Bayes__

# Crear un modelo con `naivebayes`:

La `tabla_1` que vamos a crear contiene 3 variables:

1. la hora del día
2. el lugar donde está Juan a esa hora
3. otra columna que nos indica si es o no fin de semana con un valor lógico (TRUE o FALSE).

```{r}
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
```

# Pasos a Seguir:

1. creamos la formula de modelo con la función `naive_bayes()` y luego definimos un hecho, una ocurrencia concreta de los parámetros y llamamos a la función `predict()`.


> Nota: Esta función es común a la mayoría de los modelos de predictivos,
> y sus argumentos son el nombre del modelo y un hecho almacenado como data.frame.
> Si añadimos el argumento `type="prob` nos da el resultado como pronostico probabilistico y si no, solo el pronóstico más probable.

```{r}
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
```
