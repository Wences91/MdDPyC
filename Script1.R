#Pasos a seguir para la confeccion de un modelo lineal
# 1. Analisis preliminar (Grafico y Numerico)
# 2. Construccion del modelo
# 3. Estimacion de la bondad del modelo construido

# Carga de bibliotecas y datos
library(ISLR)
Train <- read.csv2("my_dataset_train.csv", sep=",", dec=".", stringsAsFactors = FALSE)
# Solo variables numéricas
Train.num <- Train[,-c(1, 15, 18, 52, 62, 64)]
# Instancias sin perdidos
instannciasNA <- sapply(1:dim(Train.num)[1], function(x) any(is.na(Train.num[x,])))
instannciasNA <- which(!instannciasNA)
train.num <- Train.num[instannciasNA,]


Yreal = train.num[,70]
X = cbind(1,iris$Sepal.Length,iris$Petal.Length,iris$Petal.Width)


# 1.a. Analisis preliminar (Grafico)

plot(train.num[,1:8])

# 1.b. Analisis preliminar (Numerico)

library(corrplot)

correlaciones <- cor(train.num)

# Prestando atención únicamente a las correlaciones superiores a 0.5 o inferiores a -0.5
corrplot(correlaciones>=0.5|correlaciones<=(-0.5), method = "circle", type = "lower")
# Se aprecia una correlación muy grande
correlaciones["x48","x41"]
# Una variable es la otra pero *0.02304689
train.num[,"x48"]/train.num[,"x41"]

# No parece haber correlación con la variable de salida

summary(train.num[,"x52"])
any(is.na(train.num[,"x52"]))
# 2. Construccion del modelo
reg_lineal <- glm(y~.-x46-x47-x48-x52-x53-x54-x59-x60-x62-x64-x65-x66-x67-x70-x71-x72-x74, data = train.num)
reg_lineal <- glm(y~x1+x2+x3+x5+x7+x9+x10+x11+x13+x15+x16+x19+x20+
                    x21+x22+x23+x27+x28+x32+x33+x34+x35+x36+x30+
                    x40+x42+x43+x44+x49, data = train.num)
reg_lineal <- glm(y~x1+x2+x3+x5+x7+x9+x10+x11+x13+x15+x19+x20+
                    x21+x23+x27+x32+x33+x34+x35+x36+
                    x40+x42+x43+x41, data = train.num)
resumen_reg_lineal <-summary(reg_lineal)
resumen_reg_lineal

# Da igual uno que otro
predicciones <-predict(reg_lineal, Test)
predicciones <-predict(reg_lineal, Test[,c("x1","x2","x3","x5","x7","x9","x10","x11",
                            "x13","x15","x19","x20",
                              "x21","x23","x27","x32","x33","x34","x35","x36",
                              "x40","x42","x43","x41")])

# x46-x47-x48-x52-x53-x54-x59-x60-x62-x64-x65-x66-x67-x70-x71-x72-x74


# 3. Estimacion de la bondad del modelo construido

# Vamos a tener en cuenta 7 criterios.
# a) Estimacion del Sigma (Error Estandar Residual [EER])
# Calculo manual del EER

n = length(Yreal)
p = length(reg_lineal) # Numero de parametros de la aproximacion
Ypred = predict(reg_lineal, data = train.num)
vr = sum( (Ypred-Yreal) * (Ypred-Yreal) )
vr = vr * (1/(length(Yreal)-p))
vr = sqrt(vr)
cv <- 100*(vr/mean(Yreal))
cv


# Residual muy por encima del 10%.





# b) Tabla ANOVA para responder a la siguiente hipotesis
# Pregunta: H0 B1 = B2 = .. = Bd = 0
# Se consulta en la tabla "summary" el valor p-value

resumen_reg_lineal

# El valor del p-value es inferior a 0.05 y por consiguiente, se rechaza H0.





# c) Coeficiente de determinacion (R cuadrado)
# Se mira "Multiple R cuadrado" que indica que porcentaje de la
# la variabilidad de los datos la explica el modelo

resumen_reg_lineal$r.squared

# El valor es 52.4%, inferior al 80% y por consiguiente no es aceptable.

# Se debe comparar con el "R cuadrado ajustado". Los valores deben ser 
# parecidos. Si no es asi, eso puede indicar que R cuadrado se ha visto
# afectado por un elevado numero de variables predictivas.

resumen_reg_lineal$adj.r.squared


# El calculo manual se hace de la siguiente forma
Ypred = predict(reg_lineal, data = datos)
n = length(Yreal)
p = length(reg_lineal)

VT = sum( (Yreal-mean(Yreal))*(Yreal-mean(Yreal)) )
VE = sum( (Ypred-mean(Yreal))*(Ypred-mean(Yreal)) )
VR = sum(  (Yreal-Ypred) * (Yreal-mean(Yreal)))

R2 = VE / VT
R2.corregido = 1 - (1-R2)*(n-1)/(n-p)


# En nuestro caso es de 51.4%, muy parecido al anterior y por tanto,
# nos creemos el valor del "Multiple R cuadrado".






# d) Calculo del Error Cuadratico Medio (MSE)
reg_lineal.fit <- predict(reg_lineal, newdata = datos)

sum(((datos$y-reg_lineal.fit)^2))/length(reg_lineal.fit)







# e) Normalidad

par(mfrow=c(2,2))
plot(reg_lineal)

# Miramos el grafico "Normal Q-Q" donde queremos que los datos esten lo mas
# proximos posibles a la linea discontinua. En nuestro caso parece
# que los errores se distribuyen segun una distribucion normal, aunque 
# los primeros valores parece que se alejan un poco de esta linea

# Para asegurar mas este diagnostico, haremos un histograma sobre los
# errores (residuos)

par(mfrow=c(1,1))

e <-residuals(reg_lineal)
d <- e/resumen_reg_lineal$sigma

hist (d, probability = T, xlab = "Errores estandar", main = "", xlim = c(-3,3))

d.seq <- seq(-3,3,length = 50)

lines(d.seq, dnorm(d.seq, mean(d), sd(d)), col="red")

# Un ultimo test para ver si los errores son normales
# El test de normalidad Shapiro-Wilk, tiene como H0: error~N(mean,sd)

shapiro.test(e)

# Al ser el p-value>0.05 no se puede rechazar la hipotesis de normalidad








# f) Homocedasticidad (hipotesis de varianza constante)

# Mirar el grafico de plot(reg_lineal) llamado "Residuals vs Fitted
# Se debe observar que la anchura de los datos es aproximadamente igual.
# Nos marca los ejemplos extremos que son el 135, 142 y 146.

# Usaremos el test de Breusch-Pagan que tiene como H0: homocedastidad

install.packages("lmtest")
library(lmtest)
bptest(reg_lineal)

# El valor obtenido es de 0.865, mayor que 0.05 por lo que no se puede descartar la hipotesis de homocedasticidad







# g) Incorrelacion

n <- length(d)
plot(d[1:n-1],d[2:n],xlab = "Error i", ylab = "Error i-1")
lines(lowess(d[1:n-1],d[2:n]),col="red")

# No se ve ninguna tencendia creciente o decreciente en la recta, 
# lo que parece que la hipotesis se cumple.

# Saldremos de dudas con un contraste de hipotesis
# Test de Durbin-Watson, con H0: correlacion 0

library(lmtest)
dwtest(reg_lineal,alternative = "two.sided")

# p-value >0.05 no se puede rechar la hipotesis de correlacion 0. Por tanto,
# satisface la condicion de incorrelacion.
























############################### Visualizacion #####################################

# Visualizacion del ajuste
# a) Mateniendo el orden de la secuencia del conjunto
plot(1:dim(datos)[1],datos[,1],xlab="Ejemplos",ylab="y")
pred <- predict(reg_lineal, newdata = datos)
points(1:dim(datos)[1],pred, col="red")


# b) Ordenando los datos de menor a mayor valor de la variable de salida
datos_ord <-datos[sort(datos[,1], index.return=TRUE)$ix,]
reg_lineal = lm(y~x1+x2+x3, data= datos_ord)
plot(1:dim(datos_ord)[1],datos_ord[,1],xlab="Ejemplos", ylab="y")
pred <- predict(reg_lineal, newdata = datos_ord)
points(1:dim(datos_ord)[1],pred, col="red")

# Se pueden incluir las discrepancias usando la siguiente sentencia
segments(1:dim(datos_ord)[1], datos_ord$y, 1:dim(datos_ord)[1], pred,col="blue", lty = 2)

# Se puede mostrar la linea entre los ejemplos descomentando la siguiente sentencia
# lines(1:dim(datos_ord)[1],pred, col="red")



