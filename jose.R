#José Ángel


#*****************************************
#Librerías
#*****************************************
library(Boruta)
library(caret)
library(NoiseFiltersR)
library(parallel)

#******************************************
#Lectura de datos
#******************************************

Train <- read.table("data/my_dataset_train.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)
Test <- read.table("data/my_dataset_test.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)

#Guardamos una copia para trabajar

train<-Train
test<-Test

#********************************************************
#Análisis Exploratorio
#********************************************************

#Comprobamos el tipo de variables que tenemos entre manos:

str(train)

#Todas son numéricas excepto algunas binarias (strings) y algunas otras strings que habrá que estudiar en detalle. 

#Comprobamos valores perdidos usando la función de @wences
perdidosTrain <- valoresPerdidos(train)
perdidosTest <- valoresPerdidos(test)


#Obtenemos los valores perdidos en un gráfico
plot(perdidosTrain$`Total de perdidos por variable`, col=perdidosTrain$`Total de perdidos por variable`)
plot(perdidosTest$`Total de perdidos por variable`, col=perdidosTest$`Total de perdidos por variable`)

#Tenemos bastante presencia de valores perdidos por lo que tendremos que solventar este problema antes de nada.


#Vamos a ver la presencia de outliers, nos centramos en un análisis básico univarite por IQR por lo que obtendremos los boxplots
#Primero escalamos y nos quedamos con solo los numéricos para ver los boxplot de mejor manera.

numericTrain<-train[,sapply(train, is.numeric)]
trainNumericScaled<-scale(numericTrain)

boxplot(numericTrain[1:30])
boxplot(numericTrain[31:69])

#Vemos que hay mucha presencia de outliers, estos valores corresponderán en muchos casos a ruido por lo que esamos ante un problema con ruido.


#Por último acorde al código del compañero @wences sabemos que estamos ante un problema de clases no balanceadas

ggplot2::ggplot()+
  geom_bar(aes(train[,76]), alpha=.8, fill='lightblue',color='pink') + 
  geom_hline(yintercept = length(train[,76])*0.25, alpha=.5, color='red') +
  geom_text(aes(0, sum(train[,76]==0), label = paste(round(sum(train[,76]==0)/length(train[,76])*100, 2), "%"), vjust = -0.2)) +
  geom_text(aes(1, sum(train[,76]==1), label = paste(round(sum(train[,76]==1)/length(train[,76])*100, 2), "%"), vjust = -0.2)) +
  geom_text(aes(2, sum(train[,76]==2), label = paste(round(sum(train[,76]==2)/length(train[,76])*100, 2), "%"), vjust = -0.2)) +
  geom_text(aes(3, sum(train[,76]==3), label = paste(round(sum(train[,76]==3)/length(train[,76])*100, 2), "%"), vjust = -0.2)) +
  geom_text(aes(0, length(train[,76])*0.25, label = "25 %", vjust = -0.2), color = "red")

#Esta característica del problema hará que tengamos que valorar ciertas técnicas de oversampling en un futuro. 

#***************************************
# GENERACIÓN DEL DATASET BASE:
# SIN VALORES PERDIDOS Y CON TODAS NUMERICAS
#***************************************


#Pasamos todo a numérico
#****************************************

TrainSinNumericas <- train
TestSinNumericas <- test

#Pasamos los "" de las categóricas a NA

TrainSinNumericas$x0[which(TrainSinNumericas$x0=="")]<-NA
TrainSinNumericas$x14[which(TrainSinNumericas$x14=="")]<-NA 
TrainSinNumericas$x17[which(TrainSinNumericas$x17=="")]<-NA 
TrainSinNumericas$x51[which(TrainSinNumericas$x51=="")]<-NA 
TrainSinNumericas$x61[which(TrainSinNumericas$x61=="")]<-NA 
TrainSinNumericas$x63[which(TrainSinNumericas$x63=="")]<-NA 

TestSinNumericas$x0[which(TestSinNumericas$x0=="")]<-NA
TestSinNumericas$x14[which(TestSinNumericas$x14=="")]<-NA 
TestSinNumericas$x17[which(TestSinNumericas$x17=="")]<-NA 
TestSinNumericas$x51[which(TestSinNumericas$x51=="")]<-NA 
TestSinNumericas$x61[which(TestSinNumericas$x61=="")]<-NA 
TestSinNumericas$x63[which(TestSinNumericas$x63=="")]<-NA 

#Pasamos a 0 1 en las variables binarias
TrainSinNumericas$x63<-ifelse(TrainSinNumericas$x63=="active",1,0)
TrainSinNumericas$x14<-ifelse(TrainSinNumericas$x14=="true",1,0)
TrainSinNumericas$x51<-ifelse(TrainSinNumericas$x51=="yes",1,0)

TestSinNumericas$x63<-ifelse(TestSinNumericas$x63=="active",1,0)
TestSinNumericas$x14<-ifelse(TestSinNumericas$x14=="true",1,0)
TestSinNumericas$x51<-ifelse(TestSinNumericas$x51=="yes",1,0)

#Eliminamos los strings iniciales y pasamos a numerico

TrainSinNumericas$x17<-substring(TrainSinNumericas$x17,4,5)
TrainSinNumericas$x61<-substring(TrainSinNumericas$x61,4,5)

TestSinNumericas$x17<-substring(TestSinNumericas$x17,4,5)
TestSinNumericas$x61<-substring(TestSinNumericas$x61,4,5)

#Tenemos una variable categórica que parece que indica tallas. La numerificaremos teniendo en cuenta este orden.

TrainSinNumericas$x0[TrainSinNumericas$x0=="L"]<-4
TrainSinNumericas$x0[TrainSinNumericas$x0=="M"]<-3
TrainSinNumericas$x0[TrainSinNumericas$x0=="S"]<-2
TrainSinNumericas$x0[TrainSinNumericas$x0=="VL"]<-5
TrainSinNumericas$x0[TrainSinNumericas$x0=="VS"]<-1

TestSinNumericas$x0[TestSinNumericas$x0=="L"]<-4
TestSinNumericas$x0[TestSinNumericas$x0=="M"]<-3
TestSinNumericas$x0[TestSinNumericas$x0=="S"]<-2
TestSinNumericas$x0[TestSinNumericas$x0=="VL"]<-5
TestSinNumericas$x0[TestSinNumericas$x0=="VS"]<-1

#Pasamos todo a enteros

TrainSinNumericas$x0  <- as.integer(TrainSinNumericas$x0)
TrainSinNumericas$x14 <- as.integer(TrainSinNumericas$x14)
TrainSinNumericas$x17 <- as.integer(TrainSinNumericas$x17)
TrainSinNumericas$x51 <- as.integer(TrainSinNumericas$x51)
TrainSinNumericas$x61 <- as.integer(TrainSinNumericas$x61 )
TrainSinNumericas$x63 <- as.integer(TrainSinNumericas$x63)

TestSinNumericas$x0  <- as.integer(TestSinNumericas$x0)
TestSinNumericas$x14 <- as.integer(TestSinNumericas$x14)
TestSinNumericas$x17 <- as.integer(TestSinNumericas$x17)
TestSinNumericas$x51 <- as.integer(TestSinNumericas$x51)
TestSinNumericas$x61 <- as.integer(TestSinNumericas$x61 )
TestSinNumericas$x63 <- as.integer(TestSinNumericas$x63)


#Imputamos valores perdidos usando KNN
#****************************************







#*****************************************
#Selección de caracteristicas
#*****************************************

#Borouta
#*****************************************

Boruta.train <- Boruta(y ~ ., data = train, doTrace = 2, ntree = 500)
Boruta.train
# Creamos un gráfico par ver como se ha comportado el proceso
plot(Boruta.train)

# Obtenemos las variables consideradas relevantes
getConfirmedFormula(Boruta.train)
variables.relevantes<-which(Boruta.train$finalDecision=="Confirmed")

trainEntrenar<-train[,c(variables.relevantes,75)]
testPredecir<-test[,c(variables.relevantes)]


#Evolutivos
#*****************************************
#Dado que son muy lentos los ejecutaremos en paraleo

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)

ctrl <- gafsControl(functions = rfGA,
                    method = "cv",
                    number = 3,
                    allowParallel = T)

rf_search <- gafs(x = train[, -ncol(train)],
                  y = as.factor(train$y),
                  iters = 3,
                  gafsControl = ctrl)

stopCluster(cl)

#No conseguimos que acabe la selección de variables usando evolutivos

