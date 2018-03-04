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

#**************************************
#Eliminación correlaciones: Sabemos esta correlación por el gráfico de wences
#**************************************

# Relleno los NAs de x41 y x48 ya que son variables correlacionadas
dif<-train[1,42]/train[1,49]
ins <- which(is.na(train[,42]))
train[ins,42] <- dif*train[ins,49]

dif<-test[1,42]/test[1,49]
ins <- which(is.na(test[,42]))
test[ins,42] <- dif*test[ins,49]

# Elimino la x48 ya que con una de las dos es suficiente
train <- train[,-49]
test <- test[,-49]

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


imputacionKnnTotal <- function(x){
  #' Se incluye dentro una primera función imputacionKnn
  
  imputacionKnn <- function(x, y){
    #' Se le pasa una matriz o data.frame x y el índice de una de sus columnas
    #' Busca en la columna las instancias perdidas y para esas las variables que tienen valores perdidos de cara 
    #' a que el test no de fallos ya que como no hay más de un valor perdido por instancia será como
    #' máximo el mismo número de instancias perdidas.
    #' Se buscan las instancias con valores perdidos para dichas variables y se omiten para el train,
    #' estando entre ellas evidentemente las que vamos a predecir.
    #' Se construyen train y test y se entrena con K-NN, usando CV y 10 repeticiones, el resultado
    #' es la matriz o data.frame original pero con dicha columna modificada.
    
    require(caret)
    
    # Instancia perdida de la columna
    instanciasPerdidas <- which(is.na(x[,y])|x[,y]=="")
    # Otras variables con datos perdidos en dichas instancias
    variablesPerdidas <- which(sapply((1:dim(x)[2])[-y], function(z) any(is.na(x[instanciasPerdidas,z]) | x[instanciasPerdidas,z]=="")))
    
    # Búsqueda de instancias con perdidos obviando, en caso de que estén, aquellas variables descartadas
    if(length(variablesPerdidas)!=0){
      instanciasX <- sapply(1:dim(x)[1], function(z) sum(is.na(x[z,-variablesPerdidas]))+sum(x[z,-variablesPerdidas]=="", na.rm = TRUE))
    } else {
      instanciasX <- sapply(1:dim(x)[1], function(z) sum(is.na(x[z,]))+sum(x[z,]=="", na.rm = TRUE))
    }
    
    # Quedarme con los índices de las instancias con perdidos
    instanciasX <- which(instanciasX!=0)
    
    if(length(variablesPerdidas)!=0){
      train <- x[-instanciasX, -c(y,variablesPerdidas)]
      test <- x[instanciasPerdidas, -c(y,variablesPerdidas)]
    } else {
      train <- x[-instanciasX,-y]
      test <- x[instanciasPerdidas,-y]
    }
    
    train.class <- x[-instanciasX,y]
    
    
    variablesNumericas <- which(sapply(1:dim(train)[2], function(z) is.numeric(train[,z])))
    # Elimino la clase en caso de que esté entre ellas
    variablesNumericas <- variablesNumericas[!variablesNumericas==y]
    
    modelo <- caret::train(train[,variablesNumericas], train.class,
                           method = "knn",
                           tuneLength = 10,
                           trControl = trainControl(method = "cv"))
    
    modelo.predict <- predict(modelo,test[,variablesNumericas])
    if(is.factor(modelo.predict)){
      x[instanciasPerdidas,y] <- as.character(modelo.predict)
    } else {
      x[instanciasPerdidas,y] <- modelo.predict
    }
    
    x
  }
  #' Segunda parte de la función:
  #' Le paso todas las varaibles perdidas a la función anterior
  #' para ello voy pasando el mismo data.frame, con los datos originales en cada iteración
  #' y en cada una de ellas voy sustituyendo en una copia del dataframe los perdidos con
  #' las imputaciones.
  
  y <- x
  variablesPerdidas <- which(sapply((1:dim(x)[2]), function(z) any(is.na(x[,z]) | x[,z]=="")))
  for(i in variablesPerdidas){
    print(i)
    n <- imputacionKnn(x,i)
    y[,i] <- n[,i]
  }
  y
}

# Aplico la función, que tiene en cuenta tanto numeros como caracteres
train <- imputacionKnnTotal(train)
test <- imputacionKnnTotal(test)

#Esccribimos en datos

write.csv2(train, "data/Numericos_ImpKNN_SinCorr-TRAIN.csv", row.names = FALSE)
write.csv2(test, "data/Numericos_ImpKNN_SinCorr-TEST.csv", row.names = FALSE)

#*****************************************
# Creamos más datasets con imputaciones
#*****************************************


# TIPO 2: IMPUTACIÓN POR NORM

# Copiamos los datos

TrainSinNumericasMVNorm <- TrainSinNumericas
TestSinNumericasMVNorm <- TestSinNumericas

# Imputamos por norm

mice_mod <- mice(TrainSinNumericasMVNorm[,colnames(TrainSinNumericasMVNorm)], method='norm')
TrainSinNumericasMVNorm <- complete(mice_mod)
mice_mod <- mice(TestSinNumericasMVNorm[,colnames(TestSinNumericasMVNorm)], method='norm')
TestSinNumericasMVNorm <- complete(mice_mod)

TrainSinNumericasMVNorm <- TrainSinNumericasMVNorm[,-42]
TestSinNumericasMVNorm  <- TestSinNumericasMVNorm[,-42]

write.csv2(TrainSinNumericasMVNorm, "data/trainSinNumericosMVNorm.csv", row.names = FALSE)
write.csv2(TestSinNumericasMVNorm, "data/testSinNumericosMVNorm.csv",row.names = FALSE)


# TIPO 3: IMPUTACIÓN POR MEDIA

TrainSinNumericasMVQuadratic <- TrainSinNumericas
TestSinNumericasMVQuadratic <- TestSinNumericas

# Imputamos por norm

mice_mod <- mice(TrainSinNumericasMVQuadratic[,colnames(TrainSinNumericasMVQuadratic)], method='quadratic')
TrainSinNumericasMVQuadratic <- complete(mice_mod)
mice_mod <- mice(TestSinNumericasMVQuadratic[,colnames(TestSinNumericasMVQuadratic)], method='quadratic')
TestSinNumericasMVQuadratic <- complete(mice_mod)


TrainSinNumericasMVQuadratic <- TrainSinNumericasMVQuadratic[,-42]
TestSinNumericasMVQuadratic  <- TestSinNumericasMVQuadratic[,-42]

write.csv2(TrainSinNumericasMVQuadratic, "data/trainSinNumericosMVQuadratic.csv", row.names = FALSE)
write.csv2(TestSinNumericasMVQuadratic, "data/testSinNumericosMVQuadratic.csv", row.names = FALSE)



#*****************************************
#Selección de caracteristicas
#*****************************************

#Volvemos a obtener los datos para evitar problemas en la anterior generacion


Train <- read.table("data/my_dataset_train.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)
Test <- read.table("data/my_dataset_test.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)

#Guardamos una copia para trabajar

train<-Train
test<-Test


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



#*****************************************
#Función de clasificación
#*****************************************


#Esta función ha sido usada paras las diversas configuraciones de prepreocesado usadas. 

validacion<-function(train, test, nombre)
{
  control <- trainControl(method = "cv", number = 5, returnResamp = "all", search = "random")
  
  train$y<-as.factor(train$y)
  
  trainModelRpart <- caret::train(y ~.,  data = train, 
                                  method = "rpart", 
                                  trControl = control,
                                  preProc = c("center", "scale"))
  
  modelo.predict0 <- caret::predict.train(trainModelRpart, test, type="raw")
  predicciones <- as.integer(as.character(modelo.predict0))
  
  predicciones <- matrix(c(1:683,predicciones),683,2,byrow = FALSE)
  colnames(predicciones) <- c("Id","Prediction")
  write.table(predicciones, nombre, col.names = TRUE, row.names = FALSE,
              quote=FALSE, sep = ",")
  return(max(trainModelRpart$results$Accuracy))
}


#*******************************************
#Votación por mayoria
#*******************************************

#Votación por mayoría de todos los scripts de kaggle: La idea es obtener las mejores solucioenes de kaggle, combinarlas y por mayoria asignar la clase



#**********************************************
# Lectura de ficheros
#**********************************************

#Leemos las distintas predicciones de test
setwd("./sub083omas")
temp = list.files(pattern="*.csv")
#Leemos los ficheros
myfiles = lapply(temp, read.delim, sep=",")
setwd("../")


#**********************************************
# Creación del dataframe
#**********************************************
myfiles[[1]]$Prediction
matriz.predicciones<-1:683

for(i in 1:length(myfiles))
{
  matriz.predicciones<-cbind(matriz.predicciones,myfiles[[i]]$Prediction)
}
matriz.predicciones<-as.data.frame(matriz.predicciones[,2:33])


#**********************************************
# Obtencion de votaciones
#**********************************************

#Ahora creamos una lista con las votaciones de cada elemento

lista.votaciones<-apply(matriz.predicciones,MARGIN=1,table)

#Recorremos la lista y nos quedamos con la mayoritaria

resultado.votacion<-vector()

for(i in 1:length(lista.votaciones))
{
  resultado.votacion[i]<-names(which.min(lista.votaciones[[i]]))
}

resultado.votacion

predicciones <- as.integer(resultado.votacion)

# Construyo la matriz de predicciones
predicciones <- matrix(c(1:683,predicciones),683,2,byrow = FALSE)
colnames(predicciones) <- c("Id","Prediction")
# Guardo
write.table(predicciones, "sub/votacion-minoria.csv", col.names = TRUE, row.names = FALSE,
            quote=FALSE, sep = ",")


