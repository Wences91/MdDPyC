#############
# FUNCIONES #
#############
# Adaptacion de a funcion de Cross-Validation de Wenceslao de GLM para KNN

crossvalidation5 <- function(train){
  set.seed(2)
  
  datos<-train[sample(nrow(train)),]
  
  # Creo 5 folds
  folds <- cut(seq(1,nrow(train)),breaks=5,labels=FALSE)
  
  # CV para cada fold
  precision <- mean(sapply(1:5, function(x){
    
    indices <- which(folds==x,arr.ind=TRUE)
    test <- datos[indices, ]
    train <-datos[-indices, ]
    
    # A partir de aqui es especifico de mi modelo
    modelo <- caret::train(train[,-dim(train)[2]], train$y,
                           method = "knn",
                           preProcess = c("center","scale"),
                           tuneGrid = data.frame(.k=1))
    
    
    modelo.predict0 <- caret::predict.train(modelo, test, type="raw")
    predicciones <- as.integer(as.character(modelo.predict0))
    
    return(mean(predicciones==test[,dim(test)[2]]))
    
  }))
  
  return(precision)
  
}

# Adaptacion de la funcion predicctionTest de Wenceslao de GLM para KNN
prediccionTest <- function(train, test){
  # Se le pasa un train y test, entrena con el primero y predice sobre el segundo
  
  set.seed(2)
  
  modelo <- caret::train(train[,-dim(train)[2]], train$y,
                         method = "knn",
                         preProcess = c("center","scale"),
                         tuneGrid = data.frame(.k=1))
  
  
  modelo.predict0 <- caret::predict.train(modelo, test, type="raw")
  predicciones <- as.integer(as.character(modelo.predict0))
  
  # Guardo aparte las predicciones para devolverlas al final
  resultados <- predicciones
  
  # Creo un tabla de predicciones para subirlo a Kaggle
  predicciones <- matrix(c(1:length(predicciones),predicciones), length(predicciones), 2,byrow = FALSE)
  colnames(predicciones) <- c("Id","Prediction")
  
  # Guardo
  write.table(predicciones, "sample.csv", col.names = TRUE, row.names = FALSE,
              quote=FALSE, sep = ",")
  
  return(resultados)
}



##################
# METODOS Alvaro #
##################

# Muchas de las funciones que se invocan en este codigo han sido realizadas por mis compañeros
# por lo tanto, para su ejecucion es necesario declararlas antes

################################################################
# Solo Numericos Media Funcion Propia / Eliminando categoricos #
################################################################

Train <- read.csv2("my_dataset_train.csv", sep=",", dec=".", stringsAsFactors = FALSE)
Test <- read.csv2("my_dataset_test.csv", sep=",", dec=".", stringsAsFactors = FALSE)

# Compruebo si tiene valores perdidos
valoresPerdidos(Train)
valoresPerdidos(Test)

#Ambos tienen valores perdidos
# Imputo los valores numéricos de Train y Test
train<-imputacionNumericos(Train)
test<-imputacionNumericos(Test)

#Elimino los variables categóricas
vc<-which(sapply(train, is.character))
Nctrain<-train[,-vc]
Nctest<-test[,-vc]

#Vuelvo a comprobar los valores perdidos y obtengo 0
valoresPerdidos(Nctrain)
valoresPerdidos(Nctest)

# Aplico cv
crossvalidation5(Nctrain) # 0.6986692
prediccionTest(Nctrain,Nctest) # kaggle 0.71532

##################################
# Imputacion KNN sin categoricas #
##################################
setwd("D:/kaggle/kaggle")
train <- read.csv2("imputacionKnn-Train.csv")
vc<-which(sapply(train, is.numeric)==TRUE)

crossvalidation5(train[,vc]) # 0.6961024



#############################
# Base KNN + Boruta + SMOTE #
#############################
setwd("D:/kaggle/kaggle")

train <- read.csv2("Numericos_ImpKNN_SinCorr-Boruta-TRAIN.csv")
test <- read.csv2("Numericos_ImpKNN_SinCorr-Boruta-TEST.csv")

require(unbalanced)
smote0 <- unbalanced::ubSMOTE(train[,-56], factor(as.integer(I(train[,56]==0))), perc.over = 200)
smote0 <-cbind(smote0[[1]], smote0[[2]])
smote0 <- smote0[which(smote0[,56]==1),]

smote0[,56] <- 0

names(smote0)[56] <- "y"

trainSMOTE <- rbind(train, smote0)
crossvalidation5(trainSMOTE) # 0.8509353


############################
#  Media Moda y Numerifico #
############################

Train <- read.csv2("my_dataset_train.csv", sep=",", dec=".", stringsAsFactors = FALSE)
Test <- read.csv2("my_dataset_test.csv", sep=",", dec=".", stringsAsFactors = FALSE)

# Compruebo si tiene valores perdidos
valoresPerdidos(Train)
valoresPerdidos(Test)

#Ambos tienen valores perdidos
# Imputo los valores numéricos de Train y Test
train<-imputacionNumericos(Train)
test<-imputacionNumericos(Test)

train<-imputacionCaracteres(train)
test<-imputacionCaracteres(test)

# Compruebo si tiene valores perdidos
valoresPerdidos(train)
valoresPerdidos(test)


which(sapply(train, is.character)==TRUE)

##################################################################
# "Numerifico"  siguiendo el metodo realizado por mis compañeros #
##################################################################

# x0
train[which(train[,1]=="VS"), 1] <- 1
train[which(train[,1]=="S"), 1] <- 2
train[which(train[,1]=="M"), 1] <- 3
train[which(train[,1]=="L"), 1] <- 4
train[which(train[,1]=="VL"), 1] <- 5
train[,1] <- as.integer(train[,1])


test[which(test[,1]=="VS"), 1] <- 1
test[which(test[,1]=="S"), 1] <- 2
test[which(test[,1]=="M"), 1] <- 3
test[which(test[,1]=="L"), 1] <- 4
test[which(test[,1]=="VL"), 1] <- 5
test[,1] <- as.integer(test[,1])

#x14
train[which(train[,15]=="true"), 15] <- 1
train[which(train[,15]=="false"), 15] <- 0
train[,15] <- as.integer(train[,15])

test[which(test[,15]=="true"), 15] <- 1
test[which(test[,15]=="false"), 15] <- 0
test[,15] <- as.integer(test[,15])

#x17
train[,18] <- as.integer(substr(train[,18], 4, 4))

test[,18] <- as.integer(substr(test[,18], 4, 4))

#x51
train[which(train[,52]=="yes"), 52] <- 1
train[which(train[,52]=="no"), 52] <- 0
train[,52] <- as.integer(train[,52])

test[which(test[,52]=="yes"), 52] <- 1
test[which(test[,52]=="no"), 52] <- 0
test[,52] <- as.integer(test[,52])

#x61
train[,62] <- as.integer(substr(train[,62], 4, 5))

test[,62] <- as.integer(substr(test[,62], 4, 5))

#x63
train[which(train[,64]=="active"), 64] <- 1
train[which(train[,64]=="inactive"), 64] <- 0
train[,64] <- as.integer(train[,64])

test[which(test[,64]=="active"), 64] <- 1
test[which(test[,64]=="inactive"), 64] <- 0
test[,64] <- as.integer(test[,64])

which(sapply(test, is.character)==TRUE)

crossvalidation5(train) # 0.7060033


#####################
# Media Moda Boruta #
#####################

require(Boruta)

trainB <- Boruta(y ~ ., data = train, doTrace = 2, ntree = 500)

# Recuperamos las variables

getConfirmedFormula(trainB)
vr<-which(trainB$finalDecision=="Confirmed")

#Creamos nuestros train y test con las variables seleccionadas
trainBoruta<-train[,c(vr,76)]
testBoruta<-test[,c(vr)]

crossvalidation5(trainBoruta) #0.8002137


#############################
# Media Moda + Boruta + IPF #
#############################
require(Boruta)
require(NoiseFiltersR)

trainBIPF <- train
trainBIPF[,76] <- factor(trainBIPF[,76])
# Aplico IPF
trainBIPF <- IPF(y ~., trainBIPF, consensus = TRUE, s=2)

trainBIPF <- trainBIPF$cleanData
crossvalidation5(trainBIPF) # 0.7294227


######################################
# Media Moda + Boruta + IPF + Boruta #
######################################

#Vuelvo a aplicar Boruta

trainBIPFB <- Boruta(y ~ ., data = trainBIPF, doTrace = 2, ntree = 500)

# Recuperamos las variables

getConfirmedFormula(trainBIPFB)
vr<-which(trainBIPFB$finalDecision=="Confirmed")

#Creamos nuestros train y test con las variables seleccionadas
trainF<-train[,c(vr,76)]
testF<-test[,c(vr)]
crossvalidation5(trainF) # 0.7910515
