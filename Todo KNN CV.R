#############
# Funciones #
#############

# Cross-Validation propio para KNN

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

# Predicciones para KNN
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


###########
# Modelos #
###########

setwd("D:/kaggle/kaggle")

train <- read.csv2("Numericos_ImpKNN_SinCorr-TRAIN.csv", stringsAsFactors = FALSE)
test <- read.csv2("Numericos_ImpKNN_SinCorr-TEST.csv", stringsAsFactors = FALSE)

###########
# 1. Base #
###########


crossvalidation5(train) # 0.7155338
prediccionTest(train, test)


#################
# 1. Base Media #
#################

#train <- read.csv2("data/Numericos_SinCorr_Media-TRAIN.csv", stringsAsFactors = FALSE)
#test <- read.csv2("data/Numericos_SinCorr_Media-TEST.csv", stringsAsFactors = FALSE)

crossvalidation5(train) # 0.6407568
prediccionTest(train, test)

#################
# 1. Base AR    #
#################

#train <- read.csv2("data/Numericos_ImpRA_SinCorr-TRAIN.csv", stringsAsFactors = FALSE)
#test <- read.csv2("data/Numericos_ImpRA_SinCorr-TEST.csv", stringsAsFactors = FALSE)

crossvalidation5(train) # 0.6411204
prediccionTest(train, test)

###############
# 1. Base KNN #
###############

#train <- read.csv2("data/Numericos_ImpKNN_SinCorr-TRAIN.csv", stringsAsFactors = FALSE)
#test <- read.csv2("data/Numericos_ImpKNN_SinCorr-TEST.csv", stringsAsFactors = FALSE)

crossvalidation5(train) # 0.6400181
prediccionTest(train, test)

##########################
# 2. Selección variables #
##########################
# A través de Random Forest obtengo un vecto ordenado en función de la importancia
# de las variables que intervienen en él

library(randomForest)
rm <-randomForest::randomForest(train[,-75], as.factor(train[,75]))
importance <- caret::varImp(rm, scale=FALSE)
var <- order(importance[[1]])

crossvalidation5(train[,-var[1:25]]) # 0.7983896
prediccionTest(train[,-var[1:25]], test)

######################
#  3. Base + Boruta #
######################
train <- read.csv2("Numericos_ImpKNN_SinCorr-Boruta-TRAIN.csv")
test <- read.csv2("Numericos_ImpKNN_SinCorr-Boruta-TEST.csv")

crossvalidation5(train) #0.8075465
prediccionTest(train, test)


#######################
# 4. Boruta + IPF #
#######################


train$y <- as.factor(train$y)
train_bo_ipf <- NoiseFiltersR::IPF(train, s=3)
train_bo_ipf <- train_bo_ipf$cleanData

crossvalidation5(train_bo_ipf) # 0.8952831
prediccionTest(train_bo_ipf,test)

##########
# 5. IPF #
##########
train <- read.csv2("Numericos_ImpKNN_SinCorr-TRAIN.csv", stringsAsFactors = FALSE)
test <- read.csv2("Numericos_ImpKNN_SinCorr-TEST.csv", stringsAsFactors = FALSE)

trainIPF <- train
install.packages("NoiseFiltersR")
library(NoiseFiltersR)
# Factorizo la clase para aplicarle IPF
trainIPF[,75] <- factor(trainIPF[,75])
# Le quito las instancias con ruido
# indico que pare a la segunda que no encuentre y que lo haga mediante consenso
trainIPF <- IPF(y ~., trainIPF, consensus = TRUE, s=2)
# Me quedo unicamente con el data.frame limpio
trainIPF <- trainIPF$cleanData

crossvalidation5(trainIPF[,-var[1:25]]) # 0.8175613
prediccionTest(trainIPF[,-var[1:25]], test)


############
# 6. SMOTE #
############

trainSMOTE <- train
install.packages("unbalanced")
library(unbalanced)
# Aplico SMOTE sobre la clase 0 respecto al resto
instanciasSmote <- unbalanced::ubSMOTE(trainSMOTE[,-75], factor(as.integer(I(train[,75]==0))), perc.over = 200)
# Se unen todas las variables con la clase
instanciasSmote <-cbind(instanciasSmote[[1]], instanciasSmote[[2]])
# Me quedo con las instancias sintéticas para la clase minoritaria 0 que está marcadas como 1
instanciasSmote <- instanciasSmote[which(instanciasSmote[,75]==1),]
# Las convierto a 0
instanciasSmote[,75] <- 0
# Cambio el nombre de la clase a y
names(instanciasSmote)[75] <- "y"
# Añado los SMOTE a train
trainSMOTE <- rbind(trainSMOTE, instanciasSmote)

crossvalidation5(trainSMOTE[,-var[1:25]]) # 0,8503597
prediccionTest(trainSMOTE[,-var[1:25]], test)

##################
# 7. IPF + SMOTE #
##################

trainIPF_SMOTE <- train

# Factorizo la clase para aplicarle IPF
trainIPF_SMOTE[,75] <- factor(trainIPF_SMOTE[,75])
# Le quito las instancias con ruido
# indico que pare a la segunda que no encuentre y que lo haga mediante consenso
trainIPF_SMOTE <- IPF(y ~., trainIPF_SMOTE, consensus = TRUE, s=2)
# Me quedo unicamente con el data.frame limpio
trainIPF_SMOTE <- trainIPF_SMOTE$cleanData

# Aplico SMOTE sobre la clase 0 respecto al resto
instanciasSmote <- unbalanced::ubSMOTE(trainIPF_SMOTE[,-75], factor(as.integer(I(trainIPF_SMOTE[,75]==0))), perc.over = 200)
# Se unen todas las variables con la clase
instanciasSmote <-cbind(instanciasSmote[[1]], instanciasSmote[[2]])
# Me quedo con las instancias sintéticas para la clase minoritaria 0 que está marcadas como 1
instanciasSmote <- instanciasSmote[which(instanciasSmote[,75]==1),]
# Las convierto a 0
instanciasSmote[,75] <- 0
# Cambio el nombre de la clase a y
names(instanciasSmote)[75] <- "y"
# Añado los SMOTE a train
trainIPF_SMOTE <- rbind(trainIPF_SMOTE, instanciasSmote)

crossvalidation5(trainIPF_SMOTE[,-var[1:25]]) # 0.8624823
prediccionTest(trainIPF_SMOTE[,-var[1:25]], test)

##################
# 8. SMOTE + IPF #
##################

trainSMOTE_IPF <- train

# Aplico SMOTE sobre la clase 0 respecto al resto
instanciasSmote <- unbalanced::ubSMOTE(trainSMOTE_IPF[,-75], factor(as.integer(I(trainSMOTE_IPF[,75]==0))), perc.over = 200)
# Se unen todas las variables con la clase
instanciasSmote <-cbind(instanciasSmote[[1]], instanciasSmote[[2]])
# Me quedo con las instancias sintéticas para la clase minoritaria 0 que está marcadas como 1
instanciasSmote <- instanciasSmote[which(instanciasSmote[,75]==1),]
# Las convierto a 0
instanciasSmote[,75] <- 0
# Cambio el nombre de la clase a y
names(instanciasSmote)[75] <- "y"
# Añado los SMOTE a train
trainSMOTE_IPF <- rbind(trainSMOTE_IPF, instanciasSmote)

# Factorizo la clase para aplicarle IPF
trainSMOTE_IPF[,75] <- factor(trainSMOTE_IPF[,75])
# Le quito las instancias con ruido
# indico que pare a la segunda que no encuentre y que lo haga mediante consenso
trainSMOTE_IPF <- IPF(y ~., trainSMOTE_IPF, consensus = TRUE, s=2)
# Me quedo unicamente con el data.frame limpio
trainSMOTE_IPF <- trainSMOTE_IPF$cleanData

crossvalidation5(trainSMOTE_IPF[,-var[1:25]]) # 0.8443521
prediccionTest(trainSMOTE_IPF[,-var[1:25]], test)


##################
# 9. Tomek Links #
##################

trainTL <- train

# Busco las instancias Tomek Links
instanciasTL<- unbalanced::ubTomek(trainTL[,-75], (trainTL[,75]==0))
# Elimino esas instancias de train
trainTL <- trainTL[-instanciasTL$id.rm,]

crossvalidation5(trainTL[,-var[1:25]]) # 0.8069632
prediccionTest(trainTL[,-var[1:25]], test)

##########################
# 10. Tomek Links + SMOTE #
##########################

trainTL_SMOTE <- train

# Busco las instancias Tomek Links
instanciasTL<- unbalanced::ubTomek(trainTL_SMOTE[,-75], (trainTL_SMOTE[,75]==0))
# Elimino esas instancias de train
trainTL_SMOTE <- trainTL_SMOTE[-instanciasTL$id.rm,]

# Aplico SMOTE sobre la clase 0 respecto al resto
instanciasSmote <- unbalanced::ubSMOTE(trainTL_SMOTE[,-75], factor(as.integer(I(trainTL_SMOTE[,75]==0))), perc.over = 200)
# Se unen todas las variables con la clase
instanciasSmote <-cbind(instanciasSmote[[1]], instanciasSmote[[2]])
# Me quedo con las instancias sintéticas para la clase minoritaria 0 que está marcadas como 1
instanciasSmote <- instanciasSmote[which(instanciasSmote[,75]==1),]
# Las convierto a 0
instanciasSmote[,75] <- 0
# Cambio el nombre de la clase a y
names(instanciasSmote)[75] <- "y"
# Añado los SMOTE a train
trainTL_SMOTE <- rbind(trainTL_SMOTE, instanciasSmote)

crossvalidation5(trainTL_SMOTE[,-var[1:25]]) # 0.8481349
prediccionTest(trainTL_SMOTE[,-var[1:25]], test)


##########################
# 11. SMOTE + Tomek Links #
##########################

trainSMOTE_TL <- train

# Aplico SMOTE sobre la clase 0 respecto al resto
instanciasSmote <- unbalanced::ubSMOTE(trainSMOTE_TL[,-75], factor(as.integer(I(trainSMOTE_TL[,75]==0))), perc.over = 200)
# Se unen todas las variables con la clase
instanciasSmote <-cbind(instanciasSmote[[1]], instanciasSmote[[2]])
# Me quedo con las instancias sintéticas para la clase minoritaria 0 que está marcadas como 1
instanciasSmote <- instanciasSmote[which(instanciasSmote[,75]==1),]
# Las convierto a 0
instanciasSmote[,75] <- 0
# Cambio el nombre de la clase a y
names(instanciasSmote)[75] <- "y"
# Añado los SMOTE a train
trainSMOTE_TL <- rbind(trainSMOTE_TL, instanciasSmote)

# Busco las instancias Tomek Links
instanciasTL<- unbalanced::ubTomek(trainSMOTE_TL[,-75], (trainSMOTE_TL[,75]==0))
# Elimino esas instancias de train
trainSMOTE_TL <- trainSMOTE_TL[-instanciasTL$id.rm,]

crossvalidation5(trainSMOTE_TL[,-var[1:25]]) # 0.8527825  
prediccionTest(trainSMOTE_TL[,-var[1:25]], test)


###########
# 12. ROS #
###########

trainROS <- train
install.packages("ROSE")
library(ROSE)
# Factorizo la clase
trainROS[,75] <- factor(trainROS[,75])
# Modifico los levels de la clase para aplicar ROS
levels(trainROS[,75]) <- c(0,1,1,1)
instanciasROS <- ROSE::ovun.sample(y ~., trainROS, method = "over")
# Añado a train las instancias ROS de la clase 0
trainROS <- rbind(train, instanciasROS$data[which(instanciasROS$data[,75]==0),])

crossvalidation5(trainROS[,-var[1:25]]) # 0.9134734
prediccionTest(trainROS[,-var[1:25]], test)



#################
# 13. ROS + IPF #
#################

trainROS_IPF <- train

# Factorizo la clase
trainROS_IPF[,75] <- factor(trainROS_IPF[,75])
# Modifico los levels de la clase para aplicar ROS
levels(trainROS_IPF[,75]) <- c(0,1,1,1)
instanciasROS <- ROSE::ovun.sample(y ~., trainROS_IPF, method = "over")
# Añado a train las instancias ROS de la clase 0
trainROS_IPF <- rbind(train, instanciasROS$data[which(instanciasROS$data[,75]==0),])

# Factorizo la clase para aplicarle IPF
trainROS_IPF[,75] <- factor(trainROS_IPF[,75])
# Le quito las instancias con ruido
# indico que pare a la segunda que no encuentre y que lo haga mediante consenso
trainROS_IPF <- IPF(y ~., trainROS_IPF, consensus = TRUE, s=2)
# Me quedo unicamente con el data.frame limpio
trainROS_IPF <- trainROS_IPF$cleanData

crossvalidation5(trainROS_IPF[,-var[1:25]]) # 0.919891
prediccionTest(trainROS_IPF[,-var[1:25]], test)


#############
# 14. ROS 2 #
#############

train_ROS2 <- train

# Hago ROS de la clase 0 con cada una del resto de manera individual y prestando
# más atención a la clase 1
train_ROS2_1 <- train[which(train_ROS2[,75]==0|train_ROS2[,75]==1),]
train_ROS2_1 <- ROSE::ovun.sample(y ~., train_ROS2_1, method = "over", p=0.5)
train_ROS2_1 <- train_ROS2_1$data
levels(train_ROS2_1) <- c(0,1)

train_ROS2_2 <- train[which(train_ROS2[,75]==0|train_ROS2[,75]==2),]
train_ROS2_2 <- ROSE::ovun.sample(y ~., train_ROS2_2, method = "over", p=0.3)
train_ROS2_2 <- train_ROS2_2$data
levels(train_ROS2_2) <- c(0,2)

train_ROS2_3 <- train[which(train_ROS2[,75]==0|train_ROS2[,75]==1),]
train_ROS2_3 <- ROSE::ovun.sample(y ~., train_ROS2_3, method = "over", p=0.3)
train_ROS2_3 <- train_ROS2_3$data
levels(train_ROS2_3) <- c(0,3)

train_ROS2 <- rbind(train, train_ROS2_1, train_ROS2_2, train_ROS2_3)

crossvalidation5(train_ROS2[,-var[1:25]]) # 0.9591007
prediccionTest(train_ROS2[,-var[1:25]], test)


###################
# 15. ROS 2 + IPF #
###################

train_ROS2_IPF <- train

# Hago ROS de la clase 0 con cada una del resto de manera individual y prestando
# más atención a la clase 1
train_ROS2_1 <- train[which(train_ROS2_IPF[,75]==0|train_ROS2_IPF[,75]==1),]
train_ROS2_1 <- ROSE::ovun.sample(y ~., train_ROS2_1, method = "over", p=0.5)
train_ROS2_1 <- train_ROS2_1$data
levels(train_ROS2_1) <- c(0,1)

train_ROS2_2 <- train[which(train_ROS2_IPF[,75]==0|train_ROS2_IPF[,75]==2),]
train_ROS2_2 <- ROSE::ovun.sample(y ~., train_ROS2_2, method = "over", p=0.3)
train_ROS2_2 <- train_ROS2_2$data
levels(train_ROS2_2) <- c(0,2)

train_ROS2_3 <- train[which(train_ROS2_IPF[,75]==0|train_ROS2_IPF[,75]==1),]
train_ROS2_3 <- ROSE::ovun.sample(y ~., train_ROS2_3, method = "over", p=0.3)
train_ROS2_3 <- train_ROS2_3$data
levels(train_ROS2_3) <- c(0,3)

train_ROS2_IPF <- rbind(train, train_ROS2_1, train_ROS2_2, train_ROS2_3)

# Factorizo la clase para aplicarle IPF
train_ROS2_IPF[,75] <- factor(train_ROS2_IPF[,75])
# Le quito las instancias con ruido
# indico que pare a la segunda que no encuentre y que lo haga mediante consenso
train_ROS2_IPF <- IPF(y ~., train_ROS2_IPF, consensus = TRUE, s=2)
# Me quedo unicamente con el data.frame limpio
train_ROS2_IPF <- train_ROS2_IPF$cleanData

crossvalidation5(train_ROS2_IPF[,-var[1:25]]) # 0,9641174
prediccionTest(train_ROS2_IPF[,-var[1:25]], test)


###########
# 16. ENN #
###########

trainENN <- train

instanciasENN <- unbalanced::ubENN(trainENN[,-75], (trainENN[,75]==0))
trainENN <- trainENN[-instanciasENN$id.rm,]

crossvalidation5(trainENN[,-var[1:25]]) # 0.8471805
prediccionTest(trainENN[,-var[1:25]], test)


##################
# 17. Duplicar 0 #
##################

trainROS_Propio <- train

trainROS_Propio <- rbind(trainROS_Propio, train[which(trainROS_Propio[,75]==0),])

crossvalidation5(trainROS_Propio[,-var[1:25]]) # 0.8370848
prediccionTest(trainROS_Propio[,-var[1:25]], test)


#############################
# 18. Duplicar 0 + Random 1 #
#############################

trainROS_Propio <- train

trainROS_Propio <- rbind(trainROS_Propio, train[which(trainROS_Propio[,75]==0),])
trainROS_Propio <- rbind(trainROS_Propio, train[sample(which(trainROS_Propio[,75]==1), length(which(trainROS_Propio[,75]==1))/2.5),]) 

crossvalidation5(trainROS_Propio[,-var[1:25]]) # 0.8604651
prediccionTest(trainROS_Propio[,-var[1:25]], test)


##############
# Descartada #
##############

trainDescartada <- train

trainDescartada <- rbind(trainDescartada, train[which(trainDescartada[,75]==0),])
trainDescartada <- rbind(trainDescartada, train[sample(which(trainDescartada[,75]==1), length(which(trainDescartada[,75]==1))/2.5),]) 

# Predigo sobre test y asigno las predicciones de la clase obtenidas a test
retro <- prediccionTest(trainDescartada[,-var[1:25]], test)
retro <- cbind(test, y=retro)


# Factorizo la clase para aplicarle IPF
retro[,75] <- factor(retro[,75])
# Le quito las instancias con ruido
# indico que pare a la segunda que no encuentre y que lo haga mediante consenso
retro <- IPF(y ~., retro, s=2, nfolds=5, consensus=TRUE, p=0.01)
# Me quedo unicamente con el data.frame limpio
retro <- retro$cleanData

# Uno train y test
descartada <- rbind(trainDescartada, retro)

crossvalidation5(descartada[,-var[1:25]]) # 0.8722105 0.8680094  0.8769325
prediccionTest(descartada[,-var[1:25]],test)
