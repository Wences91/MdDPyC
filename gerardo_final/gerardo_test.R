#partimos de los datos númericos, sin correlación y cuyos valores perdidos
# se han imputado usando knn
train <- read.csv2("data/Numericos_ImpKNN_SinCorr-TRAIN.csv",
                    stringsAsFactors = FALSE);
test <- read.csv2("data/Numericos_ImpKNN_SinCorr-TEST.csv",
                   stringsAsFactors = FALSE);

train_base <- read.csv2("data/Numericos_SinCorr-TRAIN.csv")
test_base <- read.csv2("data/Numericos_SinCorr-TEST.csv")

##############
# CV 5-Folds #
##############
source("./funciones_de_clasificacion.R")



###
# 
##


###########
# Modelos #
###########

###########
# 1. Base (imputacion media) #
###########
#source("funciones_de_clasificacion.R")
#imputamos los valores perdidos con el valor medio usando la función correspondiente 
#train1_base <- imputacionValoresPerdidos(train_base)
#y <- train1_base$y
#train1_base$y<-NULL
#train1_base <- cbind(y, train1_base)

#test1_base <- imputacionValoresPerdidos(test_base)
#write.csv2(train1_base, "data/Numericos_SinCorr_Media-TRAIN.csv", row.names = FALSE)
#write.csv2(test1_base, "data/Numericos_SinCorr_Media-TEST.csv", row.names = FALSE)

# partimos de los valores guardados
train_media <- read.csv2("data/Numericos_SinCorr_Media-TRAIN.csv");
test_media <- read.csv2("data/Numericos_SinCorr_Media-TEST.csv");

crossvalidation5(train_media) # 0.5359196
prediccionTest(train_media, test_media,"01.sample_imputacion_media.csv")

###########
# 1. Base (imputación RF) #
###########
#imputación de valores pedido con randomforest
#library(randomForest)
#train2_base <- rfImpute(y ~ . , train_base)
#colnames(test_base)
#x71 no tiene na
#test2_base <- rfImpute( x71 ~ . , test_base)
#y <-test2_base$y
#test2_base$y<-NULL
#test2_base <- cbind(test2_base,y)
#write.csv2(train2_base, "data/Numericos_ImpRA_SinCorr-TRAIN.csv", row.names = FALSE)
#write.csv2(test2_base, "data/Numericos_ImpRA_SinCorr-TEST.csv", row.names = FALSE)

#partimos de los valores guardados
train_ra <- read.csv2("data/Numericos_ImpRA_SinCorr-TRAIN.csv");
test_ra <- read.csv2("data/Numericos_ImpRA_SinCorr-TEST.csv");
crossvalidation5(train_ra) #0.546549
prediccionTest(train_ra, test,"02.sample_imputacion_ra.csv")


###########
# 1. Base #
###########
train_knn <- train
test_knn <- test
crossvalidation5(train) # 0.6400181
prediccionTest(train, test,"03.sample_base_imputacion_knn.csv")

# Gráfico para ver el acierto sobre train
prediccionesBase <- prediccionTest(train, train)

##########################
# 2. Selección variables #
##########################
# A través de Random Forest obtengo un vecto ordenado en función de la importancia
# de las variables que intervienen en él


rm <-randomForest::randomForest(train[,-75], as.factor(train[,75]))
importance <- caret::varImp(rm, scale=FALSE)
var <- order(importance[[1]])

crossvalidation5(train[,-var[1:25]]) # 0.5403119
prediccionTest(train[,-var[1:25]], test,"04.sample_base_sv.csv")

##########
# 3. Selección variables (Boruta) #
##########
#library(Boruta)

#train_knn <- read.csv2("data/Numericos_ImpKNN_SinCorr-TRAIN.csv",
# stringsAsFactors = FALSE)
#test_knn <- read.csv2("data/Numericos_ImpKNN_SinCorr-TEST.csv", 
#                      stringsAsFactors = FALSE)


#var2 <- Boruta(train_knn[,-ncol(train_knn)], train_knn$y)
#train3_base <- train_knn[,!(names(train_knn) %in%names(var2$finalDecision)[var2$finalDecision=="Rejected"])]
#test3_base <- test_knn[,!(names(test_knn) %in%names(var2$finalDecision)[var2$finalDecision=="Rejected"])]

#write.csv2(train3_base, "data/Numericos_ImpKNN_SinCorr-Boruta-TRAIN.csv", row.names = FALSE)
#write.csv2(test3_base, "data/Numericos_ImpKNN_SinCorr-Boruta-TEST.csv", row.names = FALSE)

#partimos de los valores iniciales
train_bo <- read.csv2("data/Numericos_ImpKNN_SinCorr-Boruta-TRAIN.csv");
test_bo <- read.csv2("data/Numericos_ImpKNN_SinCorr-Boruta-TEST.csv");

crossvalidation5(train_bo) # 0.5538811
prediccionTest(train_bo, test_bo,"05.sample_base_sv_boruta.csv")


##########
# 3. Selección variables (Boruta) + IPF#
##########
train_bo$y <- as.factor(train_bo$y)
train_bo_ipf <- NoiseFiltersR::IPF(train_bo, s=3)
train_bo_ipf <- train_bo_ipf$cleanData
test_bo_ipf <- test_bo
crossvalidation5(train_bo_ipf) # 0.7717314
prediccionTest(train_bo_ipf, test_bo_ipf,"06.sample_base_sv_boruta_ipf.csv")



##########
# 3. IPF #
##########

trainIPF <- train

library(NoiseFiltersR)
# Factorizo la clase para aplicarle IPF
trainIPF[,75] <- factor(trainIPF[,75])
# Le quito las instancias con ruido
# indico que pare a la segunda que no encuentre y que lo haga mediante consenso
trainIPF <- IPF(y ~., trainIPF, consensus = TRUE, s=2)
# Me quedo unicamente con el data.frame limpio
trainIPF <- trainIPF$cleanData

crossvalidation5(trainIPF[,-var[1:25]]) #0.5992802 
prediccionTest(trainIPF[,-var[1:25]], test,"07.sample_base_sv_ipf.csv")

############
# 4. SMOTE #
############

trainSMOTE <- train

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

crossvalidation5(trainSMOTE[,-var[1:25]]) # 
prediccionTest(trainSMOTE[,-var[1:25]], test,"08.sample_base_sv_smoote.csv")

##################
# 5. IPF + SMOTE #
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

crossvalidation5(trainIPF_SMOTE[,-var[1:25]]) # 0.6094697
prediccionTest(trainIPF_SMOTE[,-var[1:25]], test,"09.sample_base_sv_ipf_smoote.csv")

##################
# 6. SMOTE + IPF #
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

crossvalidation5(trainSMOTE_IPF[,-var[1:25]]) #0.6008772
prediccionTest(trainSMOTE_IPF[,-var[1:25]], test,"10.sample_base_sv_smote_ipv.csv")

##################
# 7. Tomek Links #
##################

trainTL <- train

# Busco las instancias Tomek Links
instanciasTL<- unbalanced::ubTomek(trainTL[,-75], (trainTL[,75]==0))
# Elimino esas instancias de train
trainTL <- trainTL[-instanciasTL$id.rm,]

crossvalidation5(trainTL[,-var[1:25]]) # 0.5485493
prediccionTest(trainTL[,-var[1:25]], test,"11.sample_base_sv_tomelinks.csv")

##########################
# 8. Tomek Links + SMOTE #
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

crossvalidation5(trainTL_SMOTE[,-var[1:25]]) # 0.6095479
prediccionTest(trainTL_SMOTE[,-var[1:25]], test,"12.sample_base_sv_tomelinks_smote.csv")

##########################
# 9. SMOTE + Tomek Links #
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

crossvalidation5(trainSMOTE_TL[,-var[1:25]]) # 0.5938863
prediccionTest(trainSMOTE_TL[,-var[1:25]], test,"13.sample_base_sv_smote_tomelinks.csv")

###########
# 10. ROS #
###########

trainROS <- train

library(ROSE)
# Factorizo la clase
trainROS[,75] <- factor(trainROS[,75])
# Modifico los levels de la clase para aplicar ROS
levels(trainROS[,75]) <- c(0,1,1,1)
instanciasROS <- ROSE::ovun.sample(y ~., trainROS, method = "over")
# Añado a train las instancias ROS de la clase 0
trainROS <- rbind(train, instanciasROS$data[which(instanciasROS$data[,75]==0),])

crossvalidation5(trainROS[,-var[1:25]]) # 0.7704359
prediccionTest(trainROS[,-var[1:25]], test,"14.sample_base_sv_ROS.csv")


#################
# 11. ROS + IPF #
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

crossvalidation5(trainROS_IPF[,-var[1:25]]) # 0.7798058
prediccionTest(trainROS_IPF[,-var[1:25]], test,"15.sample_base_sv_ROS_IPF.csv")

#############
# 12. ROS 2 #
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

crossvalidation5(train_ROS2[,-var[1:25]]) # 0.8389088
prediccionTest(train_ROS2[,-var[1:25]], test,"16.sample_base_sv_ROS-OVA.csv")

###################
# 13. ROS 2 + IPF #
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

crossvalidation5(train_ROS2_IPF[,-var[1:25]]) # 
prediccionTest(train_ROS2_IPF[,-var[1:25]], test,"17.sample_base_sv_ROS-OVA_IPF.csv")

###########
# 14. ENN #
###########

trainENN <- train

instanciasENN <- unbalanced::ubENN(trainENN[,-75], (trainENN[,75]==0))
trainENN <- trainENN[-instanciasENN$id.rm,]

crossvalidation5(trainENN[,-var[1:25]]) # 0.8723058
prediccionTest(trainENN[,-var[1:25]], test,"18.sample_base_sv_ENN.csv")

##################
# 15. Duplicar 0 #
##################

trainROS_Propio <- train

trainROS_Propio <- rbind(trainROS_Propio, train[which(trainROS_Propio[,75]==0),])

crossvalidation5(trainROS_Propio[,-var[1:25]]) # 0.5619751
prediccionTest(trainROS_Propio[,-var[1:25]], test,"19.sample_base_sv_ROS-propio.csv")

#############################
# 16. Duplicar 0 + Random 1 #
#############################

trainROS_Propio <- train

trainROS_Propio <- rbind(trainROS_Propio, train[which(trainROS_Propio[,75]==0),])
trainROS_Propio <- rbind(trainROS_Propio, train[sample(which(trainROS_Propio[,75]==1), length(which(trainROS_Propio[,75]==1))/2.5),]) 

crossvalidation5(trainROS_Propio[,-var[1:25]]) # 0.5990698
prediccionTest(trainROS_Propio[,-var[1:25]], test,"20.sample_base_sv_ROS-propio.csv")


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

crossvalidation5(descartada[,-var[1:25]]) # 0,5997358
prediccionTest(descartada[,-var[1:25]], test, "21.descartada.csv")
