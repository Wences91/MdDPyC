#BATERIA PRUEBAS

#leemos los datos del método base

train <- read.csv2("data/Numericos_ImpKNN_SinCorr-TRAIN.csv", header=TRUE)
test <- read.csv2("data/Numericos_ImpKNN_SinCorr-TEST.csv", header=TRUE)

str(train)

###########
# 1. Base #
###########

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

##############################
# 2. Imputación con las medias 
##############################



##########################
# 2. Selección variables #
##########################
# A través de Random Forest obtengo un vecto ordenado en función de la importancia
# de las variables que intervienen en él

library(randomForest)
rm <-randomForest::randomForest(train[,-75], as.factor(train[,75]))
importance <- caret::varImp(rm, scale=FALSE)
var <- order(importance[[1]])

validacion(train[,-var[1:25]],test,"rpart/baseSV.csv")


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
validacion(trainIPF[,-var[1:25]], test,"rpart/SVIPF.csv" ) # 0.6881916


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

validacion(trainSMOTE[,-var[1:25]],test,"rpart/smote.csv") 

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

validacion(trainIPF_SMOTE[,-var[1:25]],test,"rpart/ipfsmote.csv") 

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

validacion(trainSMOTE_IPF[,-var[1:25]],test,"rpart/smoteipf.csv") 

##################
# 7. Tomek Links #
##################

trainTL <- train

# Busco las instancias Tomek Links
instanciasTL<- unbalanced::ubTomek(trainTL[,-75], (trainTL[,75]==0))
# Elimino esas instancias de train
trainTL <- trainTL[-instanciasTL$id.rm,]

validacion(trainTL[,-var[1:25]],test,"rpart/tomeklinks.csv") 


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

validacion(trainTL_SMOTE[,-var[1:25]],test,"rpart/tomeklinkssmote.csv") 


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

validacion(trainSMOTE_TL[,-var[1:25]],test,"rpart/smotetomeklinks.csv") 


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


validacion(trainROS[,-var[1:25]],test,"rpart/ros.csv") 



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

validacion(trainROS_IPF[,-var[1:25]],test,"rpart/rosipf.csv") 


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

validacion(train_ROS2[,-var[1:25]],test,"rpart/ros2.csv") 

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

validacion(train_ROS2_IPF[,-var[1:25]],test,"rpart/ros2ipf.csv") 

###########
# 14. ENN #
###########

trainENN <- train

instanciasENN <- unbalanced::ubENN(trainENN[,-75], (trainENN[,75]==0))
trainENN <- trainENN[-instanciasENN$id.rm,]

validacion(trainENN[,-var[1:25]],test,"rpart/enn.csv") 

##################
# 15. Duplicar 0 #
##################

trainROS_Propio <- train

trainROS_Propio <- rbind(trainROS_Propio, train[which(trainROS_Propio[,75]==0),])

crossvalidation5(trainROS_Propio[,-var[1:25]]) # 0.643618
prediccionTest(trainROS_Propio[,-var[1:25]], test)

#############################
# 16. Duplicar 0 + Random 1 #
#############################

trainROS_Propio <- train

trainROS_Propio <- rbind(trainROS_Propio, train[which(trainROS_Propio[,75]==0),])
trainROS_Propio <- rbind(trainROS_Propio, train[sample(which(trainROS_Propio[,75]==1), length(which(trainROS_Propio[,75]==1))/2.5),]) 

crossvalidation5(trainROS_Propio[,-var[1:25]]) # 0.6458915
prediccionTest(trainROS_Propio[,-var[1:25]], test)

# Gráfico para ver el acierto sobre train (Mejora el acierto de la clase 0 pero se reducen en la dos)
prediccionesFinal <- prediccionTest(trainROS_Propio[,-var[1:25]], train)

ggplot2::ggplot()+
  geom_col(aes(x=0:3, y=as.data.frame(table(train[,75]))[,2], fill="Original"), alpha=.8) + 
  geom_col(aes(x=0:3, y=as.data.frame(diag(table(train[,75], prediccionesFinal)))[,1], fill="Predicciones"), alpha=.8) +
  scale_fill_discrete("Datos") +
  labs(x = "Clase", y = "Valores")


