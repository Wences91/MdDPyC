setwd("/Users/wences/Desktop/Máster/Minería de datos Preprocesamiento y clasificación/Final/MdDPyC/glm")
# Carga de datos
Train <- read.csv2("data/Numericos_ImpKNN_SinCorr-TRAIN.csv", stringsAsFactors = FALSE)
Test <- read.csv2("data/Numericos_ImpKNN_SinCorr-TEST.csv", stringsAsFactors = FALSE)

# Las copio a variables nuevas para trabajar sobre ellas
train <- Train
test <- Test

# Comprobación de NAs
sum(is.na(train))
sum(is.na(test))

(table(train[,75])/dim(train)[1])*100 # Clase 0 desbalanceada
# Limpieza
library(NoiseFiltersR)
# Factorizo la clase para IPF
train[,75] <- factor(train[,75])

# IPF
# Le quito las instancias con ruido
# indico que pare a la segunda que no encuentre
train <- IPF(y ~., train, s=2, nfolds=3, consensus=TRUE, p=0.005) #nfolda4
# Me quedo unicamente con el data.frame limpio
train <- train$cleanData
table(train[,75])
(table(train[,75])/dim(train)[1])*100 # Clase 0 desbalanceada



instanciasSmote <- unbalanced::ubTomek(train[,-75], (train[,75]==0))
train <- train[-instanciasSmote$id.rm,]

# ubENN
# instanciasSmote <- unbalanced::ubENN(train[,-75], (train[,75]==0))
# train <- train[-instanciasSmote$id.rm,]

# SMOTE
library(unbalanced)
instanciasSmote <- ubSMOTE(train[,-75], factor(as.integer(I(train[,75]==0))), perc.over = 400, perc.under = 100)
instanciasSmote <-cbind(instanciasSmote[[1]], instanciasSmote[[2]])
# Me quedo con las instancias inventadas para la clase minoritaria
instanciasSmote <- instanciasSmote[which(instanciasSmote[,75]==1),]
instanciasSmote[,75] <- 0
names(instanciasSmote)[75] <- "y"

# Añado los SMOTE
train <- rbind(train, instanciasSmote)
(table(train[,75])/dim(train)[1])*100

library(caret)
#KNN
modeloCaret0 <- caret::train(train[,-c(bbb[1:23], 75)], as.factor(train[,75]), method="knn",
                             tuneGrid = data.frame(k=1),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict0 <- caret::predict.train(modeloCaret0, Train, type="raw")
predicciones <- as.integer(as.character(modelo.predict0))


mean(predicciones==Train[,75]) # 0.8940616
modeloCaret0 #  0.8946206  0.84627

modeloCaret0$results$Kappa

sapply(1:50, function(x){
  modeloCaret0 <- caret::train(train[,-c(bbb[1:x], 75)], as.factor(train[,75]), method="knn",
                               tuneGrid = data.frame(k=1),
                               preProcess = c("center", "scale"),
                               tuneLength = 10,
                               trControl = trainControl(method = "cv"))
  c(x, modeloCaret0$results$Kappa)
}) # 23



 
library(randomForest)
# RF
rm <-randomForest(train[,-c(75)], as.factor(train[,75]))
predict.rm <- predict(rm, Train[,-c(75)])
predicciones <- as.integer(as.character(predict.rm))


mean(predicciones==Train[,75]) #0.84
rm #13

importance <- caret::varImp(rm, scale=FALSE)

aaa<-which(importance[[1]]<21)
bbb<-order(importance[[1]])

# Creo las clases
train.class <- train[,75]
train.class0 <- as.integer(I(train.class==0))
train.class1 <- as.integer(I(train.class==1))
train.class2 <- as.integer(I(train.class==2))
train.class3 <- as.integer(I(train.class==3))

# Quito a train la clase
train <- train[,-75]

# Dataframes
training0 <- cbind(train, y=train.class0)
training1 <- cbind(train, y=train.class1)
training2 <- cbind(train, y=train.class2)
training3 <- cbind(train, y=train.class3)

# Modelos y predicciones
modeloCaret0 <- caret::train(training0[,-75], as.factor(training0[,75]), method="glm", family=binomial(),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict0 <- caret::predict.train(modeloCaret0, Train[,-75], type="prob")
modeloCaret1 <- caret::train(training1[,-75], as.factor(training1[,75]), method="glm", family=binomial(),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict1 <- caret::predict.train(modeloCaret1, Train[,-75], type="prob")
modeloCaret2 <- caret::train(training2[,-75], as.factor(training2[,75]), method="glm", family=binomial(),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict2 <- caret::predict.train(modeloCaret2, Train[,-75], type="prob")
modeloCaret3 <- caret::train(training3[,-75], as.factor(training3[,75]), method="glm", family=binomial(),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict3 <- caret::predict.train(modeloCaret3, Train[,-75], type="prob")

# Media de Accuracy
mean(c(modeloCaret0$results$Accuracy, modeloCaret1$results$Accuracy, modeloCaret2$results$Accuracy, modeloCaret3$results$Accuracy))


predicciones <- ifelse(modelo.predict0[,2]>modelo.predict1[,2] & modelo.predict0[,2]>modelo.predict2[,2] & modelo.predict0[,2]>modelo.predict3[,2], 0,
                       ifelse(modelo.predict1[,2]>modelo.predict2[,2] & modelo.predict1[,2]>modelo.predict3[,2], 1,
                              ifelse(modelo.predict2[,2]>modelo.predict3[,2], 2, 3) ))

mean(predicciones==Train[,75])


# Construyo la matriz de predicciones
predicciones <- matrix(c(1:683,predicciones),683,2,byrow = FALSE)
colnames(predicciones) <- c("Id","Prediction")
# Guardo
write.table(predicciones, "sample.csv", col.names = TRUE, row.names = FALSE,
            quote=FALSE, sep = ",")
