set.seed(2)
setwd("/Users/wences/Desktop/Máster/Minería de datos Preprocesamiento y clasificación/Final/MdDPyC/glm")
pre <- as.integer(read.table("/Users/wences/Downloads/sample-3.csv", stringsAsFactors = FALSE, sep=",", header =TRUE)[,2])
pre2 <- as.integer(read.table("/Users/wences/Downloads/sample-5.csv", stringsAsFactors = FALSE, sep=",", header =TRUE)[,2])
pre3 <- as.integer(read.table("/Users/wences/Downloads/sample-4.csv", stringsAsFactors = FALSE, sep=",", header =TRUE)[,2]) #.841
pre4 <- as.integer(read.table("/Users/wences/Downloads/sample-6.csv", stringsAsFactors = FALSE, sep=",", header =TRUE)[,2]) #.844
pre5 <- as.integer(read.table("/Users/wences/Downloads/sample-7.csv", stringsAsFactors = FALSE, sep=",", header =TRUE)[,2]) #.841
pre6 <- as.integer(read.table("/Users/wences/Downloads/sample-8.csv", stringsAsFactors = FALSE, sep=",", header =TRUE)[,2]) #.839
pre7 <- as.integer(read.table("/Users/wences/Downloads/sample-9.csv", stringsAsFactors = FALSE, sep=",", header =TRUE)[,2]) #.844


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

# Selección de variables más relevantes
library(randomForest)
rm <-randomForest::randomForest(train[,-c(75)], as.factor(train[,75]))

predict.rm <- predict(rm, Test)
predicciones <- as.integer(as.character(predict.rm))

importance <- caret::varImp(rm, scale=FALSE)
var<-order(importance[[1]])
var[1:25] # Voy a descartar estas variables

# SMOTE
train0 <- train
train0[which(train0[,75]%in%c(1,2,3)),75]<-1
train0[,75] <- factor(train0[,75])
(table(train0[,75])/dim(train0)[1])*100

library(unbalanced)
#smote <- unbalanced::ubSMOTE(train0[,-75], train0[,75], perc.over = 100, perc.under = 100)
#smote <- smote$X[which(smote$Y==1),] # Cojo los sinteticos de la clase minoritaria
#smote <- cbind(smote, y=0)
#train <- rbind(train, smote)

library(smotefamily)
#smote <- smotefamily::SMOTE(train0[,-75], train0[,75], dup_size = 1.8)
#smote <- smote$syn_data
#names(smote)[75] <- "y"
#train <- rbind(train, smote)


train <- Train
(table(train[,75])/dim(train)[1])*100
train <- rbind(train, train[which(train[,75]==0),]) # 0.84
train <- rbind(train, train[sample(which(train[,75]==1), length(which(train[,75]==1))/2.8),]) 
#KNN
library(caret)
modeloCaret0 <- caret::train(traini[,-c(var[1:25], 75)], as.factor(traini[,75]), method="knn",
                             tuneGrid = data.frame(k=1),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict0 <- caret::predict.train(modeloCaret0, Test, type="raw")
predicciones <- as.integer(as.character(modelo.predict0))
sum(predicciones==pre) #655, 656, 655   - 651
sum(predicciones==pre2) #658, 659, 658  - 654
sum(predicciones==pre3) #683, 682, 681  - 672
sum(predicciones==pre4) # 681           - 674
sum(predicciones==pre5) #               - 673
sum(predicciones==pre6) #               - 672
sum(predicciones==pre7) #               - 673
modeloCaret0# 0.841, 0.841, 0.844

testi <- cbind(Test, y=predicciones)
testi[,75]<-factor(testi[,75])
testi <- IPF(y ~., testi, s=2, nfolds=5, consensus=TRUE, p=0.01)
testi <- testi$cleanData
traini <- rbind(train, testi)

mean(predicciones==Train[,75]) # 1
modeloCaret0 # 0.9855558  0.9803243 -> 0.84184

sapply(1:50, function(x){
  modeloCaret0 <- caret::train(train[,-c(bbb[1:x], 75)], as.factor(train[,75]), method="knn",
                               tuneGrid = data.frame(k=1),
                               preProcess = c("center", "scale"),
                               tuneLength = 10,
                               trControl = trainControl(method = "cv"))
  c(x,modeloCaret0$results$Kappa)
})


importance <- caret::varImp(modeloCaret0, scale=FALSE)

print(importance[[1]]<0.56)
c(1, 17, 31, 32, 39,48, 49, 56, 68, 69, 73)
c(1, 2, 5, 9, 17, 30, 32, 34, 38,44, 55, 56, 58, 68, 69)
aaa
library(randomForest)
# RF
rm <-randomForest(train[,-c(75)], as.factor(train[,75]))
?randomForest
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
