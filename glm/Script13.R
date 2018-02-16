setwd("/Users/wences/Desktop/Máster/Minería de datos Preprocesamiento y clasificación/Final/MdDPyC/glm")
pre <- as.integer(read.table("/Users/wences/Downloads/sample-3.csv", stringsAsFactors = FALSE, sep=",", header =TRUE)[,2])
pre2 <- as.integer(read.table("/Users/wences/Downloads/sample-5.csv", stringsAsFactors = FALSE, sep=",", header =TRUE)[,2])
pre3 <- as.integer(read.table("/Users/wences/Downloads/sample-4.csv", stringsAsFactors = FALSE, sep=",", header =TRUE)[,2])
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

# Limpieza IPF
library(NoiseFiltersR)
# Factorizo la clase para IPF
train[,75] <- factor(train[,75])
# Le quito las instancias con ruido
# indico que pare a la segunda que no encuentre



train <- IPF(y ~., train, s=2, nfolds=5, consensus=TRUE, p=0.01) #nfolda4
# Me quedo unicamente con el data.frame limpio
train <- train$cleanData
(table(train[,75])/dim(train)[1])*100 # Clase 0 desbalanceada


library(unbalanced)
?ubTomek
instanciasSmote <- unbalanced::ubTomek(train[,-75], as.integer(train[,75]==0))
train <- train[-instanciasSmote$id.rm,]

instanciasSmote <- unbalanced::ubENN(train[,-75], (train[,75]==0))
train <- train[-instanciasSmote$id.rm,]

# SMOTE
library(unbalanced)

instanciasSmote <- ubSmoteExs(train[,-75], factor(as.integer(I(train[,75]==0))))
instanciasSmote <-cbind(instanciasSmote[[1]], instanciasSmote[[2]])
# Me quedo con las instancias inventadas para la clase minoritaria
instanciasSmote <- instanciasSmote[which(instanciasSmote[,75]==1),]
instanciasSmote[,75] <- 0
names(instanciasSmote)[75] <- "y"
# Añado los SMOTE
train <- rbind(train, instanciasSmote)
(table(train[,75])/dim(train)[1])*100

p <- train
set.seed(100)
train <- p

# RUS
train2 <- train[which(train[,75]==0|train[,75]==1),]
train2[,75] <- factor(train2[,75])
table(train2[,75])
prueba1 <- ROSE::ovun.sample(y ~., train2, method = "both", p=0.5)#0.50
prueba1 <- prueba1$data
levels(prueba1[,75])<-c(0,1)

prueba3[,75]
train3 <- train[which(train[,75]==0|train[,75]==2),]
train3[,75] <- factor(train3[,75])
table(train3[,75])
prueba2 <- ROSE::ovun.sample(y ~., train3, method = "both", p=0.25)#0.50
prueba2 <- prueba2$data
levels(prueba2[,75])<-c(0,2)

train4 <- train[which(train[,75]==0|train[,75]==3),]
train4[,75] <- factor(train4[,75])
table(train4[,75])
prueba3 <- ROSE::ovun.sample(y ~., train4, method = "both", p=0.25)#0.50
prueba3 <- prueba$data
levels(prueba3[,75])<-c(0,3)

train <- rbind(train2, train3, train4, train)
table(train[,75])

train3 <- train
levels(train3[,75])<- c(1,0,1,1)
table(train3[,75])
prueba <- ROSE::ovun.sample(y ~., train3, method = "over", p=0.27)#27
pruebaTrain <- cbind(prueba$data[which(prueba$data[,75]==0),-75],y=1)
train <- rbind(train, pruebaTrain)


minority.indices0 <- (1:dim(train)[1])[train[,75] == 0]
to.add0 <- dim(train)[1] - 2.5 * length(minority.indices0)
duplicate0 <- sample(minority.indices0, to.add0, replace = T)
for( j in 1:length(duplicate0)){
  train <- rbind(train, train[duplicate0[j],])
} 

minority.indices1 <- (1:dim(train)[1])[train[,75] == 1]
to.add1 <- dim(train)[1] - 4.2 * length(minority.indices1)
duplicate1 <- sample(minority.indices1, to.add1, replace = T)
for( j in 1:length(duplicate1)){
  train <- rbind(train, train[duplicate1[j],])
} 


library(caret)
#KNN
modeloCaret0 <- caret::train(train[,-c(bbb[1:25], 75)], as.factor(train[,75]), method="knn",
                             tuneGrid = data.frame(k=1),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict0 <- caret::predict.train(modeloCaret0, Test, type="raw")
predicciones <- as.integer(as.character(modelo.predict0))
sum(predicciones==pre) #668
sum(predicciones==pre2)
sum(predicciones==pre3) #683
modeloCaret0

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
