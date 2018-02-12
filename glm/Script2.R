# setwd("/Users/wences/Desktop/Máster/Minería de datos Preprocesamiento y clasificación/Final/MdDPyC/glm")
# Carga de datos
Train <- read.csv2("data/numericos-imputacionKnn-Train.csv", stringsAsFactors = FALSE)
Test <- read.csv2("data/numericos-imputacionKnn-Test.csv", stringsAsFactors = FALSE)

# Las copio a variables nuevas para trabajar sobre ellas
train <- Train
test <- Test

# Comprobación de NAs
sum(is.na(train))
sum(is.na(test))

# Creo las clases
train.class <- train[,76]
train.class0 <- as.integer(I(train.class==0))
train.class1 <- as.integer(I(train.class==1))
train.class2 <- as.integer(I(train.class==2))
train.class3 <- as.integer(I(train.class==3))

# Quito a train la clase
train <- train[,-76]

# Dataframes
training0 <- cbind(train, y=train.class0)
training1 <- cbind(train, y=train.class1)
training2 <- cbind(train, y=train.class2)
training3 <- cbind(train, y=train.class3)

# Modelos y predicciones
modeloCaret0 <- caret::train(training0[,-76], as.factor(training0[,76]), method="glm", family=binomial(),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict0 <- caret::predict.train(modeloCaret0, training0[,-76], type="prob")
modeloCaret1 <- caret::train(training1[,-76], as.factor(training1[,76]), method="glm", family=binomial(),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict1 <- caret::predict.train(modeloCaret1, training1[,-76], type="prob")
modeloCaret2 <- caret::train(training2[,-76], as.factor(training2[,76]), method="glm", family=binomial(),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict2 <- caret::predict.train(modeloCaret2, training2[,-76], type="prob")
modeloCaret3 <- caret::train(training3[,-76], as.factor(training3[,76]), method="glm", family=binomial(),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict3 <- caret::predict.train(modeloCaret3, training3[,-76], type="prob")

# Media de Accuracy
mean(c(modeloCaret0$results$Accuracy, modeloCaret1$results$Accuracy, modeloCaret2$results$Accuracy, modeloCaret3$results$Accuracy))


predicciones <- ifelse(modelo.predict0[,2]>modelo.predict1[,2] & modelo.predict0[,2]>modelo.predict2[,2] & modelo.predict0[,2]>modelo.predict3[,2], 0,
                       ifelse(modelo.predict1[,2]>modelo.predict2[,2] & modelo.predict1[,2]>modelo.predict3[,2], 1,
                              ifelse(modelo.predict2[,2]>modelo.predict3[,2], 2, 3) ))

mean(predicciones==Train[,76])



# Construyo la matriz de predicciones
predicciones <- matrix(c(1:683,predicciones),683,2,byrow = FALSE)
colnames(predicciones) <- c("Id","Prediction")
# Guardo
write.table(predicciones, "sample.csv", col.names = TRUE, row.names = FALSE,
            quote=FALSE, sep = ",")
