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


# Elimino x0, x17 y x61
train <- train[,-c(1, 18, 62)]

# Limpieza
library(NoiseFiltersR)
# Factorizo la clase para IPF
train[,73] <- factor(train[,73])
# Le quito las instancias con ruido
# indico que pare a la segunda que no encuentre
?IPF
train <- IPF(y ~., train, consensus= TRUE, s=10)
# Me quedo unicamente con el data.frame limpio
train <- train$cleanData
(table(train[,73])/dim(train)[1])*100 # Clase 0 desbalanceada

# SMOTE
library(unbalanced)
instanciasSmote <- ubSMOTE(train[,-73], factor(as.integer(I(train[,73]==0))), perc.over = 200)
instanciasSmote <-cbind(instanciasSmote[[1]], instanciasSmote[[2]])
# Me quedo con las instancias inventadas para la clase minoritaria
instanciasSmote <- instanciasSmote[which(instanciasSmote[,73]==1),]
instanciasSmote[,73] <- 0
names(instanciasSmote)[73] <- "y"

# Añado los SMOTE
train <- rbind(train, instanciasSmote)
(table(train[,73])/dim(train)[1])*100

# Creo las clases
train.class <- train[,73]
train.class0 <- as.integer(I(train.class==0))
train.class1 <- as.integer(I(train.class==1))
train.class2 <- as.integer(I(train.class==2))
train.class3 <- as.integer(I(train.class==3))

# Quito a train la clase
train <- train[,-73]

# Dataframes
training0 <- cbind(train, y=train.class0)
training1 <- cbind(train, y=train.class1)
training2 <- cbind(train, y=train.class2)
training3 <- cbind(train, y=train.class3)

# Modelos y predicciones
modeloCaret0 <- caret::train(training0[,-73], as.factor(training0[,73]), method="glm", family=binomial(),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict0 <- caret::predict.train(modeloCaret0, training0[,-73], type="prob")
modeloCaret1 <- caret::train(training1[,-73], as.factor(training1[,73]), method="glm", family=binomial(),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict1 <- caret::predict.train(modeloCaret1, training1[,-73], type="prob")
modeloCaret2 <- caret::train(training2[,-73], as.factor(training2[,73]), method="glm", family=binomial(),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict2 <- caret::predict.train(modeloCaret2, training2[,-73], type="prob")
modeloCaret3 <- caret::train(training3[,-73], as.factor(training3[,73]), method="glm", family=binomial(),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict3 <- caret::predict.train(modeloCaret3, training3[,-73], type="prob")

# Media de Accuracy
mean(c(modeloCaret0$results$Accuracy, modeloCaret1$results$Accuracy, modeloCaret2$results$Accuracy, modeloCaret3$results$Accuracy))


predicciones <- ifelse(modelo.predict0[,2]>modelo.predict1[,2] & modelo.predict0[,2]>modelo.predict2[,2] & modelo.predict0[,2]>modelo.predict3[,2], 0,
                       ifelse(modelo.predict1[,2]>modelo.predict2[,2] & modelo.predict1[,2]>modelo.predict3[,2], 1,
                              ifelse(modelo.predict2[,2]>modelo.predict3[,2], 2, 3) ))

mean(predicciones==train.class)


# Construyo la matriz de predicciones
predicciones <- matrix(c(1:683,predicciones),683,2,byrow = FALSE)
colnames(predicciones) <- c("Id","Prediction")
# Guardo
write.table(predicciones, "sample.csv", col.names = TRUE, row.names = FALSE,
            quote=FALSE, sep = ",")
