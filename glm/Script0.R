library(caret)
library(dummies)
library(NoiseFiltersR)
library(unbalanced)
library(randomForest)


## Preparación de los datos
# Imputación con KNN (función propia)
knntraining <- imputacionKnnTotal(Train)
knntesting <- imputacionKnnTotal(Test)

write.csv2(knntraining, "imputacionKnn-Train.csv", row.names = FALSE)
write.csv2(knntesting, "imputacionKnn-Test.csv", row.names = FALSE, queo)

?write.csv2
test <- knntesting
test[,15] <- as.integer(I(test[,15])=="true")
test[,52] <- as.integer(I(test[,52])=="yes")
test[,64] <- as.integer(I(test[,64])=="active")
test[,15] <- scale(test[,15])
test[,52] <- scale(test[,52])
test[,64] <- scale(test[,64])
test<-cbind(test[,-1],data.frame(dummy(test[,1]))[-1])
#Importante asignar los mismos nombre a test y test
names(test)[75:78]<-c("x76","x77","x78", "x79")
test[,75] <- scale(test[,75])
test[,76] <- scale(test[,76])
test[,77] <- scale(test[,77])
test[,78] <- scale(test[,78])
# Para dejar la clase al final
test <- test[,-c(17, 61, 79)]
##### En caso de coger las variables importantes
test <- test[,variablesImportantes]


# Para testear el modelo
class.prueba <- knntraining[,76]
test <- knntraining
test[,15] <- as.integer(I(test[,15])=="true")
test[,52] <- as.integer(I(test[,52])=="yes")
test[,64] <- as.integer(I(test[,64])=="active")
test[,15] <- scale(test[,15])
test[,52] <- scale(test[,52])
test[,64] <- scale(test[,64])
test<-cbind(test[,-1],data.frame(dummy(test[,1]))[-1])
#Importante asignar los mismos nombre a test y test
names(test)[76:79]<-c("x76","x77","x78", "x79")
test[,76] <- scale(test[,76])
test[,77] <- scale(test[,77])
test[,78] <- scale(test[,78])
test[,79] <- scale(test[,79])
# Para dejar la clase al final
test <- cbind(test[,-75], "y"=test[,75])
test <- test[,-c(17, 61, 79)]
##### En caso de coger las variables importantes
test <- test[,variablesImportantes]




# Lo asigno a las variables que voy a usar para no perder las originales
training <- knntraining
testing <- knntesting



# Dummies
training[,15] <- as.integer(I(training[,15])=="true")
training[,52] <- as.integer(I(training[,52])=="yes")
training[,64] <- as.integer(I(training[,64])=="active")
training[,15] <- scale(training[,15])
training[,52] <- scale(training[,52])
training[,64] <- scale(training[,64])
training<-cbind(training[,-1],data.frame(dummy(training[,1]))[-1])
#Importante asignar los mismos nombre a training y test
names(training)[76:79]<-c("x76","x77","x78", "x79")
training[,76] <- scale(training[,76])
training[,77] <- scale(training[,77])
training[,78] <- scale(training[,78])
training[,79] <- scale(training[,79])
# Para dejar la clase al final
training <- cbind(training[,-75], "y"=training[,75])

# Se eliminan las variables categóricas no dummizadas
training <- training[,-c(17, 61)]

# Limpieza
# Factorizo la clase para IPF
training[,77] <- factor(training[,77])
# Le quito las instancias con ruido (excluyendo las que tienen caracteres)
# indico que pare a la segunda que no encuentre
training <- IPF(y ~., training, s=2)
# Me quedo unicamente con el data.frame limpio
training <- training$cleanData
(table(training[,77])/dim(training)[1])*100 # Clase 0 desbalanceada


### Varaiables más destacadas
variablesImportantes <- filterVarImp(training[,1:76], training[,77], scale=TRUE)
variablesImportantes<-data.frame("filas"=1:76, "nombre"=rownames(variablesImportantes), variablesImportantes)
# Ordenor por las más importantes para la clase 0
variablesImportantes <- variablesImportantes[order(variablesImportantes[,3], decreasing = TRUE),]
variablesImportantes<-variablesImportantes[which(variablesImportantes[,3]>0.6),1]
# Variables más destacadas, para seleccionar más adelante
variablesImportantes


# SMOTE
instanciasSmote <- ubSMOTE(training[,-77], factor(as.integer(I(training[,77]==0))), perc.over = 300)
instanciasSmote <-cbind(instanciasSmote[[1]], instanciasSmote[[2]])
# Me quedo con las instancias inventadas para la clase minoritaria
instanciasSmote <- instanciasSmote[which(instanciasSmote[,77]==1),]
instanciasSmote[,77] <- 0
names(instanciasSmote)[77] <- "y"

# Añado los SMOTE
training <- rbind(training, instanciasSmote)


train.class <- training[,77]
# Quita la clase del data.frame
training <- training[,-77]
(table(train.class)/dim(training)[1])*100 # Se desbalancea la clase 0

##### En caso de coger solo las variables importantes
training <- training[,variablesImportantes]


# Creo las clases
train.class0 <- as.integer(I(train.class==0))
train.class1 <- as.integer(I(train.class==1))
train.class2 <- as.integer(I(train.class==2))
train.class3 <- as.integer(I(train.class==3))

# Dataframes
training0 <- cbind(training, y=train.class0)
training1 <- cbind(training, y=train.class1)
training2 <- cbind(training, y=train.class2)
training3 <- cbind(training, y=train.class3)


modelo0 <- glm(y~., data=training0, family = "binomial", maxit=500)
modelo.predict0 <- predict(modelo0,newdata=test ,type="response")
modelo1 <- glm(y~., data=training1, family = "binomial", maxit=500)
modelo.predict1 <- predict(modelo1,newdata=test ,type="response")
modelo2 <- glm(y~., data=training2, family = "binomial", maxit=500)
modelo.predict2 <- predict(modelo2,newdata=test ,type="response")
modelo3 <- glm(y~., data=training3, family = "binomial", maxit=500)
modelo.predict3 <- predict(modelo3,newdata=test ,type="response")

predicciones <- ifelse(modelo.predict0>modelo.predict1 & modelo.predict0>modelo.predict2 & modelo.predict0>modelo.predict3, 0,
                       ifelse(modelo.predict1>modelo.predict2 & modelo.predict1>modelo.predict3, 1,
                              ifelse(modelo.predict2>modelo.predict3, 2, 3) ))

mean(predicciones==class.prueba)

# Construyo la matriz de predicciones
predicciones <- matrix(c(1:683,predicciones),683,2,byrow = FALSE)
colnames(predicciones) <- c("Id","Prediction")
# Guardo
write.table(predicciones, "sample.csv", col.names = TRUE, row.names = FALSE,
            quote=FALSE, sep = ",")
