source("funciones_de_clasificacion.R")
source("funciones_aed.R")
source("funciones_modificacion_dataset.R")
library(FSelector)
library(NoiseFiltersR)
library(unbalanced)
library(Boruta)
library(smotefamily)
#partimos del dataset preprocesado en el que no hay variables correladas,
# los valores perdidos se han imputado con KNN y
# todos los valores son numéricos
Train <- read.csv2("data/Numericos_ImpKNN_SinCorr-TRAIN.csv", 
                   stringsAsFactors = FALSE)
TestFinal <- read.csv2("data/Numericos_ImpKNN_SinCorr-TEST.csv", 
                       stringsAsFactors = FALSE)


# Selección de variables más relevantes
#Forma de calcular las variables más relevantes usando RF
library(randomForest)
rm <-randomForest::randomForest(Train[,-c(75)], as.factor(Train[,75]))
importance <- caret::varImp(rm, scale=FALSE)
var<-order(importance[[1]])
var[1:25] # Voy a descartar estas variables
Train <- Train[,-var[1:25]]
TestFinal <- TestFinal[,-var[1:25]]

#Forma de calcular variables relevantes usando Boruta
var2 <- Boruta(Train[,-ncol(Train)], Train$y)
Train <- Train[,!(names(Train) %in%names(var2$finalDecision)[var2$finalDecision=="Rejected"])]
TestFinal <- TestFinal[,!(names(Train) %in%names(var2$finalDecision)[var2$finalDecision=="Rejected"])]

#convertimos en factor la clase si no lo es
if(!is.factor(Train[,ncol(Train)])){
  Train[,length(Train)] <- as.factor(Train[,ncol(Train)])
}

# Forma de aplicar el filtro de ruido IFP
filtter <- NoiseFiltersR::IPF(Train, s=3)
Train <- filtter$cleanData
(table(Train$y)/dim(Train)[1])*100

#Forma de aplicar SMOTE al conjunto de datos 
#Uso de la función ubSMOTE
smote <- ubSMOTE(Train[,-ncol(Train)], factor(as.integer(I(Train[,ncol(Train)]==0))), perc.over = 300)
r <- cbind(smote$X, y=as.integer(as.character(smote$Y)))
r <- r[which(r[,ncol(Train)]==1),]
r[,ncol(Train)]<-0
Train<-rbind(Train,r)

#uso de la función DBSMOTE
b <- DBSMOTE(Train[,-ncol(Train)], Train$y, 20)
#uso de la función ANS
b <- ANS(Train[,-ncol(Train)], Train$y)#46-48%
#uso de la función ADAS
b <- ADAS(Train[,-ncol(Train)], Train$y) #48-50%

#aplicar SMOTE para su uso
Train <- b$data
Train$y <- as.factor(Train$class)
Train$class<-NULL

#aplicamos bosting aleatorio duplicando 0 y añadiendo un número aleatorio de instancias de la clase 1
Train <- rbind(Train, Train[which(Train[,ncol(Train)]==0),]) # 0.84
Train <- rbind(Train, Train[sample(which(Train[,ncol(Train)]==1), length(which(Train[,ncol(Train)]==1))/2.5),]) 
(table(Train$y)/dim(Train)[1])*100

#Formas de aplicar undersampling
#Detección de fronteras con ENN
t <- NoiseFiltersR::ENN(y~.,Train, k=4)
Train <- t$cleanData
(table(Train$y)/dim(Train)[1])*100

#filtrado usando ipf
filtter <- NoiseFiltersR::IPF(Train, s=3)

Train <- filtter$cleanData
(table(Train$y)/dim(Train)[1])*100


subset <- c()

# obtenemos la clase del conjunto de entrenamiento
salidaSummary <- sort(table(Train$y))

# forma generar un data frame para test donde los items de cada
# clase se reparten de forma proporcional
Test <-do.call("rbind",lapply(names(salidaSummary), function(x){
    porcentaje <- salidaSummary[x]/sum(salidaSummary)
    t <- rownames(Train[Train$y==x,])
    #tamaño del nuevo dataframe
    data.frame(Train[t[sample(length(t),length(t)*0.2)],])
  })
)
Test.salida <- Test$y
Test$y <- NULL
#nos quedamos en train aquellas que no se han seleccionado para test
TrainP <- Train[-as.integer(rownames(Test)),]

#ejecutamos ripper para ver los resultados como quedan
# Ripper se ha ejecutado para todas las combinaciones descritas anteriormete
salida <- ejecutarRipper(TrainP, Test)
sum(as.numeric(salida$prediccion) == as.numeric(Test.salida))/sum(as.numeric(Test.salida))

#ejecutamos ripper para ver como quedan los resultados
salidaF <- ejecutarRipper(Train, TestFinal)


# aplicamos la filosofía a knn
modeloCaret0 <- caret::train(TrainP[,-ncol(TrainP)], as.factor(TrainP$y), method="knn",
                             tuneGrid = data.frame(k=1),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict0 <- caret::predict.train(modeloCaret0, Test, type="raw")
predicciones <- as.integer(as.character(modelo.predict0))
mean(predicciones==Test.salida)

modelo.predict0F <- caret::predict.train(modeloCaret0, TestFinal, type="raw")
prediccionesF <- as.integer(as.character(modelo.predict0F))
# Construyo la matriz de predicciones
prediccionesF <- matrix(c(1:683,prediccionesF),683,2,byrow = FALSE)
colnames(prediccionesF) <- c("Id","Prediction")
# Guardamos para subir a kaggle
write.table(prediccionesF, "sample.csv", col.names = TRUE, row.names = FALSE,
            quote=FALSE, sep = ",")

