source("funciones_de_clasificacion.R")
source("funciones_aed.R")
source("funciones_modificacion_dataset.R")
library(FSelector)
library(NoiseFiltersR)
library(unbalanced)
library(Boruta)
library(smotefamily)

Train <- read.csv2("data/Numericos_ImpKNN_SinCorr-TRAIN.csv", 
                   stringsAsFactors = FALSE)
TestFinal <- read.csv2("data/Numericos_ImpKNN_SinCorr-TEST.csv", 
                       stringsAsFactors = FALSE)


colNumericas <- which(sapply(Train, is.numeric)==TRUE)
colNumericasF <- which(sapply(TestFinal, is.numeric)==TRUE)



# Selección de variables más relevantes
library(randomForest)
rm <-randomForest::randomForest(Train[,-c(75)], as.factor(Train[,75]))
importance <- caret::varImp(rm, scale=FALSE)
var<-order(importance[[1]])
var[1:25] # Voy a descartar estas variables
Train <- Train[,-var[1:25]]
TestFinal <- TestFinal[,-var[1:25]]

var2 <- Boruta(Train[,-ncol(Train)], Train$y)
Train <- Train[,!(names(Train) %in%names(var2$finalDecision)[var2$finalDecision=="Rejected"])]
TestFinal <- TestFinal[,!(names(Train) %in%names(var2$finalDecision)[var2$finalDecision=="Rejected"])]

#convertimos en factor la clase si no lo es
if(!is.factor(Train[,ncol(Train)])){
  Train[,length(Train)] <- as.factor(Train[,ncol(Train)])
}




#filtrado usando ipf
filtter <- NoiseFiltersR::IPF(Train, s=3)
Train <- filtter$cleanData
(table(Train$y)/dim(Train)[1])*100




#Aplicamos smote
smote <- ubSMOTE(Train[,-ncol(Train)], factor(as.integer(I(Train[,ncol(Train)]==0))), perc.over = 300)
r <- cbind(smote$X, y=as.integer(as.character(smote$Y)))
r <- r[which(r[,ncol(Train)]==1),]
r[,ncol(Train)]<-0
Train<-rbind(Train,r)

b <- DBSMOTE(Train[,-ncol(Train)], Train$y, 20)
b <- ANS(Train[,-ncol(Train)], Train$y)#46-48%
b <- ADAS(Train[,-ncol(Train)], Train$y) #48-50%

Train <- b$data
Train$y <- as.factor(Train$class)
Train$class<-NULL

#aplicamos bosting aleatorio
Train <- rbind(Train, Train[which(Train[,ncol(Train)]==0),]) # 0.84
Train <- rbind(Train, Train[sample(which(Train[,ncol(Train)]==1), length(which(Train[,ncol(Train)]==1))/2.5),]) 
#K

(table(Train$y)/dim(Train)[1])*100

#ahora aplicamos enn para suavizar fronteras
t <- NoiseFiltersR::ENN(y~.,Train, k=4)
Train <- t$cleanData
(table(Train$y)/dim(Train)[1])*100

#filtrado usando ipf
filtter <- NoiseFiltersR::IPF(Train, s=3)

Train <- filtter$cleanData
(table(Train$y)/dim(Train)[1])*100


subset <- c()
#weights <- FSelector::chi.squared(y~.,Train)
#subset <- FSelector::cutoff.k(weights,5)



salidaSummary <- sort(table(Train$y))


Test <-do.call("rbind",lapply(names(salidaSummary), function(x){
  porcentaje <- salidaSummary[x]/sum(salidaSummary)
  t <- rownames(Train[Train$y==x,])
  #print(sample(length(t),length(t)*0.1))
  data.frame(Train[t[sample(length(t),length(t)*0.1)],])
  #data.frame(Train[t[sample(length(t),1)],])
})
)
Test.salida <- Test$y
Test$y <- NULL

TrainP <- Train[-as.integer(rownames(Test)),]


colNoNumericas <- colnames(TrainP)[-colNumericas]

final <- colNumericas[names(colNumericas) %in% subset]

salida <- ejecutarRipper(TrainP, Test, final)
sum(as.numeric(salida$prediccion) == as.numeric(Test.salida))/sum(as.numeric(Test.salida))
salidaF <- ejecutarRipper(Train, TestFinal, final)


##knn
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
# Guardo
write.table(prediccionesF, "sample.csv", col.names = TRUE, row.names = FALSE,
            quote=FALSE, sep = ",")

