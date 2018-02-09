source("funciones_de_clasificacion.R")
source("funciones_aed.R")
source("funciones_de_entrada_salida.R")
source("funciones_modificacion_dataset.R")


Train <-lecturaDatos("my_dataset_train.csv")


#valores perdidos moda para columnas no numéricas
# y valores numéricos mean
escrituraDatos("dataset_mean_moda.csv",
               imputacionValoresPerdidos(Train), 
               "./datos_parciales/")

escrituraDatos("dataset_median_moda.csv",
               imputacionValoresPerdidos(Train,c(),"median"), 
               "./datos_parciales/")



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

Train <- Train[-as.integer(rownames(Test)),]

colNumericas <- which(sapply(Train, is.numeric)==TRUE)
colNoNumericas <- colnames(Train)[-colNumericas]

salida <- ejecutarRipper(Train, Test, colNoNumericas)
sum(salida$prediccion == Test.salida)/sum(Test.salida)