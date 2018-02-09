#' función que calcula ripper sobre un conjunt de train/tes
#'  devuelve una medida de acuracy. Se supone que el
#'  último atributod del dataset es la variable de clasificación.
#'  En este caso si hay string values da fallo
#' @param train conjunto de entrenamiento
#' @param test conjunto de entrenamiento
#' @param id de comlumnas o nombre de atributos a excluir

ejecutarRipper <- function(train, test, excludeAttr=c()){
  library(caret)
  library(RWeka)
  
  #entendemos que en la última columa está la clase discriminatoria
  class <- colnames(train)[ncol(train)]
  
  if(!is.factor(train[,ncol(train)])){
    train[,length(train)] <- as.factor(train[,ncol(train)])
  }
  
  formulaRip <- as.formula(paste(class,"~."))
  #si no tenemos atributos que excluir
  if(length(excludeAttr) == 0){
    trainOwn <- train
    testOwn <- test
  }else{
    #caso que sean números suponemos que son índices
    if(is.numeric(excludeAttr)){
      #no quitamos el último elemenot que es la clase
      trainOwn <- train[, -excludeAttr]
      testOwn <- test[, -excludeAttr]
    }
    else{#caso de que los atributos sean nombres
      #no quitamos el último elemenot que es la clase
      trainOwn <- train[, !(names(train) %in% excludeAttr)]
      testOwn <- test[, !(names(test) %in% excludeAttr)]
    }
    
  }
  
  model.Ripper = JRip(formulaRip, trainOwn)
  model.Ripper.pred = predict(model.Ripper, newdata = testOwn)
  return (list("modelo"=model.Ripper,"prediccion"=model.Ripper.pred))
}

