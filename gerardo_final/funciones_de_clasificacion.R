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

#' Dado un data set train vamos a aplicar un proceso de cross validation
#' para ver como se comporta
#' @param train conjunto de entrenamiento
#' @param folds número de conjuntos que se van a conformar
crossvalidation5 <- function(train, folds_cut=5){
  # Cross-Validation
  # Se le pasa el train que se va a usar para construir el modelo y automaticamente
  # hace todo
  
  set.seed(2)
  
  # Mezclo las instacias
  datos<-train[sample(nrow(train)),]
  
  # Creo 5 folds
  folds <- cut(seq(1,nrow(train)),breaks=folds_cut,labels=FALSE)
  
  # A través de un sapply hago la CV para cada fold
  precision <- mean(sapply(1:folds_cut, function(x){
    
    indices <- which(folds==x,arr.ind=TRUE)
    test <- datos[indices, ]
    train <-datos[-indices, ]
    print (paste("Ejecutando el fold ",x))
    
    resultado <- ejecutarRipper(train, test)
    print (paste("resultado parcial", sum(resultado$prediccion==test[,dim(test)[2]])))
    
    return(
      mean(
        as.numeric(as.character(resultado$prediccion))==test[,dim(test)[2]]
      )
    )
    
  }))
  
  return(precision)
}

#' Función para guardar a fichero la preducción sobre un conjunto de datos
#'   la usaremos para guardar los datos para Kaggel
#' @param train conjunto de entrenamiento
#' @param test conjunto de test para kaggel
#' @param nombre que va a tener el fichero

prediccionTest <- function(train, test, nombre="sample.csv", folder="./"){
  # Se le pasa un train y test, entrena con el primero y 
  #predice sobre el segundo
  
  set.seed(2)
  
  resultado <- ejecutarRipper(train, test)
  
  
  # Guardo aparte las predicciones para devolverlas al final
  predicciones <- as.numeric(as.character(resultado$prediccion))
  
  # Creo un tabla de predicciones para subirlo a Kaggle
  predicciones <- matrix(
    c(1:length(predicciones),predicciones), 
    length(predicciones), 2,byrow = FALSE)
  colnames(predicciones) <- c("Id","Prediction")
  
  # Guardo
  write.table(predicciones, paste(folder,nombre), col.names = TRUE, row.names = FALSE,
              quote=FALSE, sep = ",")
  
  return(resultado$prediccion)
}
