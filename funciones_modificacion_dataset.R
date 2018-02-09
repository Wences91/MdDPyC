#' filtramos filas de un dataset que tenga más de "muchos" valores perdidos
#' @param dataset conjunto de datos sobre el que vamos a filtrar
#' @param int número de valores perdidos a partir de los cuales 
#'   se va a filtrar
#' @return nuevo dataset con menos filas.

eliminarFilasConMuchoValoresPerdidos <- function(dataset, muchos=5){
  res <- calculoValoresPerdidosPorFila(dataset)
  mal <- ( res > muchos)
  return (datos [ ! mal , ])
}

# funciones para imputar nuevos valores a los valores perdidos
#' @param dataset conjunto de datos con los que vamos a trabajar
#' @param columnas a las que se les van a imputar las cuestiones
#' @param modelo string tipo de imputacion mean|median

imputacionValoresPerdidos <- function(dataset, columnas=c(), 
                                      modelo="mean"){
  datasetInterno <- dataset
  
  if(length(columnas) == 0)
    iteracion = 1:ncol(datasetInterno)
  else
    iteracion = columnas
  
  datasetInterno[,iteracion]<-sapply(iteracion, function(columna){
    fila <- datasetInterno[,columna]
    if(is.numeric(fila)){
      valorNuevo <- do.call(modelo, list(x = fila,na.rm = TRUE))
      fila[is.na(fila)] <- valorNuevo 
      fila
    }
    else{
      t <- sort(table(fila),decreasing = TRUE)
      fila[is.na(fila) | fila == ""] <- names(t)[1]
    }
  })
  return (datasetInterno)
}
