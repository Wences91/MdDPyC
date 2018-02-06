#' función para la lectura de archivos. Argumentos
#' @param path ruta hasta el archivo de datos al leer
#' @para file archivo a leer
lecturaDatos <- function(file, path="./"){
  pathCompleto <- paste ( path , file , sep="" )
  
  dataset <- read.csv2(pathCompleto, sep=",", dec=".", 
                       stringsAsFactors = FALSE)
  
  return (dataset)
}

#' función para la escritura de archivos. Argumentos
#' @param path ruta hasta el archivo de datos al leer
#' @param path dataset conjunto de datos a guardar
#' @para file archivo a leer
escrituraDatos <- function(file, dataset, path = "./"){
  pathCompleto <- paste ( path , file , sep="" )
  write.table(dataset, pathCompleto, col.names = TRUE, 
              row.names = FALSE, quote=FALSE, sep = ",")
}
