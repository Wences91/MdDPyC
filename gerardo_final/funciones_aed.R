#'función para el cálculo de valores perdidos por columna
#'  entendemos que en el caso de los caracteres el valor perdido
#'  es el valor vacío
#' @param dataset conjunto de datos sobre el que vamos a buscar 
#'   valores perdidos
#' @return número de valores perdidos por fila
calculoValoresPerdidosPorFila <- function(dataset){
  res <- apply ( dataset , 1 , 
                 function (x) sum(is.na(x) | x==""))/ncol(dataset) * 100
  return (res)
}
