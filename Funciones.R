
moda <- function(x) {
  #' Dado un vector se crea una tabla de conteos y se selecciona el que más aparece
  
  t <- table(x)
  return(as.numeric(names(t)[t == max(t)]))
}


valoresPerdidos <- function(y){
  #' Dada una matriz o dataframe se le aplican don sapply, uno por fila y otro por columna
  #' cada uno cuenta los NAs y campos vacios en el caso de caracteres ("")
  #' Tras ello se devuelve una lista con el máximo para cada uno de ellos, el total de
  #' valores perdidos en general y por por columna y los índice de filas y columnas con valores perdidos
  
  instanciasPerdidos <- sapply(1:dim(y)[1], function(x) sum(is.na(y[x,]))+sum(y[x,]=="", na.rm = TRUE))
  
  variablesPerdidos <- sapply(1:dim(y)[2], function(x) sum(is.na(y[,x]))+sum(y[,x]=="", na.rm = TRUE))
  names(variablesPerdidos) <- colnames(y)

  return(list("Total perdidos"=sum(instanciasPerdidos),
              "Maximo instancia"=max(instanciasPerdidos),
              "Maximo variable"=max(variablesPerdidos),
              "Total de perdidos por variable"=variablesPerdidos,
              "Instancias con perdidos"=which(instanciasPerdidos!=0),
              "Variables con perdidos"=which(variablesPerdidos!=0)))
}

