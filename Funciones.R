
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


imputacionNumericos <- function(x, y=1:dim(x)[2]){
  #' Dado una matriz o data.frame x y un vector y a modo de índice de variables, en su defecto todas
  #' se buscan columna a columna los valores NAs y se sustituyen por la media.
  #' En primer lugar comprueba que columnas son numericas del vector, obteniendo dicho índice.
  #' Tras ello recorre un bucle cada columna de las marcadas en el indice sustituyendo los NA por las medias.
  
  x <- x[,y]
  colNumericas <- sapply(x, is.numeric)
  
  indicesColNumericas <- which(colNumericas)
  
  for(z in indicesColNumericas){
    x[is.na(x[,z]),z]<-mean(x[,z], na.rm = TRUE)
  }
  
  x
}

imputacionCaracteres <- function(x, y=1:dim(x)[2]){
  #' Igual que numéricos pero con caracteres y la moda
   
  x <- x[,y]
  colCaracteres <- sapply(x, is.character)
  
  indicesColCaracteres <- which(colCaracteres)
  
  for(z in indicesColCaracteres){
    x[x[,z]=="",z]<-names(sort(table(x[,z]), decreasing = TRUE))[1]
  }
  
  x
}
