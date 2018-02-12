# setwd("/Users/wences/Desktop/Máster/Minería de datos Preprocesamiento y clasificación/Final/MdDPyC/glm")
# Carga de datos
Train <- read.table("data/my_dataset_train.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)
Test <- read.table("data/my_dataset_test.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)

# Las copio a variables nuevas para trabajar sobre ellas
train <- Train
test <- Test

dif<-train[1,42]/train[1,49]
ins <- which(is.na(train[,42]))
train[ins,42] <- dif*train[ins,49]

dif<-test[1,42]/test[1,49]
ins <- which(is.na(test[,42]))
test[ins,42] <- dif*test[ins,49]


train <- train[,-49]
test <- test[,-49]

train <- imputacionKnnTotal(train)
test <- imputacionKnnTotal(test)

# x0
train[which(train[,1]=="VS"), 1] <- 1
train[which(train[,1]=="S"), 1] <- 2
train[which(train[,1]=="M"), 1] <- 3
train[which(train[,1]=="L"), 1] <- 4
train[which(train[,1]=="VL"), 1] <- 5
train[,1] <- as.integer(train[,1])


test[which(test[,1]=="VS"), 1] <- 1
test[which(test[,1]=="S"), 1] <- 2
test[which(test[,1]=="M"), 1] <- 3
test[which(test[,1]=="L"), 1] <- 4
test[which(test[,1]=="VL"), 1] <- 5
test[,1] <- as.integer(test[,1])

#x14
train[which(train[,15]=="true"), 15] <- 1
train[which(train[,15]=="false"), 15] <- 0
train[,15] <- as.integer(train[,15])

test[which(test[,15]=="true"), 15] <- 1
test[which(test[,15]=="false"), 15] <- 0
test[,15] <- as.integer(test[,15])

#x17
train[,18] <- as.integer(substr(train[,18], 4, 4))

test[,18] <- as.integer(substr(test[,18], 4, 4))

#x51
train[which(train[,51]=="yes"), 51] <- 1
train[which(train[,51]=="no"), 51] <- 0
train[,51] <- as.integer(train[,51])

test[which(test[,51]=="yes"), 51] <- 1
test[which(test[,51]=="no"), 51] <- 0
test[,51] <- as.integer(test[,51])

#x61
train[,61] <- as.integer(substr(train[,61], 4, 5))

test[,61] <- as.integer(substr(test[,61], 4, 5))

#x63
train[which(train[,63]=="active"), 63] <- 1
train[which(train[,63]=="inactive"), 63] <- 0
train[,63] <- as.integer(train[,63])

test[which(test[,63]=="active"), 63] <- 1
test[which(test[,63]=="inactive"), 63] <- 0
test[,63] <- as.integer(test[,63])

# Guardado
write.csv2(train, "data/Numericos_ImpKNN_SinCorr-TRAIN.csv", row.names = FALSE)
write.csv2(test, "data/Numericos_ImpKNN_SinCorr-TEST.csv", row.names = FALSE)
