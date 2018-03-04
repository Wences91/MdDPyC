training <- sample(1:dim(train)[1],dim(train)[1]*0.8)
train0 <- train[training,]
test0 <- train[-training,]
modeloCaret0 <- caret::train(train0[,-c(var[1:25], 75)], as.factor(train0[,75]), method="knn",
                             tuneGrid = data.frame(k=1),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict0 <- caret::predict.train(modeloCaret0, test0[,-75], type="raw")

predicciones <- as.integer(as.character(modelo.predict0))
mean(predicciones==test0[,75])