source('preProc.R')
library(randomForest)

train <- dataTrain()
test <- dataTest()
head(train)
str(train)
str(test)

model <- randomForest(total_cases ~ ., data=train, ntree=500)
summary(model)
pred <- round(predict(model, test))
writePredictor(pred)
head(predicted)
