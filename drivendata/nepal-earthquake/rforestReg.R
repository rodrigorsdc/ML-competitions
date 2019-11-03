library(randomForest)

TEST = FALSE
alpha = 0.8

f1_micro <- function(y_true, y_pred) {
    confusion <- table(y_true, y_pred)
    TP1 <- confusion[1, 1]
    TP2 <- confusion[2, 2]
    TP3 <- confusion[3, 3]
    FP1 <- sum(confusion[, 1]) - TP1
    FP2 <- sum(confusion[, 2]) - TP2
    FP3 <- sum(confusion[, 3]) - TP3
    FN1 <- sum(confusion[1, ]) - TP1
    FN2 <- sum(confusion[2, ]) - TP2
    FN3 <- sum(confusion[3, ]) - TP3
    Pmicro <- (TP1 + TP2 + TP3) / (TP1 + TP2 + TP3 + FP1 + FP2 + FP3)
    Rmicro <- (TP1 + TP2 + TP3) / (TP1 + TP2 + TP3 + FN1 + FN2 + FN3)
    Fmicro <- (2 * Pmicro * Rmicro) / (Pmicro + Rmicro)
    return(Fmicro)
}


trainX = read.csv('train_values.csv', row.names=NULL)
trainY = read.csv('train_labels.csv', row.names=NULL)
quake = merge(trainX, trainY, by="building_id", by.x="building_id", by.y="building_id")

testX = read.csv('test_values.csv', row.names=NULL)
test_building_id = testX$building_id

remove_predictors <- c(1, 4, 3, 39, 38, 37, 36, 35, 34, 33)
quake = quake[, -remove_predictors]
testX = testX[, -remove_predictors]
quake$geo_level_1_id = as.factor(quake$geo_level_1_id)
testX$geo_level_1_id = as.factor(testX$geo_level_1_id)

quake1 = quake[quake$damage_grade == 1,]
quake2 = quake[quake$damage_grade == 2,]
quake3 = quake[quake$damage_grade == 3,]
nrow(quake1)
nrow(quake2)
nrow(quake3)
quake1 = quake1[sample(nrow(quake1), 25000),]
quake2 = quake2[sample(nrow(quake2), 100000),]
quake3 = quake3[sample(nrow(quake3), 80000),]

quake = rbind(quake1, quake2, quake3)
quake = quake[sample(nrow(quake)),]
## quake$damage_grade = factor(quake$damage_grade)

if (TEST == TRUE){    
    train <- rep(TRUE, nrow(quake))
} else {
    train <- sample(c(TRUE, FALSE), nrow(quake), TRUE, c(alpha, 1.0 - alpha))
}
model <- randomForest(damage_grade ~ ., data=quake, subset=train, ntree = 500)
if (TEST == TRUE) {
    pred <- predict(model, newdata=testX)
    predicted <- data.frame(building_id=test_building_id, damage_grade=pred)
    write.csv(predicted, "predicted.csv", row.names=FALSE)
} else {
    pred <- predict(model, newdata=quake[-train,])
    ## pred <- factor(pred, ordered=TRUE)
    mean (pred == quake[-train,]$damage_grade)
    print(f1_micro(quake[-train,]$damage_grade, pred))
}

