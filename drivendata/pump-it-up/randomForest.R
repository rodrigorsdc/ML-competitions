library(randomForest)

alpha = 1.0

pumpX <- read.csv('trainX.csv')
testX <- read.csv('testX.csv')
pumpX$date_recorded <- gsub("-", "", pumpX$date_recorded)
testX$date_recorded <- gsub("-", "", testX$date_recorded)
rm_pred <- c("subvillage", "scheme_name", "installer", "ward", "wpt_name",
             "funder", "date_recorded", "lga", "extraction_type",
             "scheme_management")
for (n in rm_pred) {
    pumpX[, n] <- as.numeric(pumpX[, n])
    testX[, n] <- as.numeric(testX[, n])
}

pumpX$id <- NULL
test_id <- testX$id
testX$id <- NULL
pumpY <- read.csv('trainY.csv')

rf <- randomForest(pumpX, pumpY$status_group, ntree = 500)
rf
pred <- predict(rf, testX)
predicted <- data.frame(id=test_id, status_group=pred)

write.csv(predicted, "predicted.csv", row.names=FALSE)
