dataTrain <- function() {

    trainX <- read.csv('dengue_features_train.csv')
    trainY <- read.csv('dengue_labels_train.csv')

    trainX$city <- as.numeric(trainX$city)
    trainX$week_start_date <- gsub("-", "", trainX$week_start_date)
    trainX$week_start_date<- as.numeric(trainX$week_start_date)

    trainY$city <- as.numeric(trainY$city)

    # NA missing
    for (n in names(trainX)) {
        idx_NA <- which(is.na(trainX[, n]))
        j <- 2
        for (i in idx_NA) {
            trainX[, n][i] <- trainX[, n][i-1]
            j <- j + 1
        }
    }
    total_cases <- trainY$total_cases
    return (cbind(trainX, total_cases))
}

dataTest <- function() {

    testX <- read.csv('dengue_features_test.csv')

    testX$city <- as.numeric(testX$city)
    testX$week_start_date <- gsub("-", "", testX$week_start_date)
    testX$week_start_date<- as.numeric(testX$week_start_date)

    sum(is.na(testX))
    # NA missing
    for (n in names(trainX)) {
        idx_NA <- which(is.na(testX[, n]))
        j <- 2
        for (i in idx_NA) {
            testX[, n][i] <- testX[, n][i-1]
            j <- j + 1
        }
    }
    return(testX)
}  

writePredictor <- function(predicted) {
    test <- dataTest()
    city <- ifelse(test$city == 1, "iq", "sj")
    predicted <- data.frame(city=city, year=test$year, weekofyear=test$weekofyear,
                            total_cases=pred)
    write.csv(predicted, 'predicted.csv', row.names=FALSE)
}
