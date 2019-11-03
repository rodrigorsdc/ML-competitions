library(keras)
library(corrplot)
library(dummies)
library(nnet)

TEST_MOD = FALSE
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





quakeX = read.csv('train_values.csv')
quakeY = read.csv('train_labels.csv')

quake = merge(quakeX, quakeY, by="building_id")
quakeTest = read.csv('test_values.csv')
test_building_id = quakeTest$building_id

quake1 = quake[quake$damage_grade == 1,]
quake2 = quake[quake$damage_grade == 2,]
quake3 = quake[quake$damage_grade == 3,]

quake1 = quake1[sample(nrow(quake1), 25000, replace=FALSE),]
quake2 = quake2[sample(nrow(quake2), 25000, replace=FALSE),]
quake3 = quake3[sample(nrow(quake3), 25000, replace=FALSE),]

## quake <- rbind(quake1, quake2, quake3)
quake13 <- rbind(quake1, quake3)
quake12 <- rbind(quake1, quake2)
quake23 <- rbind(quake2, quake3)
quake13 <- quake13[sample(nrow(quake13)),]
quake12 <- quake12[sample(nrow(quake12)),]
quake23 <- quake23[sample(nrow(quake23)),]
trainX <- train[, -ncol(train)]
trainY <- train[, c(1, ncol(train))]

quake$position = as.numeric(quake$position) - 1
quake$land_surface_condition = as.numeric(quake$land_surface_condition) - 1
quake$foundation_type = as.numeric(quake$foundation_type) - 1
quake$roof_type = as.numeric(quake$roof_type) - 1
quake$ground_floor_type = as.numeric(quake$ground_floor_type) - 1
quake$other_floor_type = as.numeric(quake$other_floor_type) - 1
quake$plan_configuration = as.numeric(quake$plan_configuration) - 1
quake$legal_ownership_status = as.numeric(quake$legal_ownership_status) - 1

quake$position = as.factor(quake$position)
quake$land_surface_condition = as.factor(quake$land_surface_condition)
quake$foundation_type = as.factor(quake$foundation_type)
quake$roof_type = as.factor(quake$roof_type)
quake$ground_floor_type = as.factor(quake$ground_floor_type)
quake$other_floor_type = as.factor(quake$other_floor_type)
quake$plan_configuration = as.factor(quake$plan_configuration)
quake$legal_ownership_status = as.factor(quake$legal_ownership_status)


quake$damage_grade = as.numeric(quake$damage_grade) - 1
quake$damage_grade = as.factor(quake$damage_grade)

alpha = ifelse(TEST_MOD == TRUE, 1.0, 0.8)
d = sort(sample(nrow(quake12), nrow(quake12)*alpha))
train12 = quake12[d,]
test12 = quake12[-d,]

alpha = ifelse(TEST_MOD == TRUE, 1.0, 0.8)
d = sort(sample(nrow(quake13), nrow(quake13)*alpha))
train13 = quake13[d,]
test13 = quake13[-d,]

alpha = ifelse(TEST_MOD == TRUE, 1.0, 0.8)
d = sort(sample(nrow(quake13), nrow(quake13)*alpha))
train23 = quake23[d,]
test23 = quake23[-d,]

M <- cor(quake, quake$damage_grade)
M
corrplot(M)

quakeTest$position = as.numeric(quakeTest$position) - 1
quakeTest$land_surface_condition =
    as.numeric(quakeTest$land_surface_condition) - 1
quakeTest$foundation_type = as.numeric(quakeTest$foundation_type) - 1
quakeTest$roof_type = as.numeric(quakeTest$roof_type) - 1
quakeTest$ground_floor_type = as.numeric(quakeTest$ground_floor_type) - 1
quakeTest$other_floor_type = as.numeric(quakeTest$other_floor_type) - 1
quakeTest$plan_configuration = as.numeric(quakeTest$plan_configuration) - 1
quakeTest$legal_ownership_status =
    as.numeric(quakeTest$legal_ownership_status) - 1

quakeTest$position = as.factor(quakeTest$position)
quakeTest$land_surface_condition =
    as.factor(quakeTest$land_surface_condition)
quakeTest$foundation_type = as.factor(quakeTest$foundation_type)
quakeTest$roof_type = as.factor(quakeTest$roof_type)
quakeTest$ground_floor_type = as.factor(quakeTest$ground_floor_type)
quakeTest$other_floor_type = as.factor(quakeTest$other_floor_type)
quakeTest$plan_configuration = as.factor(quakeTest$plan_configuration)
quakeTest$legal_ownership_status =
    as.factor(quakeTest$legal_ownership_status)

model12 <- multinom(damage_grade ~ foundation_type +
                      roof_type +  ground_floor_type +
                      other_floor_type +
                      has_superstructure_mud_mortar_stone +
                      has_superstructure_cement_mortar_brick, data=train12)

model13 <- multinom(damage_grade ~ foundation_type +
                      roof_type +  ground_floor_type +
                      other_floor_type +
                      has_superstructure_mud_mortar_stone +
                      has_superstructure_cement_mortar_brick, data=train13)


model23 <- multinom(damage_grade ~ foundation_type +
                      roof_type +  ground_floor_type +
                      other_floor_type +
                      has_superstructure_mud_mortar_stone +
                      has_superstructure_cement_mortar_brick, data=train23)


if (TEST_MOD == TRUE) {
    pp12 <- predict(model12, quakeTest, "probs")
    pp23 <- predict(model23, quakeTest, "probs")
    pred <- ifelse(pp12 < 0.3, 1, ifelse(pp23 > 0.55, 3, 2))
    predicted <- data.frame(building_id=test_building_id,
                        damage_grade=as.numeric(pred))
    write.csv(predicted, "predicted.csv", row.names=FALSE)
    table(pred)
} else {
    test <- rbind(test12, test23)
    test <- test[sample(nrow(test)),]
    pp12 <- predict(model12, test, "probs")
    pp23 <- predict(model23, test, "probs")
    pp23 <- ifelse(pp23 > 0.34, 3, 2)
    mean(pp23 == test23$damage_grade)
    table(test23$damage_grade)
    tail(test$damage_grade)
    pred <- ifelse(pp12 < 0.5, 1, ifelse(pp23 > 0.55, 3, 2))
    table(pred)
    mean(pred == test$damage_grade)
    f1_micro(test$damage_grade, pred)
}

