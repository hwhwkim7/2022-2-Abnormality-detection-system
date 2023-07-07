install.packages("recipes", type='binary')
library(solitude)
library(caret)

train = read.csv("C:/PythonStudy/train_2.csv", fileEncoding = 'CP949') # train
test = read.csv("C:/PythonStudy/test_2.csv", fileEncoding = 'CP949') # test

## Tool로 구별
train1 = train[train$TOOL_NAME == 'Tool1', 9:50]
train2 = train[train$TOOL_NAME == 'Tool2', 9:50]
train3 = train[train$TOOL_NAME == 'Tool3', 9:50]
train4 = train[train$TOOL_NAME == 'Tool4', 9:50]

test1 = test[test$TOOL_NAME == 'P8TCVD060306', 9:50]
test2 = test[test$TOOL_NAME == 'P8TCVD060307', 9:50]
test3 = test[test$TOOL_NAME == 'P8TCVD060308', 9:50]
test4 = test[test$TOOL_NAME == 'P8TCVD060309', 9:50]

## Isolation Forest

# training data로 학습
iforest1 = isolationForest$new()
iforest1$fit(train1)
iforest2 = isolationForest$new()
iforest2$fit(train2)
iforest3 = isolationForest$new()
iforest3$fit(train3)
iforest4 = isolationForest$new()
iforest4$fit(train4)

# 학습된 모델에 testing data 적용
preds1 = iforest1$predict(test1)
indices1 = preds1[which(preds1$anomaly_score > 0.592)]
preds2 = iforest2$predict(test2)
indices2 = preds2[which(preds2$anomaly_score > 0.596)]
preds3 = iforest3$predict(test3)
indices3 = preds3[which(preds3$anomaly_score > 0.6037)]
preds4 = iforest4$predict(test4)
indices4 = preds4[which(preds4$anomaly_score > 0.608)]

pred_normal1 = test1[indices1$id, ]
pred_abnormal1 = test1[-indices1$id, ]
pred_normal2 = test2[indices2$id, ]
pred_abnormal2 = test2[-indices2$id, ]
pred_normal3 = test3[indices3$id, ]
pred_abnormal3 = test3[-indices3$id, ]
pred_normal4 = test4[indices4$id, ]
pred_abnormal4 = test4[-indices4$id, ]

# 예측 결과
cat(nrow(pred_normal1), nrow(pred_abnormal1))
cat(nrow(pred_normal2), nrow(pred_abnormal2))
cat(nrow(pred_normal3), nrow(pred_abnormal3))
cat(nrow(pred_normal4), nrow(pred_abnormal4))

# 그래프
par(mfrow = c(2, 2))
plot(preds1$anomaly_score, col='blue', pch=16, main='Tool1')
abline(h = 0.592, col='red', lwd=3)
abline(v = 319, col='Gold', lwd=2)
abline(v = 479, col='Gold', lwd=2)

plot(preds2$anomaly_score, col='blue', pch=16, main='Tool2')
abline(h = 0.596, col='red', lwd=3)
abline(v = 202, col='Gold', lwd=2)
abline(v = 237, col='Gold', lwd=2)

plot(preds3$anomaly_score, col='blue', pch=16, main='Tool3')
abline(h = 0.6037, col='red', lwd=3)
abline(v = 191, col='Gold', lwd=2)
abline(v = 697, col='Gold', lwd=2)

plot(preds4$anomaly_score, col='blue', pch=16, main='Tool4')
abline(h = 0.608, col='red', lwd=3)
abline(v = 197, col='Gold', lwd=2)
abline(v = 482, col='Gold', lwd=2)


## confusion matrix
actual1 = test[test$TOOL_NAME == 'P8TCVD060306', 51]
predicted1 = preds1$anomaly_score
predicted1[predicted1>0.592] = "Abnormal"
predicted1[predicted1<=0.592] = "Normal"
confusion1 = confusionMatrix(as.factor(actual1), as.factor(predicted1))

actual2 = test[test$TOOL_NAME == 'P8TCVD060307', 51]
predicted2 = preds2$anomaly_score
predicted2[predicted2>0.596] = "Abnormal"
predicted2[predicted2<=0.596] = "Normal"
confusion2 = confusionMatrix(as.factor(actual2), as.factor(predicted2))

actual3 = test[test$TOOL_NAME == 'P8TCVD060308', 51]
predicted3 = preds3$anomaly_score
predicted3[predicted3>0.6037] = "Abnormal"
predicted3[predicted3<=0.6037] = "Normal"
confusion3 = confusionMatrix(as.factor(actual3), as.factor(predicted3))

actual4 = test[test$TOOL_NAME == 'P8TCVD060309', 51]
predicted4 = preds4$anomaly_score
predicted4[predicted4>0.608] = "Abnormal"
predicted4[predicted4<=0.608] = "Normal"
confusion4 = confusionMatrix(as.factor(actual4), as.factor(predicted4))

confusion1
confusion2
confusion3
confusion4

