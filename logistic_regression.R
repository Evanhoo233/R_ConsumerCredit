# Logistic Regression

# Importing the dataset
train = read.csv('ConsumerCred-train.csv')
train = train[2:12]

test=read.csv('ConsumerCred-test.csv')
test=test[2:11]


train$MonthlyIncome = ifelse(is.na(train$MonthlyIncome),
                               ave(train$MonthlyIncome, FUN = function(x) mean(x, na.rm = TRUE)),
                               train$MonthlyIncome)

train$NumberOfDependents = ifelse(is.na(train$NumberOfDependents),
                                    ave(train$NumberOfDependents, FUN = function(x) mean(x, na.rm = TRUE)),
                                  train$NumberOfDependents)

test$MonthlyIncome = ifelse(is.na(test$MonthlyIncome),
                             ave(test$MonthlyIncome, FUN = function(x) mean(x, na.rm = TRUE)),
                            test$MonthlyIncome)

test$NumberOfDependents = ifelse(is.na(test$NumberOfDependents),
                                  ave(test$NumberOfDependents, FUN = function(x) mean(x, na.rm = TRUE)),
                                  test$NumberOfDependents)

# Feature Scaling
train[,2:11] = scale(train[,2:11])
test[,1:10] = scale(test[1:10])

# Fitting Logistic Regression to the Training set
classifier = glm(formula = SeriousDlqin2yrs~ .,
                 
                 data = train)

# Predicting the Test set results
y_pred = predict(classifier, type = 'response', newdata = test[1:10])

y_pred

write.csv(y_pred,file="D:/Textbooks/y_pred.csv",quote=F,row.names = F)
