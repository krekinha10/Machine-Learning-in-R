# Multiple Linear Regression

# Importing the dataset
data = read.csv('50_Startups.csv')


# Enconding the data
data$State = factor(data$State,
                    levels= c("New York","California","Florida"  ),
                    labels = c(1,2,3))

# SPlitting the dataset into the Training set and Test set 
library(caTools)
set.seed(123)
split = sample.split(data$Profit , SplitRatio = 0.8)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)


# Feature scaling 
#training_set[,2:3] = scale(training_set[,2:3])
#test_set[,2:3] = scale(test_set[,2:3])


# Fitting the Multiple Linear Regression to the Training set 
regressor = lm(formula = Profit ~ .,
               data = training_set)


# Predicting the Test Results
y_pred = predict(regressor, newdata = test_set)


# Building the optimal model using Backward Elimination
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data= data)

summary(regressor)



