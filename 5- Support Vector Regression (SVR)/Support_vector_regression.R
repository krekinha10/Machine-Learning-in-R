# Support Vector Regression

# Uploading the dataset
data = read.csv('Position_Salaries.csv')
data = data[2:3]



# Fitting the SVR model to the dataset
# Only run the following line if you do not have the e1071 on your R release 
# (or if it is the first time that you use it) to use it take of the # symbol and run it
#install.packages('e1071') 
library(e1071)
regressor = svm(formula = Salary ~ .,
                data = data,
                type = 'eps-regression')


# Predicting a new result 
y_pred = predict(regressor, data.frame(Level = 6.5))


# Visualising the SVR results
# Only run the following line if you do not have the ggplot2 on your R release 
# (or if it is the first time that you use it) to use it take of the # symbol and run it
#install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = data$Level,y= data$Salary),
             color = 'red') +
  geom_line(aes(x = data$Level,y= predict(regressor, newdata = data)),
            color = 'blue') +
  ggtitle('Truth or Bluff (SVR)') +
  xlab('Level') +
  ylab('Salary')
