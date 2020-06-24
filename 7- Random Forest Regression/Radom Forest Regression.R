# Decision Tree Regression

# Importing the dataset
data = read.csv('Position_Salaries.csv')
data = data[2:3]

# Fitting Decision Tree Regression to the Dataset
# Only run the following line if you do not have the rpart on your R release 
# (or if it is the first time that you use it) to use it take of the # symbol and run it
#install.packages('rpart')

library(rpart)
regressor = rpart(formula = Salary ~ Level,
                  data = data,
                  control = rpart.control(minsplit = 1))

# Predicting a new result
y_pred = predict(regressor,data.frame(Level = 6.5))


# Visualising the Decision Tree Regression results
library(ggplot2)
ggplot()+
  geom_point(aes(x = data$Level, y = data$Salary),
             colour = 'red') +
  geom_line(aes(x = data$Level, y = predict(regressor,newdata = data)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression)') +
  xlab('Level') + 
  ylab('Salary')


# Visualising the Regression Model results(for higher resolution and smoother curve)

x_grid = seq(min(data$Level), max(data$Level), 0.1)
ggplot()+
  geom_point(aes(x = data$Level, y = data$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor,newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression)') +
  xlab('Level') + 
  ylab('Salary')
