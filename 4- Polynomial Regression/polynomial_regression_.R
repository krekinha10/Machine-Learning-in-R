# Polynomial Regression

# Data Preprocessing

data = read.csv('Position_Salaries.csv')
data = data[2:3]

library(caTools)
library (ggplot2)

# Fitting the Linear Regression to the dataset
lin_reg = lm(formula = Salary ~ ., 
             data = data)
summary(lin_reg)

# Fitting Polynomial Regression to the dataset
data$Level2 = data$Level^2
data$Level3 = data$Level^3
data$Level4 = data$Level^4

poly_reg = lm(formula = Salary ~ ., 
                 data = data)


# Visualising the Linear Regression results
ggplot()+
  geom_point(aes(x= data$Level, y=data$Salary ), 
             color = 'red') +
  geom_line(aes(x= data$Level,y= predict(lin_reg,newdata = data)),
            color = 'blue') +
  ggtitle('Truth or Bluff(Linear Regression)') +
  xlab('Level')+
  ylab('Salary')


# Visualising the Polynomial regression results
ggplot()+
  geom_point(aes(x= data$Level, y=data$Salary ), 
             color = 'red') +
  geom_line(aes(x= data$Level,y= predict(poly_reg,newdata = data)),
            color = 'blue') +
  ggtitle('Truth or Bluff(Polynomial Regression)') +
  xlab('Level')+
  ylab('Salary')


# Predicting a new result with Linear Regression
y_pred = predict(lin_reg,newdata = data.frame(Level = 6.5))



# Predicting a new result with Polynomial Regression
y_pred_poly = predict(poly_reg,newdata = data.frame(Level = 6.5,
                                               Level2 = 6.5^2,
                                               Level3 = 6.5^3,
                                               Level4 = 6.5^4))






