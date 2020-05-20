# Simple Linear Regression

# Data Preprocessing

# Import the dataset
data = read.csv('Salary_Data.csv')

# Spliting the dataset into Training and Test set
library(caTools)
set.seed(123)
split = sample.split(data$Salary, SplitRatio = 2/3)
training_set = subset(data,split == TRUE)
test_set = subset(data,split == FALSE)

# Fitting Simple Linear Regression to the Training set

regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)



# Predicting the Test set Results
y_pred = predict(regressor,newdata = test_set)


# Plotting the training set Results
#install.packages('ggplot2')
library(ggplot2)

ggplot()+
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor,
                                                              newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience(Training set)',) +
  xlab('Years of Experience') +
  ylab('Salary')

# Plotting the Test set Results

ggplot()+
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor,
                                                              newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience(Test set)',) +
  xlab('Years of Experience') +
  ylab('Salary')



