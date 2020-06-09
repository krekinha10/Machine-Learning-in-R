# Eclat Method

# Importing the mall dataset
dataset <- read.csv('Market_Basket_Optimisation.csv', header = FALSE)


# Creating a sparse matrix
library(arules)
dataset <- read.transactions('Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = TRUE)
summary(dataset)
itemFrequencyPlot(dataset, topN = 50)

# Training Eclat on the dataset
rules = eclat(data = dataset, parameter = list(support = 0.004, minlen = 2))


# Visualising the results
inspect(sort(rules, by = 'support')[1:10])
