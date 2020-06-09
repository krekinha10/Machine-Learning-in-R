# Hierarchical Clustering

# Importing the mall dataset
dataset <- read.csv('Mall_Customers.csv')
X <- dataset[4:5]

# Using the dendrogram method to find the optimal number of clusters
dendogram <- hclust(dist(X, method = 'euclidean'), method = 'ward.D')
plot(dendogram,
     main = paste('Dendogram'),
     xlab = 'Customers',
     ylab = 'Euclidean Distances')


# Fitting the Hierarchical Clustering model to the DATASET
hc <- hclust(dist(X, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 5)


# Visualizing the clusters
library(cluster)
clusplot(X,
         y_hc,
         lines = 0, 
         shades = TRUE,
         color =TRUE, 
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Cluster of Clients'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')


