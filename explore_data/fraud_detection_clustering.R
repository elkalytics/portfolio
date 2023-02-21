# load required packages
library(mvtnorm)
library(cluster)

# set seed for reproducibility
set.seed(123)

# simulate data for two groups
n <- 100 # number of observations in each group
mean_normal <- c(0, 0) # mean for normal group
mean_fraud <- c(5, 5) # mean for fraudulent group
cov <- matrix(c(1, 0.5, 0.5, 1), ncol = 2) # covariance matrix

normal_data <- rmvnorm(n, mean = mean_normal, sigma = cov) # normal data
fraud_data <- rmvnorm(n, mean = mean_fraud, sigma = cov) # fraudulent data

# combine data and create labels
data <- rbind(normal_data, fraud_data)
labels <- rep(c("Normal", "Fraudulent"), each = n)

# perform cluster analysis using k-means
k <- 2 # number of clusters
kmeans_result <- kmeans(data, k)

# visualize cluster results
plot(data, col = kmeans_result$cluster, main = "Cluster Analysis Results", 
     xlab = "Variable 1", ylab = "Variable 2", pch = 20)
legend("topleft", legend = unique(labels), col = 1:length(unique(labels)), pch = 20)

# determine which group is fraudulent based on cluster centroids
centroids <- kmeans_result$centers
distances <- apply(centroids, 1, function(x) sqrt(sum((x - centroids)^2)))
fraud_group <- which.max(distances)

# print results
if(fraud_group == 1) {
  cat("The fraudulent group is 'Normal'\n")
} else {
  cat("The fraudulent group is 'Fraudulent'\n")
}
