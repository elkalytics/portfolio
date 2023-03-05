#' Perform cluster analysis using k-means
#'
#' This function illustrates how to perform cluster analysis on simulated data for 
#' two groups using the k-means algorithm, and visualizes the results.
#'
#' @param n the number of observations in each group
#' @param mean_normal the mean for the normal group
#' @param mean_fraud the mean for the fraudulent group
#' @param cov the covariance matrix
#' @param k the number of clusters
#' @return The group identified as fraudulent based on the cluster centroids
#' @export
#' @examples
#' data <- simulate_data(n = 100, mean_normal = c(0, 0), mean_fraud = c(5, 5), cov = matrix(c(1, 0.5, 0.5, 1), ncol = 2))
#' fraudulent_group <- kmeans_cluster(data, k = 2)

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