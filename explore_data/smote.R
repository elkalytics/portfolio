#' Synthetic Minority Oversampling Technique (SMOTE)
#'
#' This function implements the Synthetic Minority Oversampling Technique (SMOTE) algorithm to oversample the minority class in an imbalanced dataset by creating synthetic instances.
#'
#' @param x A matrix or data frame containing the predictor variables.
#' @param y A vector containing the response variable.
#' @param k An integer specifying the number of nearest neighbors to use when creating synthetic instances. The default value is 5.
#' @param oversample A numeric value specifying the desired ratio of the number of instances in the majority class to the number of instances in the minority class after oversampling. For example, if oversample = 2, the number of instances in the majority class will be twice the number of instances in the minority class after oversampling. The default value is 1.
#' @return A list with two elements: x, a matrix or data frame containing the balanced predictor variables, and y, a vector containing the balanced response variable.
#'
#' @examples
#' # Load the imbalanced dataset
#' data(iris)
#' table(iris$Species)
#'
#' # Create an imbalanced dataset by removing some instances of the majority class
#' iris_imbalanced <- iris[-c(1:25),]
#'
#' # Apply SMOTE to oversample the minority class
#' iris_balanced <- smote(iris_imbalanced[,1:4], iris_imbalanced$Species, k = 5, oversample = 1)
#'
#' # Check the class distribution of the balanced dataset
#' table(iris_balanced$y)
#'
smote <- function(x, y, k = 5, oversample = 1) {
  # Split the data into majority and minority classes
  majority <- x[y == levels(y)[1],]
  minority <- x[y != levels(y)[1],]
  
  # Determine the number of synthetic instances to create
  n <- round(oversample * nrow(majority) - nrow(minority))
  
  # If there are no synthetic instances to create, return the original dataset
  if (n <= 0) {
    return(list(x = x, y = y))
  }
  
  # Find the k-nearest neighbors for each minority class instance
  nn <- as.matrix(dist(rbind(majority, minority)))[(nrow(majority) + 1):(nrow(majority) + nrow(minority)), 1:nrow(majority)]
  
  # Create the synthetic instances by interpolating between the minority class instance and its k-nearest neighbors
  synth <- minority + matrix(apply(nn, 2, function(x) {
    diff <- majority[x[2],] - minority[x[1],]
    minority[x[1],] + runif(k, 0, 1) * diff
  }), ncol = n, byrow = TRUE)
  
  # Combine the original and synthetic instances and their labels
  x_new <- rbind(x, synth)
  y_new <- factor(c(y, rep(levels(y)[-1], each = n)))
  
  # Return the balanced dataset
  return(list(x = x_new, y = y_new))
}