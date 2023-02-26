# Load the MASS package
library(MASS) # LDA
library(ggplot2) # Visualize
library(mda) # FDA
library(klaR) # RDA

# Load the iris dataset
data(iris)

# Plot the relationship between petal length and petal width, colored by species
ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  labs(x = "Petal Length", y = "Petal Width")

# Plot the relationship between sepal length and petal length, colored by species
ggplot(data = iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  labs(x = "Sepal Length", y = "Petal Length")

# Separate the data into predictors (X) and response (Y)
X <- iris[, 1:4]
Y <- iris[, 5]

######################################### Linear Discriminant Analysis (LDA)

# Fit the LDA model
lda_model <- lda(X, Y)

# Print the summary of the model
summary(lda_model)

# Make predictions on new data
new_data <- data.frame(Sepal.Length = c(5.1, 5.8, 6.7),
                       Sepal.Width = c(3.5, 2.7, 3.1),
                       Petal.Length = c(1.4, 4.1, 5.6),
                       Petal.Width = c(0.2, 1.0, 2.4))

predict(lda_model, newdata = new_data)


######################################### Quadratic Discriminant Analysis (QDA)
## assumption of equal covariance matrices across the response groups is relaxed

# Fit the QDA model
qda_model <- qda(X, Y)

# Print the summary of the model
summary(qda_model)

# Make predictions on new data
new_data <- data.frame(Sepal.Length = c(5.1, 5.8, 6.7),
                       Sepal.Width = c(3.5, 2.7, 3.1),
                       Petal.Length = c(1.4, 4.1, 5.6),
                       Petal.Width = c(0.2, 1.0, 2.4))

predict(qda_model, newdata = new_data)


######################################### Flexible Discriminant Analysis (FDA)
## covariance matrices are modeled using a smooth function of the predictors
## for when the assumption of equal covariance matrices across the response groups is not met

# Fit the FDA model
fda_model <- fda(Species ~ ., data = iris)

# Print the summary of the model
summary(fda_model)

# Make predictions on new data
new_data <- data.frame(Sepal.Length = c(5.1, 5.8, 6.7),
                       Sepal.Width = c(3.5, 2.7, 3.1),
                       Petal.Length = c(1.4, 4.1, 5.6),
                       Petal.Width = c(0.2, 1.0, 2.4))

predict(fda_model, newdata = new_data)


######################################### Regularized Discriminant Analysis (RDA)
#  improve the accuracy of the model when the number of predictors is large relative to the sample size

# Fit the RDA model
rda_model <- rda(X, Y, lambda = 0.5)

# Print the summary of the model
summary(rda_model)

# Make predictions on new data
new_data <- data.frame(Sepal.Length = c(5.1, 5.8, 6.7),
                       Sepal.Width = c(3.5, 2.7, 3.1),
                       Petal.Length = c(1.4, 4.1, 5.6),
                       Petal.Width = c(0.2, 1.0, 2.4))

predict(rda_model, newdata = new_data)

