# Source: https://easystats.github.io/correlation/

library(easystats)
library(BayesFactor)
library(see)
library(dplyr)

# Save correlation results
results <- correlation(iris)
results

# Summarize
summary(results, redundant = TRUE)

# Plot correlation matrix
results %>%
  summary(redundant = TRUE) %>%
  plot()

# Plot scatter
plot(cor_test(iris, 
              "Sepal.Width", "Sepal.Length"))

# Grouped correlations
iris %>%
  dplyr::select(Species, Sepal.Length, Sepal.Width, Petal.Width) %>%
  group_by(Species) %>%
  correlation()

# Bayesian correlations
correlation(iris, 
            bayesian = TRUE)

# Tetrachoric, Polychoric, Biserial, Biweight
correlation(iris, 
            include_factors = TRUE, method = "auto")

# Summarize
iris %>%
  correlation(partial = TRUE) %>%
  summary()

# Multi-level correlations
iris %>%
  correlation(partial = TRUE, multilevel = TRUE) %>%
  summary()

# Multi-level correlations without partial
iris %>%
  correlation(partial = FALSE, multilevel = TRUE) %>%
  summary()




