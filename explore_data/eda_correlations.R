#' Example script using the easystats package
#' 
#' This script demonstrates various features of the easystats package for correlation analysis.
#' This script can be found here: https://easystats.github.io/correlation/
#' I saved this script to use it as a basis for a shiny app here: https://github.com/elkalytics/portfolio/blob/main/shiny_applications/correlations/02_shiny_correlations_comparison.R
#' 
#' @import easystats
#' @import BayesFactor
#' @import see
#' @import dplyr
#' @import ggplot2
#' 
#' @param iris A built-in R dataset containing measurements of iris flowers.
#' 
#' @return This script produces a variety of correlation analyses and visualizations.
#' 
#' @examples
#' library(easystats)
#' source("path/to/this/script.R")
#' 
#' @export
# Load packages
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