#' Example R Script with Roxygen Comments
#'
#' This script illustrates how to perform a factorial type III ANOVA and 
#' post hoc tests on an example data set. It also includes lines on how to check
#' assumptions.
#' 
#' @importFrom car library
#' @importFrom effsize library
#' @importFrom sjPlot library
#'
#' @param factor1 A factor variable with levels A and B
#' @param factor2 A factor variable with levels X and Y
#' @param variable A numeric variable
#' 
#' @return A Tukey HSD test result object
#'
#' @examples
#' # Run the example script
#' source("example_script.R")
#'
#' # View the Tukey HSD test results
#' print(tukey)
#'
#' @export
#'

# Load the necessary packages
library(car)
library(effsize)
library(sjPlot)

# Create example data set
factor1 <- rep(c("A", "B"), each = 6)
factor2 <- rep(c("X", "Y"), times = 6)
variable <- c(10, 12, 14, 18, 20, 22, 25, 27, 29, 33, 35, 37)

data <- data.frame(factor1, factor2, variable)

# Check the structure of the data set
str(data)

# Check the summary of the data set
summary(data)

# Check for missing values
sum(is.na(data))

# Check normality assumptions with Shapiro-Wilk test
shapiro.test(data$variable)

# Check homogeneity of variances with Levene's test
data$interaction <- interaction(data$factor1, data$factor2)
leveneTest(variable ~ interaction, data = data)

# Run the Type III ANOVA
model <- aov(variable ~ factor1 * factor2, data = data)
anova <- Anova(model, type="III")

# Check the effect size
etasq(model)

# Check for interaction effect
plot_model(model, type = "pred", terms = c("factor1", "factor2"))

# Perform Tukey's pairwise post hoc tests
tukey <- TukeyHSD(model)
tukey
