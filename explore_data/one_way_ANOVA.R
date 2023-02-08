# Function to loop a one-way ANOVA across data

one_way_ANOVA <- function(df) {
  require(tidyverse)
  
  results_list <- list()
  for (num_var in names(df)[which(sapply(df, is.numeric))]) {
    for (factor in names(df)[which(sapply(df, is.factor))]) {
      aov_res <- aov(df[[num_var]] ~ df[[factor]])
      p_value <- round(summary(aov_res)[[1]][["Pr(>F)"]][1], 3)
      if (p_value > 0) {
        results_list[[paste0(factor, "_", num_var)]] <- data.frame(
          variable = factor,
          dependent_variable = num_var,
          variable_type = "factor",
          dependent_variable = num_var,
          dependent_variable_type = "numeric",
          p_value = p_value
        )
      }
    }
  }
  
  bind_rows(results_list)
}



# Create fake data
set.seed(123)

df <- data.frame(
  factor1 = factor(rep(c("A", "B", "C"), each = 100)),
  factor2 = factor(rep(c("D", "E", "F"), 100, replace = TRUE)),
  factor3 = factor(rep(c("G", "H", "I"), 100, replace = TRUE)),
  num_var1 = rnorm(300, mean = 10, sd = 2),
  num_var2 = rnorm(300, mean = 5, sd = 1),
  num_var3 = rnorm(300, mean = 8, sd = 3),
  num_var4 = rnorm(300, mean = 20, sd = 4),
  num_var5 = rnorm(300, mean = 15, sd = 5)
)


# Apply function
one_way_ANOVA(df) -> ANOVAs

