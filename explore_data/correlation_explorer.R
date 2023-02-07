library(MASS)

identify_distribution_and_correlation <- function(df) {
  # Convert columns with character or string data to factors
  df[] <- lapply(df, function(col) {
    if (is.character(col) || is.factor(col)) {
      as.factor(col)
    } else {
      col
    }
  })
  
  result <- data.frame(Variable = colnames(df), Distribution = character(ncol(df)), Correlation = character(ncol(df)))
  
  for (i in 1:ncol(df)) {
    column <- df[, i]
    if (is.numeric(column)) {
      if (all(is.finite(column))) {
        # Use Shapiro-Wilk test to check for normality
        normality_test <- shapiro.test(column)
        if (normality_test$p.value > 0.05) {
          # Use Pearson correlation for normally distributed data
          result[i, "Distribution"] <- "Normal"
          result[i, "Correlation"] <- "Pearson"
        } else {
          result[i, "Distribution"] <- "Non-Normal"
          # Use Spearman correlation for non-normally distributed data
          result[i, "Correlation"] <- "Spearman"
        }
      } else {
        result[i, "Distribution"] <- "Not Finite"
        result[i, "Correlation"] <- NA
      }
    } else if (is.factor(column)) {
      levels <- levels(column)
      if (length(levels) == 2 && all(levels %in% c("0", "1"))) {
        # Use Point Biserial correlation for binary data
        result[i, "Distribution"] <- "Binary"
        result[i, "Correlation"] <- "Point Biserial"
      } else {
        # Use Polychoric correlation for categorical data
        result[i, "Distribution"] <- "Categorical"
        result[i, "Correlation"] <- "Polychoric"
      }
    } else {
      result[i, "Distribution"] <- "Other"
      result[i, "Correlation"] <- NA
    }
  }
  
  return(result)
}




set.seed(123)
df <- data.frame(
  var1 = rnorm(100),
  var2 = rgamma(100, shape = 3),
  var3 = rt(100, df = 5),
  var4 = rbinom(100, size = 10, prob = 0.5),
  var5 = rcauchy(100),
  var6 = rpois(100, lambda = 5),
  var7 = rexp(100, rate = 1),
  var8 = rchisq(100, df = 5),
  var9 = rf(100, df1 = 5, df2 = 5),
  var10 = runif(100, min = 0, max = 1),
  var11 = sample(c("A", "B", "C"), 100, replace = TRUE),
  var12 = sample(c("Yes", "No"), 100, replace = TRUE),
  var13 = sample(1:10, 100, replace = TRUE),
  var14 = sample(LETTERS[1:26], 100, replace = TRUE),
  var15 = sample(0:1, 100, replace = TRUE),
  var16 = sample(100:999, 100, replace = TRUE),
  var17 = sample(c(TRUE, FALSE), 100, replace = TRUE),
  var18 = as.Date("2021-01-01") + sample(0:365, 100, replace = TRUE),
  var19 = as.POSIXct("2021-01-01 00:00:00") + sample(0:86399, 100, replace = TRUE),
  var20 = sample(letters, 100, replace = TRUE)
)

result <- identify_distribution_and_correlation(df)



