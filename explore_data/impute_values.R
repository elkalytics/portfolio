#' Impute missing values in a column
#'
#' This function imputes missing values in a column using various methods. It performs value imputation using the zero, mean, and median methods, as well as multiple imputation using the PMM, CART, and LASSO methods. It also performs imputation using the MissForest algorithm.
#' 
#' @param df A data frame.
#' @param col_name The name of the column to impute.
#'
#' @return A list containing data frames of the imputed values using different methods.
#' 
#' @importFrom mice mice
#' @importFrom missForest missForest
#' @importFrom dplyr mutate_if is.factor
#' 
#' @examples
#' library(titanic)
#' data("titanic_train")
#' impute_values(titanic_train,"Age")
#'
#' @export
# Load packages
library(mice)
library(missForest)
library(dplyr)
# Save function
impute_values <- function(df, col_name) {
  
  # Load required libraries
  library(mice)
  library(missForest)
  
  # Create a numeric version of the data frame for mice imputation
  df_numeric <- df %>% mutate_if(is.factor, as.numeric)
  
  # Value imputation
  value_imputed <- data.frame(
    original = df[[col_name]],
    imputed_zero = replace(df[[col_name]], is.na(df[[col_name]]), 0),
    imputed_mean = replace(df[[col_name]], is.na(df[[col_name]]), mean(df[[col_name]], na.rm = TRUE)),
    imputed_median = replace(df[[col_name]], is.na(df[[col_name]]), median(df[[col_name]], na.rm = TRUE))
  )
  
  # Mice imputation
  mice_imputed <- data.frame(
    original = df[[col_name]],
    imputed_pmm = complete(suppressWarnings(mice(df_numeric, method = "pmm", print=FALSE)))[[col_name]],
    imputed_cart = complete(suppressWarnings(mice(df_numeric, method = "cart", print=FALSE)))[[col_name]],
    imputed_lasso = complete(suppressWarnings(mice(df_numeric, method = "lasso.norm", print=FALSE)))[[col_name]]
  )
  
  # MissForest imputation
  missForest_imputed <- data.frame(
    original = df_numeric[[col_name]],
    imputed_missForest = missForest(select_if(df_numeric,is.numeric))$ximp[[col_name]]
  )
  
  return(list(value_imputed=value_imputed,mice_imputed=mice_imputed,missForest_imputed=missForest_imputed))
}