# - Practice Package Creation
## - Using Homework Functions
install.packages('dplyr')

library(dplyr)
library(data.table)

#' Get summary and regression of data frame
#'
#' @param data  A Data Frame
#' @param group_id Group Identification of data frame
#' @param obs Observations inside the data frame
#' @param fun Type of function
#' @param ... Additional arguments inside function
#'
#' @return A data frame in the desired group and/or observation
#' @export
#'
#' @examples
#' summarise_group_data <- function(data = iris.....)
summarise_group_data <- function(data, group_id, obs, fun, ...){
  data %>%
    group_by_at(group_id) %>%
    summarise_at(.vars = vars(obs), .funs = fun, ...)
}


###############################################################################################
#Problem 2
###############################################################################################
regress_group_data <- function(data, group_id, obs, covs, include_intercept = TRUE, ...){
  formula <- paste0(obs, '~', covs)
  covs <- paste0(covs, collapse = '+')
  final <- data %>% group_by_at(group_id) %>%
    do(tidy(lm(formula = as.formula(formula), data = . ))) %>%
    select(-std.error, -statistic, -p.value)
  mod <- pivot_wider(final, names_from = 'term', values_from = 'estimate')
  return(mod)
  if(!include_intercept)
    final_ni <- select(.data = mod, -intercept)
  return(final_ni)
}




