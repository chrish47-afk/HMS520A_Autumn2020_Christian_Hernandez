library(dplyr)
library(data.table)
# data - data frame that contains all the information
# group_id - A vector with one or multiple column names that uniquely define the groups.
# obs - A vector with one or multiple column names of targeting observations
# fun - A summary function
# ... - Extra arguments for the summary function
## The function should return a data frame with 'group_id' column(s) and all the summary staistics

summarise_group_data <- function(data, group_id, obs, fun, ...){
  data %>%
    group_by_at(group_id) %>%
    summarise_at(.vars = vars(obs), .funs = fun, ...)
  }


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


