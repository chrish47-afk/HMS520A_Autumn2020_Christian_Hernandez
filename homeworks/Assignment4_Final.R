###############################################################################################
# Assignment 4
# November 11th, 2020
###############################################################################################
rm(list = ls())
###############################################################################################
# Loading Libraries
# install.packages as necessary(Updated R version)
install.packages("dplyr")
install.packages("tidyverse")
install.packages("data.table")
install.packages("broom")
install.packages("lintr")
install.packages("reshape")
###############################################################################################
library(lintr)
library(broom)
library(tidyverse)
library(stats)
library(dplyr)  
library(data.table)
library(tidyr)
library(reshape)
library(ggplot2)
###############################################################################################
#Problem 1
###############################################################################################
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

#Testing
data(iris)
summarise_group_data(data = iris, group_id = 'Species', obs = c('Petal.Length', 'Sepal.Length'), fun = sd)

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


###############################################################################################
#Problem 3
###############################################################################################
#WHO Global Tuberculosis Report(denoted as df_tb)
WHO_data <- tidyr::who %>% data.table()

# (1) - df_tb(Following Instructions) -------------------------------------------------
# Following instructions from section 12.6, cleaning df_tb
who1 <- WHO_data %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  )
who1

##### Part by Part, FROM EXAMPLE
# who1
who1 %>% 
  count(key)
# who2
who2 <- who1 %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2
#who3
who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_")
who3
#who4
who3 %>% 
  count(new)

who4 <- who3 %>%
  select(-new, -iso2, -iso3)
#who5
who5 <- who4 %>%
  separate(sexage, c("sex", "age"), sep = 1)
who5

#### A MORE COMPLEX/EFFICIENT PIPE(Procedure)
WHO_data_mod <- WHO_data %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

# Same results
who5 # Part by Part
WHO_data #Script

# (2)  - Same Procedure for The World Bank Population---------------------------------------

# NOTE - All these files were sourced locally, and not through the cluster. Hence, same files must be sourced everytime this code is processed. 
# No Edits - Original File. (RAW FILE)
WB_data <- read.csv("C:\\Users\\chris\\Desktop\\API_SP.POP.TOTL_DS2_en_csv_v2_1637443.csv", fill = TRUE, header = TRUE, skip = 3, check.names = FALSE) %>% data.table()

#Dropping Columns Country Code, Indicator Name, and Indicator Code
WB_data$`Country Code` <- NULL
WB_data$`Indicator Name` <- NULL
WB_data$`Indicator Code` <- NULL

#Melting Data, instead of using pivot_longer
WB_data_mod <- melt(WB_data, id = "Country Name")
#IMPORTANT NOTE: melt( ) can also work, instead of using pivot_longer. If this alternative way is used, it will do it by year instead of by Country Name. 


# (3) - Merging Data, instead of inner_join-------------------------------------------------- 
#Changing names to properly get both tables to efficiently have the same column names.
WHO_data_mod <- setnames(WHO_data_mod, old = "country", new = "Country Name")
WB_data_mod <- setnames(WB_data_mod, old = c("variable", "value"), new = c("year", "pop"))


#Incident Ratios
incidence_ratios <- merge(WHO_data_mod, WB_data_mod, by = c("Country Name", "year"))
incidence_ratios$ratios <- (incidence_ratios$cases/incidence_ratios$pop)

# (4) - Regression Coefficients--------------------------------------------------------------
WHO_WB_data <- regress_group_data(data = incidence_ratios, group_id = 'Country Name', obs = 'ratios', covs = 'year', include_intercept = TRUE)
WHO_WB_data <- setnames(WHO_WB_data, old = 'Country Name', new = 'CountryName')

# (5) - Bar Plot-----------------------------------------------------------------------------
ggplot(WHO_WB_data, aes(x = reorder(CountryName, -year), y = year)) + #Adjusting data order
  geom_bar(position = "dodge", stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Regression Analysis of Incidence Ratios by Year for Each Country from 1980 to 2014") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Regression Coefficient") +
  xlab("Country Name")





