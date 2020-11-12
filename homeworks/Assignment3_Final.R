###############################################################################################
# Assignment 3
# October 28th, 2020
###############################################################################################
rm(list = ls())
###############################################################################################
# Loading Libraries
# install.packages as necessary
###############################################################################################
library(lintr)
library(styler)
library(tidyverse)
library(stats)
library(dbplyr)
library(data.table)

# Note from Assignment- In this homework, we will go through some exercises of control flow and functions. For all functions you write, I suggest you also test it on some simple problems. 

###############################################################################################
#Problem 1
###############################################################################################
# (1) -----
my_sum <- function(x){
  if (is.atomic(x = TRUE) && is.numeric(x)){ # This one has to be be modified, tried using && and || to add multiple conditions. Come back to this part. 
    print("x is an atomic vector & numeric")
  } else {
    print("Cannot be Processed")
    stop("Try Again!")
  }
  if(x){
    x <- x[!is.na(x)] # Removes the NA values, for the sum for loop to only sum integers.  
  }
   if(length(x > 0)){
    print("x has a length of more than zero")
    #return(x)
  } else {
    print("x has a length of zero")  # Can a vector have a length of zero?
    stop(length(x = 0))
  }
  sum <- 0
  for(i in x){
    #na.rm = TRUE
    sum <- sum + i
  }
  print(paste("Sum =", sum))
}

my_sum(c(1, 2, 3, NA)) # Testing

# (2) -----
my_mean <- function(x){
  my_sum(x)
    mean <- sum(x)/(length(x))
  print(paste("Mean =", mean))
  }

my_mean(c(1, 2, 3)) # Testing

# (3) -----
my_var <- function(x){
  my_sum(x)
  my_mean(x)
  {
    var <- sum((x - mean(x))^2)/(length(x) - 1)
  }
  print(paste(" Variance =", var))
}

my_var(c(1, 2 , 3, 4)) #Testing
# - Notes------------------------------------
# Methods for is.numeric should only return true if the base type of the class is double or integer and values can reasonably be regarded as numeric (e.g., arithmetic on them makes sense, and comparison should be done via the base type).
# - ! , the opposite of a command, logical not.
# The above function can be modified with ifelse(), instead of using several if() else. 

# Checking If built-in functions work --------------------------------------------------
x <- c(1, 2, 3)

my_sum <- sum(x, na.rm = TRUE)
my_sum

my_mean <- mean(x, na.rm = TRUE)
my_mean

my_var <- var(x, na.rm = TRUE)
my_var
###############################################################################################
#Problem 2
###############################################################################################
# -- Classical Fibonacci Sequence: 
# $$a_0 = 0, a_1 = 1, \ldots, a_n = a_{n - 1} + a_{n - 2}, \ldots$$

# (1) ----- Positive integers only. 
#
fib <- function(n) {
  if(!is.na(n) && n < 0){
    print("Error: Input contains negative integers") #Checking for negative integers
    return(n)
  }
  if (length(n) > 1) {
    return(sapply(n, fib))
  }
  if (n == 0) 
    return(0)
  if (n == 1) 
    return(1)
  while(is.numeric(n)){
    return(fib(n - 1) + fib(n - 2))  
  }
}

# Testing
fib(1)
fib(c(1, 2, 3))
fib(c(-1, 2, 3)) #This spits out an error as printed in the function. 

# (2) ----- Testing any startin values
fib <- function(n) {
  if(!is.na(n) && n < 0){
    print("Error: Input contains negative integers") #Checking for negative integers
    return(n)
  }
  if (length(n) > 1) {
    return(sapply(n, fib))
  }
  if (n == 0) 
    return(0)
  if (n == 1) 
    return(1)
  if (n < 0)
    return(fib(-1)*n*((-1)^((n + 1) %% 2)))  ### REVIEW THIS LINE OF CODE ###############
  while(is.numeric(n)){
    return(fib(n - 1) + fib(n - 2))  
  } 
}

# Testing
fib(c(1, 2, 3))
fib(c(-1, 2, 3)) #Still keep getting an error for this part. 
# - Notes------------------------------------
# Number two had to be further reviewed, as I can't seem to be able to properly process the function with negative integers. 
###############################################################################################
#Problem 3
###############################################################################################
# (1) -----
count <- function(vec, x){
  if(!is.atomic(vec)){
    print("Vector is not an atomic variable.")
    stop("Try Again!")
  }
  return(sum(vec == x)) #Not sure why the count( ) doesn't work in this case. Vector?
}

#Testing
count(c(1, 2, 3, 3), 3)

# (2) -----
count <- function(vec, x){
  if(!is.atomic(vec)){
    print("Vector is not an atomic variable.")
    stop("Try Again!")
  }
  return(sum(sapply(vec, function(n) all(x == n))))
}
count(c(1, 1), c(1, 1, 1))
count(c(1, 2), c(1, 2, 3))

# (3) -----
my_unique <- function(vec, return_counts = FALSE){
  return(unique(vec, incomparables = FALSE, return_counts))
}

#Testing
my_unique(c(1, 2, 3, 5, 1, 2, 3, 5))


###############################################################################################
#Problem 4
###############################################################################################
#Function that will help us with the binomial data computation.
# Our estimation follows equations:
# $$\hat{p} = \frac{1}{n}\sum_{i=1}^n X_i, \quad \hat{v} = \frac{\hat{p}(1 - \hat{p})}{n}$$


# (1) -----
# IMPORTANT ----- Following formulas were used - For proportion of successes:
# Estimation for mean(E(x) = p(hat)) = s/n = N(p)/N 
# Estimation for SD = sqrt(npq) = sqrt((p(hat)(1 - p(hat)))/ n)

# s = Sum of events (Np)
# n = Sample size   (N)

# (1) -----
# s and n have the same length.
binomial_fun <- function(s, n){
  if(is.numeric(c(s,n))){
    mean <- (s/n) # N(p) / N
  std <- (sqrt((mean*(1 - mean))/n)) # sqrt((p(hat)(1 - p(hat)))/ n)
    print(return(paste("Mean =", mean, "Standard Deviation =", std)))
  } else {
    print("Error: Your inputs are not numeric.")
    stop("Try Again!")
  }
}

#Testing
binomial_fun(10, 100)
binomial_fun(20, 100)
binomial_fun(30, 100)

# (2) -----
data <- function(s, n){
  if(is.numeric(c(s, n))){
    n <- data.table(n)  #data.table
    s <- data.table(s)  #data.table
    mean_result <- data.table((s$s/n$n)) # N(p) / N
    setnames(mean_result, "mean")
    data_final <- cbind(mean_result,n)
    # sqrt((p(hat)(1 - p(hat)))/ n)
    std <- data.table(sqrt(((data_final$mean)*(1-(data_final$mean)))/(data_final$n)))
    final <- cbind(mean_result, std)
    setnames(final, c("mean", "sd"))
    return(final)
  } else {
    print("Error: Your inputs are not numeric. ")
    stop("Try Again!")
  }
}

#Testing
data(c(10,20,30), 100)

# NOTES -----
# The above functions were produced based on the following functions:
# p(hat) = s/n = N(p) / N
# SD = sqrt((p(hat)(1 - p(hat)))/N)
# N(p) being th sum of events, and N the sample size.
# These functions were derived from the binomial distribution formula of probability of successes.
# ^^^The above formulas: For proportion of successes, not for the count of successes.
###############################################################################################
#Problem 5
###############################################################################################
# Polynomials
# Independent Variabe: x
# dependent variable: y
rm(list = ls())

# (1) -----
lin_fit <- function(x, y){
  return(lm(y ~ x))
}

#Testing 
lin_fit(c(0, 1, 2, 3), c(10, 20, 30, 40))

# (2) -----
poly_fit <- function(x, y, degree = 1){
  return(lm(y ~ poly(x, degree, raw = TRUE)))
}

#Testing
x <- c(12, 15, 24, 34, 45, 51)
y <- c(35, 45, 55, 65, 75, 85)
poly_fit(x, y)

# (3) -----
#(Hint: We could automatically infer the degree by the length of coef.)
poly_pred <- function(x, coef){
  return(poly(x,))
}


x <- c(10, 20, 30, 40, 50) 
y <- c(35, 45, 55)
poly_pred(x, y)


