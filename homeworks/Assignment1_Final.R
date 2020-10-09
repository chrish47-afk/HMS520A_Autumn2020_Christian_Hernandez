###############################################################################################
# Assignment 2
#October 11th, 2020
###############################################################################################

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
###############################################################################################
#Problem 1
###############################################################################################
# (1) 
a <- seq(0, 100, by = 1)
a
typeof(a)
#This is a double atomic vector
a
b <- seq_len(100) #c(1:100) #These are integer atomic vectors
b
typeof(a)
typeof(b)
is.atomic(a)

# (2)
c <- rep(10, times = 100)
c
typeof(c)

# (3)
d <- rep(c(1, 2, 3, 4, 5), times = 10)
d
typeof(d)

# (4)
d <- rep(c(1, 2, 3, 4, 5), each = 10)
d
typeof(d)

# (5)
e <- seq(0, 1, by = 0.1)
e
typeof(e)

###############################################################################################
#Problem 2
###############################################################################################

help('rivers')
data('rivers')
View(data)
# (1) 
## This type of data is an Atomic vector, as it only stores one consistent element, numbers, In this case legnths in miles(One dimensional). Since the data only presents numbers, this particular data can be characterized as either 'double' or 'integer' atomic vectors.

# (2)
##Calculations
data(rivers)
me <- log(mean(rivers)) #Mean
sd <- log(sd(rivers)) #Standard Deviation
ma <- log(max(rivers)) #Maximum Value
mi <- log(min(rivers)) #Minimum Value
le <- log(length(rivers)) #Length
va <- log(var(rivers)) #Variance
##Creating Vector
vector <- c(me, sd, ma, mi, le, va)
vector
##Final Vector, Naming
names(vector) <- c("Mean", "Standard Deviation", "Maximum", "Minimum", "Length", "Variance")
vector
##Checking for desired parameters
is.atomic(vector) #TRUE
str(vector)

# (3)
##Adjusting Data
x <- sort(rivers)
x
x2 <- x[!x %in% c(135:237, 1270:3710)]
x2
##New Calculations
me <- log(mean(x2)) #Mean
sd <- log(sd(x2)) #Standard Deviation
ma <- log(max(x2)) #Maximum Value
mi <- log(min(x2)) #Minimum Value
le <- log(length(x2)) #Length
va <- log(var(x2)) #Variance
##Creating Vector
vector_new <- c(me, sd, ma, mi, le, va)
vector_new
##Final Vector, Naming
names(vector_new) <- c("Mean", "Standard Deviation", "Maximum", "Minimum", "Length", "Variance")
vector_new
##Checking for desired parameters
is.atomic(vector_new) #TRUE
str(vector_new)

##Checking both vectors
vector
vector_new

###############################################################################################
#Problem 3
###############################################################################################
# (1)
x <- c(5, 6, 7, 8)
y <- c("a", "b", "c", "d")
u <- list(x, y)
str(u)
 
# (2)
u[[2]] <- c(1, 2, 3, 4)
u
str(u)
typeof(u)

# (3)
##There could potentially be a better way to do this for lists, for better efficiency. 
u_1 <- sum(u[[1]])
u_2 <- sum(u[[2]])
u_mean <- ((u_1 + u_2)/8)

# (4)
z <- (x2 = x^2) #On the assignment, these weren't described as characters. 
v <- (log_x = log(x)) ##On the assignment, these weren't described as characters.
zv <- list(z, v)

final_list <- append(u, zv)

# (5)
final_list[[4]] <- (log(x))

###############################################################################################
#Problem 4
###############################################################################################
#(1)
x <- c(1, 2, 3)
y <- c(4, 5, 6)
z <- c(7, 8, 9)
mat_1 <- rbind(x, y, z)
mat_1

#(2)
mat_2 <- matrix(c(x, y, z), ncol = 3) ##Option 1. I personally favor this one.More flexible. 
mat_2

mat_3 <- cbind(x, y, z)   ##Option 2. More practical. 
mat_3

#(3)
a <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
reshape <- matrix(a, nrow = 4, ncol = 3)
reshape

#(4)
reshape_2 <- matrix(a, nrow = 4, ncol = 3, byrow = TRUE)
reshape_2

###############################################################################################
#Problem 5
###############################################################################################
# (1)
a <- c(1:12)
b <- c(1:16)
c <- c(16:1)

A <- matrix(a, nrow = 3)
B <- matrix(b, nrow = 4)
C <- matrix(c, nrow = 4)


# (2)
B*C 
##This operation multiplies vectors in metrix accordingly. Since each matrix has the same dimensions, the elements are multiplied according to the position of the other matrix. 

# (3) 
A %*% B
A
B
##This special arithemetic is performing a some special multication involving both matrices, with matrix A being the main matrix for multiplication. It seems that both multiplication and addition are part of this arithemetic. 

# (4)
##Computing the sum of the diagonal elements of B
sum(diag(B))




















