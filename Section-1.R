## API-222 Section 1: Introduction to R
## Code by TF Laura Morris
## Notes build on previous TFs Amy Wickett & Emily Mower
## and IQSS R Into

## You should have R installed --if not:


## The following code is meant as a first introduction to R.
## It is therefore helpful to run it one line at a time and see what happens.
## To run one line of code in RStudio, you can highlight the code you want to 
## run and hit "Run" at the top of the script. 

## On a mac, you can highlight the code you want to run and hit Command + Enter
## On a PC, you can highlight the code you want to run and hit Ctrl + Enter
## If you ever forget how a function works, you can type ? followed immediately
## (e.g. with no space) by the function name to get the help file

#########################################################################  
## R Fundamentals 

## The general form for calling R functions is
## FunctionName(arg.1 = value.1, arg.2 = value.2, ..., arg.n - value.n)

## Values can be assigned names and used in subsequent operations
## Instead of the traditional "=" sign, the convention in R is "<-"
## The "=" sign also works, but I will use "<-" to be consistent with convention


#########################################################################  
## First, we will learn how to make different types of vectors
## Our first vector will contain integers 1 through 4

vec1    <- c(1, 2, 3, 4)
print(vec1)

## In R, we use square brackets for indexing
## So, for example, if we want to print the 1st element of our vector:

print(vec1[1])

## If we want to print the 4th element of our vector:

print(vec1[4])

## If we want to print the 1st and the 4th elements in one go, we make a vector with
## the desired indices and place that index vector within square brackets

print(vec1[c(1,4)])


#########################################################################  
## An alternative way to create vec1 would be using the seq() command, which allows
## us to generate a vector according to a sequence. If you just put seq(X) and if X
## is an integer, it will give you the sequence of integers from 1 to X (even if X
## is negative)

print(seq(4))
print(seq(-4))

vec2    <- seq(4)
print(vec2)

## What if we wanted to start at a different value or increment by 2 (or 3 or 4...)
## rather than by 1? That's no problem. seq() can take a "from" value, a "to" value
## and a "by" value

print(seq(from = 100, to = 120, by = 5))

## Now is a good time to remind you that if you ever forget how a function works
## you can look up it's help file. Let's try it:

?seq

## Maybe this starts to look messy to you. Note that as long as we are inside of 
## an operation (e.g. we are inside parentheses or we have written a mathematical
## operation, like * + - /, but we haven't yet put a value) we can put enters and
## the operation will continue to the next line. Let's try doing the previous line
## of code, but putting enters after each comma:

print(seq(from = 100, 
          to = 120, 
          by = 5))

## The seq command can be helpful for generating an ID variable, which could be
## useful if you want to keep track of observations in a dataset. We will cover
## this a little later, but it basically works by creating an ID column equal to
## seq(# of rows in data).

## Another operator that works similarly to seq() is the colon ":"
## However, it only allows you to work with sequences of adjacent integers
## I like to use it for indexing in for-loops, but it's easier to mess up
## and seq() can do exactly the same thing, so I would recommend using seq()

print(1:10)
print(10:1)


#########################################################################  
## Vectors don't have to be numeric. They could also be character / string
## vectors (but one vector will treat all elements as numbers or all
## elements as strings -- it can't treat some elements as numbers and some
## as strings)

word_vec  <- c("Hello", "Time To", "Learn", "R", "!")
print(word_vec)


#########################################################################  
## Suppose you want to know which element of an item equals a value
## which() is a very convenient function for this. It takes a logical statement
## (e.g. one that can be answered by TRUE or FALSE and returns the indices
## for which the statement is TRUE. It returns integer(0) if the statement
## is not TRUE for any elements)

print(which(word_vec == "Learn"))
print(which(word_vec == "Hi!"))

## You can also find out how long a vector is by using the function length()

print(length(vec1))
print(length(seq(10)))
print(length(seq(from = 100, to = 120, by = 2)))


#########################################################################  
## You can also calculate statistics about vectors

print(mean(vec1))
print(median(vec1))
print(min(vec1))
print(max(vec1))

## To get the variance, use the var() function
## To get the standard deviation, you can use sd() or sqrt(var()) or var()^(1/2)

print(var(vec1))
print(sd(vec1))



#########################################################################  
## You can also check whether two items (scalars, vectors, matrices, etc.)
## are the same. To do this on a vector, it will check whether the first
## element of the first vector equals the first element of the second vector, etc.
## We use a double equal sign "==" to check if two items are equal and a 
## single equal sign "=" to set the item on the left of the "=" equal to the
## item on the right of the "="

print(vec1)
print(vec2)
print(vec1 == vec2)    

vec4    <- c(1, 4, 9, 16)

print(vec4)
print(vec1 == vec4)

## If you want to check if two vectors are exactly the same, you can use
## the function all.equal(), which will return TRUE if they are the same
## and will describe their differences if they aren't the same

print(all.equal(vec1, vec2))
print(all.equal(vec1, vec4))


#########################################################################  
## "if statements" can be very useful. They work as follows:
## if (the logical statement in these parentheses is TRUE) {
##    do this
## } else {
##    do that
## }
## Let's try it.

## Example 1:

if (2 + 2 == 5) {
  print("Yikes")
} else {
  print("Good job!")
}

## Example 2:

if (vec1[2] == 2) {
  print("Hello")
}else {
  print("Goodbye")
}

## For loops are also very helpful. They work as follows:
##    for (each index in the given vector) {
##      do this
##    }
## Let's try one. It's also a good place to show how seq() and : can
## both work. The following three loops are three ways to do the same loop

print(length(vec1))
for (i in 1:length(vec1)) {
  print(vec1[i])
}

for (i in seq(length(vec1))) {
  print(vec1[i])
}

for (i in seq(from = 1, to = length(vec1), by = 1)) {
  print(vec1[i])
}

## While loops are similar. They work as follows:
##    while (statment in these parentheses is TRUE) {
##        do this
##      }
##    then stop
## Let's try it

i       <- 1

while (i < 4) {
  print(i)
  i   <- i + 1
}


#########################################################################
## To make a matrix, use the matrix() command
## The first element fed in is the data you want to put in matrix form
## Then, you specify the number of rows and columns
## By default, it fills information down the columns, but you can tell
## it to do by row

mtx1    <- matrix(vec1,
                  nrow = 2,
                  ncol = 2)
print(mtx1)

mtx2    <- matrix(vec1,
                  nrow = 2,
                  ncol = 2,
                  byrow = TRUE)
print(mtx2)

## Note that mtx2 is the transpose of mtx1. If you want to transpose
## a matrix, you can use t()

print(mtx1)
print(t(mtx1))

## As with vectors, you can check if two matrices are equal

print(mtx1 == mtx2)
print(all.equal(mtx1, mtx2))

print(mtx1 == t(mtx2))
print(all.equal(mtx1, t(mtx2)))

## Matrices are indexed by [row,column]

print(mtx1[1,2])
print(mtx2[1,2])

## Let's make a bigger matrix

mtx3    <- matrix(c(vec1, vec4),
                  nrow = 4,
                  ncol = 2)
print(mtx3)



#########################################################################
## You can also generate random numbers in R. However, one concern with analyses
## done using random numbers is that you might not be able to reproduce them.
## One way to avoid this is to set the "seed".
## Here's a reference for random seeds: https://en.wikipedia.org/wiki/Random_seed

set.seed(222)

## We can generate random numbers from all kinds of distributions. For now,
## we will generate a random normal variable. If I don't specify a mean
## or variance, it will assume mean = 0, standard deviation = 1.

norm_var1     <- rnorm(1)
print(norm_var1)

## Alternatively, we can specify the mean and standard deviation

norm_var2     <- rnorm(1, mean = 100, sd = 10)
print(norm_var2)

## The first element inside the parentheses is how many random variables I want
## to draw. For example, I could draw 10 

norm_vec      <- rnorm(10, mean = 5, sd = 1)
print(norm_vec)

## You can also draw from other distributions, like the uniform distribution

uni_var1      <- runif(1)
print(uni_var1)


#########################################################################
## There are lots of great datasets available as part of R packages
## Page 14 of Introduction to Statistical Learning with Applications in R
## Table 1.1 lays out 15 data sets available from R packages

## You can install a package in R using the `install.packages()`
## function. Once a package is installed you may use the `library`
## function to attach it so that it can be used.

install.packages("ISLR")

## Then, every time you want to use the package, you use library(package_name)

library(ISLR)


## Sometimes we will use outside datasets, not contained in R
## In order to read data from a file, you have to know what kind of file
## it is. The table below lists functions that can import data from
## common plain-text formats.

##  | Data Type                 | Function        |
##  | ------------------------- | --------------- |
##  | comma separated           | `read_csv()`    |
##  | tab separated             | `read_delim()`  |
##  | other delimited formats   | `read_table()`  |
##  | fixed width               | `read_fwf()`    |


################################################################### 

college_data  <- College

?College

## Let's learn about our data. To get the names of the columns in the 
## dataframe, we can use the function colnames()

colnames(college_data)

## To find out how many rows and columns are in the dataset, use dim()
## Recall that this gives us Rows followed by Columns

dim(college_data)

## To find out what type of data is in each column, we can use typeof()

typeof(college_data[,1])
typeof(college_data[,2])


## You can also look in the "environment" tab, press the blue arrow next
## to college_data and it will drop down showing the column names with
## their types and first few values. For college, all columns except the
## first are numeric. The first column is a factor column, which means it's
## categorical. To get a better sense of the data, let's look at it

View(college_data)

## To grab a column from a dataframe in R, you have 3 popular options:
##    df$column_name
##    df[,column_number]
##    df[,"column_name"]
## This will be useful so we can separate our outcome column from the
## feature columns.
## Let's try! So that we aren't overwhelmed by output, we will also use
## the function head(), which prints only the first few entries 

head(college_data$PhD)
head(college_data[,13])
head(college_data[,"PhD"])


################################################################### 
## In addition to importing individual datasets, R can save and
## load entire workspaces
ls() # list objects in our workspace

save.image(file="myWorkspace.RData") # save workspace 
rm(list=ls()) # remove all objects from our workspace 
ls() # list stored objects to make sure they are deleted

## Load the "myWorkspace.RData" file and check that it is restored
load("myWorkspace.RData") # load myWorkspace.RData
ls() # list objects

lm(college_data$Apps~college_data$Accept)

college_data$Rate <- 100*college_data$Enroll/college_data$Apps

lm(college_data$Rate~college_data$Grad.Rate)

summary(lm(college_data$Rate~college_data$Grad.Rate))
summary(lm(college_data$Grad.Rate~college_data$Rate))
