library(tidyverse)
library(AER)
library(glmnet)

prostate <- read.table('http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/prostate.data')
View(prostate)
seatbelts <- as.data.frame((USSeatBelts))

# what are the dimensions of the dataset? What does the last column represent?
nrow(prostate)
ncol(prostate)
#the final column is a dummy variable to represent whether it is in the 
#training set or the test set for this model.

# Use the cor() and pairs() functions to visualise the correlations and  
# reproduce the scatterplot matrix graphs shown in lecture1
pairs(cor(prostate[1:9]), col = "purple")
pairs((prostate[1:9]), col = "purple", row1attop = T)

# Why do the correlations associated with the gleason variable mostly 
# appear as straight lines? (0.5 pt)

#The Gleason variable has a small number of possible values in this dataset,
#so the results will be constrained to a small number of bands, which are 
#vertical because the other variable has a strong degree of variation along a
#given value of gleason.

# For the next part, please use the USSeatBelts data set from the AER package. 
# Consider the prediction problem where you want to predict fatalities per 
# million highway miles, against all other variables available in the data set.

# How many observations have missing values for at least one feature? 
# Drop those observations for now.

seatbelts2 <- drop_na(USSeatBelts)

nrow(seatbelts)-nrow(seatbelts2)

# Which variables are categorical variables? How many classes do each of 
# these categorical variables have?

cats <- seatbelts2 %>% 
  select(where(is.factor))

names(cats)

nrow(distinct(cats[1]))
nrow(distinct(cats[2]))
nrow(distinct(cats[3]))
nrow(distinct(cats[4]))
nrow(distinct(cats[5]))
nrow(distinct(cats[6]))
nrow(distinct(cats[7]))

# Which states have the highest and lowest fatality rates per million 
# highway miles? What are their respective fatality rates? (1 pt) 

seatbelts2 %>% 
  slice_max(.,order_by = .$fatalities,n = 1)

seatbelts2 %>% 
  slice_min(.,order_by = .$fatalities,n = 1)

# (Hint: look up the by function in R help, and remember the command 
# which.min from section 4 ) Now, consider the prediction problem where 
# you want to predict fatalities (per million highway miles) given all 
# other variables available in the data set. Drop the state and year 
# variables.

# Convert the remaining categorical variables to indicator variables (also called 
# "dummy" variables) with the same variable names and run a linear regression. 
# What is the adjusted R2?

seatbelts3 <- seatbelts2 
  


# Run ridge regression with cross-validation using the canned function 
# cv.glmnet from the package glmnet. You can use the ?? sequence generated 
# by cv.glment (you do not need to provide your own ?? sequence). In order to 
# receive credit for this question, make the line immediately preceding this 
# command say set.seed(222) and run the two lines together.

# Please report all numbers by rounding to three decimal places. (2 pts)
#  . Which ?? had the lowest mean cross-validation error?
#  . What was the cross-validation error?
#  . What was the standard error of the mean cross-validation error for 
#    this value of ???
#  . What was the largest value of ?? whose mean cross validation error was 
#    within one standard deviation of the lowest cross-validation error?

# Now consider the prediction problem where you want to predict seatbelt 
# usage using all other features in the data. Using the USSeatBelts data set, 
# implement your own 5-fold crossvalidation routine for KNN for k = 1, ..., 
# 100 (e.g. write the cross-validation routine yourself rather than using a 
# canned package). Include the snippet of code you wrote here. It should
# not exceed 20 lines. Which k is best according to CV?

# Plot mean cross-validation MSE as a function of k. Label the y-axis 
# "Mean CV MSE" and the x-axis "k".