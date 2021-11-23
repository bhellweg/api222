## API-222 Section 7: Tree-based methods, Bagging, Boosting
## Code by TF Laura Morris
## Code is based on past notes by TFs Emily Mower, Amy Wickett

## The following code is meant as a first introduction to these concepts in R.
## It is therefore helpful to run it one line at a time and see what happens.
## To run one line of code in RStudio, you can highlight the code you want to 
## run and hit "Run" at the top of the script. 
## Alternatively, on a mac, you can highlight the code to run and hit Command + Enter
## Or on a PC, you can highlight the code to run and hit Ctrl + Enter
## If you ever forget how a function works, you can type ? followed immediately
## (e.g. with no space) by the function name to get the help file

#########################################################################  
## Decision Trees
#install.packages("tree")
library(ISLR)
library(tree)

## We will use the Carseats data. Sales in this data set
## is a continuous variable. We start by converting it to a 
## binary one that equals "Yes" if Sales > 8 and "No" otherwise.

carseat_data        <- Carseats
high_sales          <- as.factor(ifelse(carseat_data$Sales > 8, "Yes", "No"))
carseat_data        <- data.frame(carseat_data, high_sales)
carseat_data = carseat_data[, -1]

## Let's again split the data into training and test sets

set.seed(102618)  
train               <- sample(seq(nrow(carseat_data)),
                              round(nrow(carseat_data) * 0.5))
train               <- sort(train)
test                <- which(!(seq(nrow(carseat_data)) %in% train))

## We can now train a decision tree using the function tree()
?tree

carseats_tree       <- tree(high_sales ~., 
                            data = carseat_data[train,])


plot(carseats_tree)
text(carseats_tree, pretty = 0)
?text
## From this, we see that shelving location seems to be the
## most important determinant and price is the second most.
## Beyond that, this tree is very hard to read.
## If we just type the tree object name, we get:
## The split criterion (e.g. Price < 92.5)
## The number of observations in that branch
## The deviance
## The overall prediction for the branch
## The fraction of observations in that branch that are Yes/No
## Branches with terminal nodes are indicated by *

carseats_tree

## Given how deep our tree is grown, we may be worried
## about overfitting. We can start by evaluating the 
## error rate on the test set for the current tree.

error_rate_func   <- function(predictions, true_vals) {
  error_rate      <- mean(as.numeric(predictions != true_vals))
  return(error_rate)
}

deep_tree_preds   <- predict(carseats_tree,
                             carseat_data[test, ],
                             type = "class")
error_rate_func(deep_tree_preds, carseat_data[test,"high_sales"])
summary(carseats_tree)


## The difference in our error rate between the training
## and test sets indicates that we overfit. To address this,
## we want to prune the tree. cv.tree() uses cross-validation
## to determine how much to prune the tree. 

set.seed(20)
cv_carseats_tree  <- cv.tree(carseats_tree, FUN=prune.misclass)
names(cv_carseats_tree)
cv_carseats_tree

## Size tells us the number of terminal nodes on each
## of the trees considered; dev gives us the CV errors;
## k gives us the cost-complexity parameter.
## We can plot the error as a function of size and k

par(mfrow =c(1,2))

plot(cv_carseats_tree$size ,cv_carseats_tree$dev ,type="b")
plot(cv_carseats_tree$k ,cv_carseats_tree$dev ,type="b")

opt_indx          <- which.min(cv_carseats_tree$dev)
opt_size          <- cv_carseats_tree$size[opt_indx]

print(opt_size)

## Now we can prune the tree using prune.misclass()

pruned_carseats_tree  <- prune.misclass(carseats_tree,
                                        best = opt_size)
plot(pruned_carseats_tree)
text(pruned_carseats_tree, pretty = 0)

error_rate_func(predict(pruned_carseats_tree, carseat_data[test, ], 
                        type = "class")
                , carseat_data[test,"high_sales"])

## You'll notice the error in the pruned tree is smaller than the error in the 
## full grown tree 


## Regression Trees
## For this, we will use the Boston data

library(MASS)
boston_data         <- Boston

set.seed(222)  
train               <- sample(seq(nrow(boston_data)),
                              round(nrow(boston_data) * 0.8))
train               <- sort(train)
test                <- which(!(seq(nrow(boston_data)) %in%
                                 train))

boston_tree = tree(medv~., Boston, subset = train)
summary(boston_tree)

plot(boston_tree)
text(boston_tree)

## Calculate the MSE for the Predicted Values
boston_preds        <- predict(boston_tree, 
                               newdata = boston_data[test,])

msep_func <- function(predictions, true_vals) {
  MSEP    <- mean((predictions - true_vals)^2)
  return(MSEP)
}

print(msep_func(boston_preds, boston_data[test, "medv"]))

# (1) Create an object called cv_boston_tree that runs
# CV on boston_tree to find the best size according to 
# CV error

cv_boston_tree =cv.tree(boston_tree)


plot(cv_boston_tree$size ,cv_boston_tree$dev ,type='b')

## Let's see what the best size is

cv_boston_tree

# (2) Find which size had the lowest CV error and 
# save in a variable called best_size 
best_indx           <- which.min(cv_boston_tree$dev)
best_size           <- cv_boston_tree$size[best_indx]




## Prune the tree using the best size as found above

prune_boston = prune.tree(boston_tree, 
                          best = best_size)

boston_prune_preds        <- predict(prune_boston, 
                                     newdata = boston_data[test,])

print(msep_func(boston_prune_preds, boston_data[test, "medv"]))





## Random Forest, Bagging and Boosting
## Let's load our Boston dataset from last time  

# install.packages("MASS")

library(MASS)
boston_data         <- Boston

set.seed(222)
train               <- sample(seq(nrow(boston_data)),
                              round(nrow(boston_data) * 0.8))
train               <- sort(train)
test                <- which(!(seq(nrow(boston_data)) %in%
                                 train))

## Let's run a standard random forest. 
## Mtry is the number of variables to include at each branch. By
## setting this value to equal 13, we are performing bagging. You
## may be interested in the relative importance of each variable.
## By setting importance= TRUE, R will store the importance matrix.
## You can call this by "name of random forest"$importance   

## install.packages("randomForest")
library(randomForest)
set.seed(222)

bag.boston <- randomForest(medv~., data=data.frame(boston_data[-test,]), 
                           mtry=13, importance =TRUE)
bag.boston$importance

## Now let's make some predictions 
boston.test=Boston[-train ,"medv"]
yhat.bag <- predict (bag.boston , newdata=Boston[-train ,])
mean((yhat.bag -boston.test)^2)

## We are going to compare the outcome with boosting. Boosting has the same
## general form except instead of randomForest, you will use "gbm". We list the
## distribution as gaussian" since this is a regression problem; if it were a 
## binary classification problem, we would use distribution="bernoulli". The
## argument n.trees=5000 indicates that we want 5000 t trees, and the option
## interaction.depth=4 limits the depth of each tree. Just as before, we can
## see the relative importance by looking at the summary. lstat and rm are the
## most important variables. 

## install.packages("gbm")  
library (gbm)
set.seed(222)

boost.boston <- gbm(medv~., data=data.frame(boston_data[-test,]), 
                    distribution= "gaussian", n.trees=5000, interaction.depth=4)

summary(boost.boston)  

## Now let's make some predictions 

yhat.boost <- predict (boost.boston , newdata=Boston[-train ,], n.trees=5000)
mean((yhat.boost -boston.test)^2)

## We see above that bagging actually performed better than boosting in this case. 
## Just if you're curious, let's see how random forest does on its own. In this case
## boosting and bagging outperforms random forest with no modifications    

rf.boston <- randomForest(medv~., data=data.frame(boston_data[-test,]), 
                          importance =TRUE, n.trees=5000)

yhat.rf <- predict (rf.boston , newdata=Boston[-train ,])
mean((yhat.rf -boston.test)^2)




########################################################################
## Bootstrapping 

## We will use USArrests, which is available with base R, to learn how
## to bootstrap. Bootstrapping your data can be useful for a number of 
## reasons. One common reason to bootstrap is to estimate confidence
## intervals when there's no nice formula, as there is with linear regression.

arrest_data       <- USArrests

## Let's get to know our data. 

summary(arrest_data)
dim(arrest_data)

## Note that the rownames of the dataframe give us the city
## names corresponding to each observation

row.names(arrest_data)

## From the output, we see that we don't have any categorical variables
## Let's start today with bootstrapping. We will start with linear regression so we
## can compare the estimates we get from bootstrapping with the estimates
## we get from the regression outcome. We will use murder as our y and 
## the other available variables as our x variables.

arrest_lm         <- lm(Murder ~ Assault + UrbanPop + Rape,
                        arrest_data)

## Let's review the model output

summary(arrest_lm)
confint(arrest_lm)

## Now, let's see how close we can get with bootstrapping. To bootstrap,
## we will again create a function. The function will take 3 arguments:
## 1. The formula to use for our linear regression
## 2. The data
## 3. The number of times we want to bootstrap our data

bootstrap_function  <- function(string_formula, model_data, ndraws) {
  coeff_mtx         <- matrix(0, 
                              nrow = ndraws, 
                              ncol = ncol(model_data))
  
  for (i in 1:ndraws) {
    ## For each bootstrap draw, we bootstrap the data by sampling
    ## observations with replacement.
    bootstrap_ids   <- sample(seq(nrow(model_data)),
                              nrow(model_data),
                              replace = TRUE)
    bootstrap_data   <- model_data[bootstrap_ids,]
    
    ## Then we estimate the model and save the coefficients
    bootstrap_model <- lm(as.formula(string_formula),
                          bootstrap_data)
    coeff_mtx[i,]   <- bootstrap_model$coefficients
    
    ## Just so you are sure you understand what is being saved:
    if (i == 1) {
      print(bootstrap_model$coefficients)
    }
  }
  
  return(coeff_mtx)
}

## Now we can run our bootstrap function on real data

bootstrap_estimates <- bootstrap_function("Murder ~ Assault + UrbanPop + Rape",
                                          arrest_data,
                                          5000)

## Let's see how the "empirical confidence intervals" we got from
## bootstrapping our data compare to the confidence intervals R
## gave us for the coefficients. To find these, we will use the 
## quantile() function, which takes as its first argument the 
## vector of values for which you want to estimate a quantile,
## and as its second argument the quantile you want to estimate

bootstrap_CIs       <- matrix(0, nrow = 4, ncol = 2)
for (i in 1:4) {
  bootstrap_CIs[i,1]  <- quantile(bootstrap_estimates[,i], 0.025)
  bootstrap_CIs[i,2]  <- quantile(bootstrap_estimates[,i], 0.975)
}
print(bootstrap_CIs)
confint(arrest_lm)

## We can also plot histograms for each coefficient
hist(bootstrap_estimates[,1])
hist(bootstrap_estimates[,2])
hist(bootstrap_estimates[,3])
hist(bootstrap_estimates[,4])

## Note that as you increase the number of times you bootstrap
## your data, your estimates should get closer and closer to those 
## produced by the function confint(). Also, as you increase the
## number of times you bootstrap your data, the histogram of coefficient
# estimates starts to look like a normal distribution. Try re-running
# the bootstrap_estimates <- bootstrap_function() line with 10, 20,
# 50, 200, and 10000 and see how the histograms change. See also how the 
## estimates change compared to the ones produced by confint().
## Ideally, we would want to use as large a number of draws as could 
## run in a reasonable time, probably many more than 100 or 200.


