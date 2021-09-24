install.packages("AER")
library(AER)
library(tidyverse)

data("MASchools") %>% 
  as.data.frame()

schools <- MASchools

#1 How many observations?
nrow(schools)
#Answer: 220

#2 How many variables?
ncol(schools)
#Answer: 16

#3 How many NA?
sum(is.na(schools))
#Answer: 74

#4 Which are categorical columns?
#district, municipality

#5 Mean 8th grade score?
mean(schools$score8,na.rm = T)
#Answer: 698.41

#6 SD in S-T Ratio?
sd(schools$stratio)
#Answer: 2.28

#For  the  next  few  questions,  first  omit  any  rows  (observations)  
#with  NA  values  and  drop the District and Municipality variables.   
#Then put  the  last  35  observations  (according to their row number in 
#the dataframe when you read it in originally) in a test set and the  
#remaining observations in a training set.

schools2 <- schools %>% 
  drop_na() %>% 
  select(-c(1,2))

schools_tr <- schools2 %>% 
  head(.,n = (nrow(schools2)-35))

schools_te <- schools2 %>% 
  tail(.,n = 35)

#7 Regress 8th grade score on everything.
school_lm <- lm(score8~.,schools_tr)

mean((schools_te$score8 - predict.lm(school_lm, schools_te)) ^ 2)
#Answer: 78.21

#8 Regress against spending, reduced price lunch, and S-T ratio.
school_lm2 <- lm(score8~stratio+lunch+expreg,schools_tr)
mean((schools_te$score8 - predict.lm(school_lm2, schools_te)) ^ 2)
print(school_lm)

#9 K Nearest Neighbors, k=2
install.packages("class")
library(class)
pr <- knn(schools_tr,schools_te,schools_tr$score8,k = 2)
