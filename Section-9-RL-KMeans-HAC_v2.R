## API-222 Section 9: Reinforcement Learning, K-Means, HAC
## Code by TF Laura Morris
## Code based on past code by TFs Amy Wickett, Emily Mower 

## The following code is meant as a first introduction to 
## these concepts in R. It is therefore helpful to run it
## one line at a time and see what happens. To run one 
## line of code in RStudio, you can highlight the code you want to 
## run and hit "Run" at the top of the script. 
## Alternatively, on a mac, you can highlight the code to
## run and hit Cmd+Enter.  Or on a PC, you can highlight
## the code to run and hit Ctrl + Enter. If you ever forget 
## how a function works, you can type ? followed immediately 
## (e.g. with no space) by the function name to get the help file

##################################################################

## We will need the following packages. Remember, if you 
## have not installed them, you will need to first run
## install.packages("ISLR"), install.packages("dplyr"), etc.

library(ISLR)
library(dplyr)
library(ggfortify)
library(gridExtra)

## We will begin by programming value iteration for gridworld,
## as shown in the lecture notes. This gridworld is a 3 x 4
## grid, where cells are referred to by (row_number, column_number)
## The row and column numbers start at 1, with the first row
## being the top row and the first column being the leftmost
## column. Cell (2, 2) is grayed out, so the agent cannot visit
## that cell. Cell (1, 4) has a positive reward R and cell
## (2, 4) has a negative reward -R. We beging by specifying
## the reward R

R       <- 100

## The agent discounts future rewards using a discount rate D

D       <- 0.9

## To allow for more advanced options, we also set a noise 
## parameter N. When N = 1, the agent moves exactly where
## he/she intended to move. When N < 1, the agent moves in
## the desired direction with probability N and moves 90 degrees
## to the left of the desired direction with probability (1-N)/2
## and 90 degrees to the right of the desired direction with
## probability (1-N)/2. If the agent tries to move outside the
## valid squares, he/she remains in the current cell for the 
## next period.

N       <- 1

## When the agent hits the cells with R or -R, the agent receives
## the corresponding rewards and then remains in those cells
## forever (though the agent collects the reward only once).
## For all other cells, the agent receives a reward of 0 and
## can continue to move. 

## We will now do value iteration to determine the optimal policy.
## In value iteration, we find the value of each cell under the
## optimal policy. To begin, we initialize the values of each cell
## at 0.

gridworld <- function(R, D, N){

v1      <- 0
v2      <- 0
v3      <- 0
v4      <- 0
v5      <- 0
v6      <- 0
v7      <- 0
v8      <- 0
v9      <- 0

## Value iteration involves iterating through the values of the 
## cells. Each iteration, we update the values of each cell given
## our current knowledge. Let the squares with zero rewards be
## labeled 1 to 9:
## 1 = (1, 1)
## 2 = (1, 2)
## 3 = (1, 3)
## 4 = (2, 1)
## 5 = (2, 3)
## 6 = (3, 1)
## 7 = (3, 2)
## 8 = (3, 3)
## 9 = (3, 4)

for (i in 1:1000) {
## Up
  v1u    <- D * (N * v1 + (1-N)/2 * v1 + (1-N)/2 * v2)
  v2u    <- D * (N * v2 + (1-N)/2 * v1 + (1-N)/2 * v3)
  v3u    <- D * (N * v3 + (1-N)/2 * v2 + (1-N)/2 * R)
  v4u    <- # Challenge 1
  v5u    <- D * (N * v3 + (1-N)/2 * v5 + (1-N)/2 * -R)
  v6u    <- D * (N * v4 + (1-N)/2 * v6 + (1-N)/2 * v7) 
  v7u    <- D * (N * v7 + (1-N)/2 * v6 + (1-N)/2 * v8)
  v8u    <- D * (N * v5 + (1-N)/2 * v7 + (1-N)/2 * v9)
  v9u    <- D * (N * -R + (1-N)/2 * v8 + (1-N)/2 * v9)

## Down
  v1d    <- D * (N * v4 + (1-N)/2 * v1 + (1-N)/2 * v2)
  v2d    <- D * (N * v2 + (1-N)/2 * v1 + (1-N)/2 * v3)
  v3d    <- D * (N * v5 + (1-N)/2 * v2 + (1-N)/2 * R)
  v4d    <- D * (N * v6 + (1-N)/2 * v4 + (1-N)/2 * v4)
  v5d    <- D * (N * v8 + (1-N)/2 * v5 + (1-N)/2 * -R)
  v6d    <- # Challenge 2
  v7d    <- D * (N * v7 + (1-N)/2 * v6 + (1-N)/2 * v8)
  v8d    <- D * (N * v8 + (1-N)/2 * v7 + (1-N)/2 * v9)
  v9d    <- D * (N * v9 + (1-N)/2 * v8 + (1-N)/2 * v9)
  
## Left
  v1l    <- D * (N * v1 + (1-N)/2 * v1 + (1-N)/2 * v4)
  v2l    <- D * (N * v1 + (1-N)/2 * v2 + (1-N)/2 * v2)
  v3l    <- D * (N * v2 + (1-N)/2 * v3 + (1-N)/2 * v5)
  v4l    <- D * (N * v4 + (1-N)/2 * v6 + (1-N)/2 * v1)
  v5l    <- D * (N * v5 + (1-N)/2 * v3 + (1-N)/2 * v8)
  v6l    <- D * (N * v6 + (1-N)/2 * v4 + (1-N)/2 * v6) 
  v7l    <- D * (N * v6 + (1-N)/2 * v7 + (1-N)/2 * v7)
  v8l    <- D * (N * v7 + (1-N)/2 * v5 + (1-N)/2 * v8)
  v9l    <- # Challenge 3
  
## Right
  v1r    <- D * (N * v2 + (1-N)/2 * v1 + (1-N)/2 * v4)
  v2r    <- # Challenge 4
  v3r    <- D * (N * R  + (1-N)/2 * v3 + (1-N)/2 * v5)
  v4r    <- D * (N * v4 + (1-N)/2 * v6 + (1-N)/2 * v1)
  v5r    <- D * (N * -R + (1-N)/2 * v3 + (1-N)/2 * v8)
  v6r    <- D * (N * v7 + (1-N)/2 * v4 + (1-N)/2 * v6) 
  v7r    <- D * (N * v8 + (1-N)/2 * v7 + (1-N)/2 * v7)
  v8r    <- D * (N * v9 + (1-N)/2 * v5 + (1-N)/2 * v8)
  v9r    <- D * (N * v9 + (1-N)/2 * v9 + (1-N)/2 * -R)
  
## Pick the highest value
  v1    <- max(v1u, v1d, v1l, v1r)
  v2    <- max(v2u, v2d, v2l, v2r)
  v3    <- max(v3u, v3d, v3l, v3r)
  v4    <- max(v4u, v4d, v4l, v4r)
  v5    <- max(v5u, v5d, v5l, v5r)
  v6    <- max(v6u, v6d, v6l, v6r)
  v7    <- max(v7u, v7d, v7l, v7r)
  v8    <- max(v8u, v8d, v8l, v8r)
  v9    <- max(v9u, v9d, v9l, v9r)
  
  
}

  policy <- c(which.max(c(v1u, v1d, v1l, v1r)),
              which.max(c(v2u, v2d, v2l, v2r)),
              which.max(c(v3u, v3d, v3l, v3r)),
              NA,
              which.max(c(v4u, v4d, v4l, v4r)),
              NA,
              which.max(c(v5u, v5d, v5l, v5r)),
              NA,
              which.max(c(v6u, v6d, v6l, v6r)),
              which.max(c(v7u, v7d, v7l, v7r)),
              which.max(c(v8u, v8d, v8l, v8r)),
              which.max(c(v9u, v9d, v9l, v9r)))
  policy[which(policy == 1)] <- "up"
  policy[which(policy == 2)] <- "down"
  policy[which(policy == 3)] <- "left"
  policy[which(policy == 4)] <- "right"
  print(matrix(policy, nrow = 3, byrow = TRUE))

print(matrix(c(v1, v2, v3, R, v4, NA, v5, -R, v6, v7, v8, v9),
             nrow = 3, ncol = 4, byrow = TRUE))
}
gridworld(R, D, N)
gridworld(R, D, 0.8)


## Now, we will move on to clustering methods. In this class,
## you have been introduced to K-Means clustering and Hierarchical
## clustering (HAC). We will use the Auto data set, which provides
## information on different cars, and we will apply the clustering
## techniques you've learned to identify groups of cars.

auto_data   <- Auto

## Let's start by understanding our data

?Auto
View(auto_data)

## The vehicle name should be excluded from our cluster features,
## since it is really more of an observation label.

##### K-MEANS #####

## We will start by running K-Means, using the kmeans()
## function. Let's learn about the function.

?kmeans

## Let's start by running K-Means with K=3. 

kmeans3_cars  <- kmeans(auto_data[,-9], 3)

## We use the str() function to learn about our clusters.
## str is short for structure and it is a function that
## compactly displays the structure of an R object.

str(kmeans3_cars)

## We can look at the data for each cluster to see if any
## obvious patterns jump out at us before using other methods
## to better understand what drove cluster membership. One
## of the most challenging things about clustering is trying
## to understand the meanings of the clusters you find.

cluster1 <- auto_data[which(kmeans3_cars$cluster == 1), ]
cluster2 <- auto_data[which(kmeans3_cars$cluster == 2), ]
cluster3 <- auto_data[which(kmeans3_cars$cluster == 3), ]

View(cluster1)
View(cluster2)
View(cluster3)

## If you had to explain to someone what the three clusters
## meant, what would you tell them? 
## Probably just glancing at the cluster data is not 
## informative enough. We will now look at variable
## means by cluster and compare to see if we can get
## a better sense of the meaning of each cluster.

summary_mtx           <- kmeans3_cars$centers

summary_mtx
## From this, it looks like the cars have mostly been grouped 
## according to engine characteristics and performance.
## Year seems to have been unimportant; origin seems less 
## important than other variables, but still differs by 
## cluster. One challenge here is that origin is really a
## factor variable that was treated as a numeric variable.
## Dealing with mixed data (numeric and categorical features)
## is a challenge for these methods, where only one distance
## metric (such as Euclidean distance) is used. 

## Let's pull the cluster memberships into the original
## dataframe so we can start visualizing the clusters.

auto_k3               <- cbind.data.frame(auto_data, 
                                          as.factor(kmeans3_cars$cluster))
colnames(auto_k3)[10] <- "Cluster"

## It would be nice to visualize our clusters. How can
## we do that?  One option is to reduce the dimensionality
## of the data using PCA. We will run PCA with and without
## standardizing the variables. While standardizing
## variables is preferable with PCA, we did not scale
## our variables before running K-Means, so it will be
## interesting to compare the visualizations of K-Means
## and PCA with and without standardizing.

pca_auto_stdized      <- prcomp(auto_data[,1:8], 
                                center = TRUE, scale = TRUE)
summary(pca_auto_stdized)

pca_auto_noscale      <- prcomp(auto_data[,1:8])
summary(pca_auto_noscale)


## Let's plot the first two components from PCA (with
## and without scaling) and color code the points 
## according to the cluster they were assigned under 
## K-Means.

p1  <- autoplot(pca_auto_stdized, 
                data = auto_k3, colour = 'Cluster',
                loadings = TRUE, loadings.colour = 'blue',
                loadings.label = TRUE, loadings.label.size = 3)
p2  <- autoplot(pca_auto_noscale, 
                data = auto_k3, colour = 'Cluster',
                loadings = TRUE, loadings.colour = 'blue',
                loadings.label = TRUE, loadings.label.size = 3)

grid.arrange(p1, p2, nrow = 1)

### HAC ####

## Next, let's repeat the exercise using hierarchical 
## clustering. To do this, we will use the function
## hclust(), which will require a distance argument
## found using the function dist(). Let's learn about
## these two functions.

?hclust
?dist

## Recall that you do not need to specify the number
## of clusters ahead of time when using hierarchical
## clustering. 

hc_auto   <- hclust(dist(auto_data[,1:8], 
                         method = "euclidean"),
                    method = "complete")

## Let's look at our dendrogram (the structure of
## the clustering), which looks like a tree.

plot(hc_auto, 
     hang = -1, cex = 0.1, 
     labels = auto_data[,9],
     xlab = "Car Models")

## The names are far too small for us to see right now.
## However, we can understand the high-level structure.
## For K-Means, we set K=3. From this, we see that 
## moving K from 2 to 3, 3 to 4, and 4 to 5 all make
## the resulting clusters significantly more similar.
## We can now save the dendrogram as a PDF to examine
## the names of the cars to figure out which cars
## are similar and which ones are different.
pdf("DendrogramHC.pdf")
plot(hc_auto, 
     hang = -1, cex = 0.1, 
     labels = auto_data[,9],
     xlab = "Car Models")
dev.off()

## Since we used 3 clusters for K-Means, let's start
## using 3 clusters for Hierarchical Clustering. We
## cut the dendrogram using the function cutree().
## Let's first learn about cutree().

?cutree

## Now, let's cut the dendrogram down to 3 clusters
## and attach the cluster memberships from HC to
## the original data frame.

hc3                     <- cutree(hc_auto, k = 3)
auto_hc3                <- cbind.data.frame(auto_data, 
                                            as.factor(hc3))
colnames(auto_hc3)[10]  <- "Cluster"

## We can now plot the clusters resulting from 
## hierarchical clustering on the first two 
## principal components with and without standardizing
## variables.

p1  <- autoplot(pca_auto_stdized, 
                data = auto_hc3, colour = 'Cluster',
                loadings = TRUE, loadings.colour = 'blue',
                loadings.label = TRUE, loadings.label.size = 3)
p2  <- autoplot(pca_auto_noscale, 
                data = auto_hc3, colour = 'Cluster',
                loadings = TRUE, loadings.colour = 'blue',
                loadings.label = TRUE, loadings.label.size = 3)

grid.arrange(p1, p2, nrow = 1)

#### ADDITIONAL MATERIAL: CHANGING K & LINKAGE CRITERIA ####

## Let's compare the results from K-Means and 
## hierarchical clustering for different numbers
## of clusters K. Also, let's see what happens
## if we change the linkage criteria used for
## hierarchical clustering. To do this, we first
## write a function, so we can run it repeatedly
## for a range of parameters:

kmeans_v_hc <- function(K, auto_data, linkage = "complete") {
  
  ## Run K-Means
  kmeans_cars             <- kmeans(auto_data[,-9], K)
  auto_k                  <- cbind.data.frame(auto_data, 
                                              as.factor(kmeans_cars$cluster))
  colnames(auto_k)[10]    <- "Cluster"
  
  ## Run Hierarchical Clustering
  hc_auto                 <- hclust(dist(auto_data[,1:8], 
                                         method = "euclidean"),
                                    method = linkage)
  hc                      <- cutree(hc_auto, k = K)
  auto_hc                 <- cbind.data.frame(auto_data, 
                                              as.factor(hc))
  colnames(auto_hc)[10]   <- "Cluster"
  
  ## Plot K-Means v. Hierarchical Clustering
  
  p1  <- autoplot(pca_auto_stdized, 
                  data = auto_k, colour = 'Cluster',
                  loadings = TRUE, loadings.colour = 'blue',
                  loadings.label = TRUE, loadings.label.size = 3,
                  main = "K-Means")
  p2  <- autoplot(pca_auto_stdized, 
                  data = auto_hc, colour = 'Cluster',
                  loadings = TRUE, loadings.colour = 'blue',
                  loadings.label = TRUE, loadings.label.size = 3,
                  main = "Hierarchical Clustering")
  
  grid.arrange(p1, p2, nrow = 1)
}

## Let's start by looking at K-Means v. HC for K = 3

kmeans_v_hc(3, auto_data)

## What happens if we change the linkage criteria?

kmeans_v_hc(3, auto_data, "single")
kmeans_v_hc(3, auto_data, "average")
kmeans_v_hc(3, auto_data, "centroid")
kmeans_v_hc(3, auto_data, "median")
kmeans_v_hc(3, auto_data, "ward.D2")

## Let's try a few more K's

kmeans_v_hc(5, auto_data)
kmeans_v_hc(8, auto_data)
kmeans_v_hc(8, auto_data)

## We could also try running our algorithms on 
## standardized data:

stdz_auto   <- cbind.data.frame(scale(auto_data[,1:8]),
                                auto_data[,9])
kmeans_v_hc(3, stdz_auto)
kmeans_v_hc(5, stdz_auto)

## Note that we didn't deal with the fact that K-Means
## may give different results, depending on how it is
## initialized. The fastest way to deal with this is 
## to change the nstart parameter in the kmeans()
## function, e.g.

kmeans3_100starts <- kmeans(auto_data[,-9],
                            centers = 3,
                            nstart = 100)

## Below is a function that accomplishes the same result.
## It runs k-means 100 times, each time checking if the
## resulting total within-cluster sum of squares is 
## better (smaller) than all previous runs. If it is,
## it chooses that run as the current best run. If it
## isn't, it keeps going.

best_kmeans <- function(K) {
  loss      <- Inf
  
  ## Run K-Means
  for (i in 1:100){
    kmeans_cars             <- kmeans(auto_data[,-9], K)
    if (kmeans_cars$tot.withinss < loss) {
      kmeans_final          <- kmeans_cars
      loss                  <- kmeans_cars$tot.withinss
    }
  }
  auto_k                  <- cbind.data.frame(auto_data, 
                                              as.factor(kmeans_final$cluster))
  colnames(auto_k)[10]    <- "Cluster"
  
  return(auto_k)
}

auto_k      <- best_kmeans(3)



##### Solutions to Challenges ####

# Challenge 1
D * (N * v1 + (1-N)/2 * v4 + (1-N)/2 * v4)

# Challenge 2
D * (N * v6 + (1-N)/2 * v6 + (1-N)/2 * v7) 

# Challenge 3
D * (N * v8 + (1-N)/2 * v9 + (1-N)/2 * -R)

# Challenge 4
D * (N * v3 + (1-N)/2 * v2 + (1-N)/2 * v2)
