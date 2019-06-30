library(recommenderlab)
library(ggplot2)
library(cluster)
set.seed(1)


data("MovieLense")
ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,  colCounts(MovieLense) > 100]
ratings_movies_norm <- normalize(ratings_movies)
ratings_movies_good <- binarize(ratings_movies, minRating = 3)
which_train <- sample(x = c(TRUE, FALSE), size = nrow(ratings_movies), replace = TRUE, prob = c(0.8, 0.2))
recc_data_train <- ratings_movies[which_train, ] 
recc_data_test <- ratings_movies[!which_train, ] 





 cost_df <- data.frame()
 for(i in 1:100){
   kmeans<- kmeans(x=MovieLense, centers=i, iter.max=50)
   cost_df<- rbind(cost_df, cbind(i, kmeans$tot.withinss))
   }
 names(cost_df) <- c("cluster", "cost") #Elbow method to identify the idle number of Cluster
 #Cost plot
 ggplot(data=cost_df, aes(x=cluster, y=cost, group=1)) + theme_bw(base_family="Garamond") + geom_line(colour = "darkgreen") + theme(text = element_text(size=20)) + ggtitle("Reduction In Cost For Values of 'k'\n") + xlab("\nClusters") + ylab("Within-Cluster Sum of Squares\n")





















recc_model <- Recommender(data = recc_data_train, method = "IBCF", parameter = list(method="jaccard",k = 30))
n_recommended <- 10
recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = n_recommended) 
#We can define a matrix with the recommendations for each user:
recc_matrix <- sapply(recc_predicted@items, function(x){
  colnames(ratings_movies)[x] 
})

recc_matrix[, 2]


evaluation_scheme <- evaluationScheme(ratings_movies, method="cross-validation", k=5, given=3, goodRating=5) #k=5 meaning a 5-fold cross validation. given=3 meaning a Given-3 protocol
evaluation_results <- evaluate(evaluation_scheme, method="IBCF", n=c(1,3,5,10,15,20))
eval_results <- getConfusionMatrix(evaluation_results)[[1]]