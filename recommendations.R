require(recommenderlab)
require(ggplot2)
require(data.table)
require(reshape2)

# Read in data
setwd("/Users/Bonhwang/Documents/GitHub/movie-recommender")
movie_data <- read.csv("movies.csv", stringsAsFactors = FALSE)
rating_data <- read.csv("ratings.csv", stringsAsFactors = FALSE)

str(movie_data)
summary(movie_data)
head(movie_data)

summary(rating_data)
head(rating_data)

# Encode movie genres using one-hot encoding
genres <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

for (genre in genres) {
  movie_data[, genre] <- as.numeric(grepl(genre, movie_data$genres))
}

# Preprocess ratings data
rating_matrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE)
rating_matrix <- as.matrix(rating_matrix[,-1])
# Convert rating matrix into recommenderlab sparse matrix
rating_matrix <- as(rating_matrix, "realRatingMatrix")
rating_matrix

# Recommendation System Parameters
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)
lapply(recommendation_model, "[[", "description")

# Use item-based collaborative filtering
recommendation_model$IBCF_realRatingMatrix$parameters
similarity_mat <- similarity(rating_matrix[1:4, ],
                             method = "cosine",
                             which = "users")
as.matrix(similarity_mat)
image(as.matrix(similarity_mat), main = "User's Similarities")

movie_similarity <- similarity(rating_matrix[, 1:4], method = "cosine", which = "items")
as.matrix(movie_similarity)
image(as.matrix(movie_similarity), main = "Movie's Similarities")

# Extract unique ratings
rating_values <- as.vector(rating_matrix@data)
unique(rating_values)

ratings_table <- table(rating_values)
ratings_table

# Visualize most viewed movies
movie_views <- colCounts(rating_matrix)
table_views <- data.frame(movie = names(movie_views), 
                                        views = movie_views)
table_views <- table_views[order(table_views$views, decreasing = TRUE), ]
table_views$title <- as.character(movie_data[match(table_views$movie, movie_data$movieId), "title"])
head(table_views)

# Plot top films
ggplot(head(table_views), aes(x = reorder(title, -views), y = views)) +
  geom_bar(stat="identity", fill = "steelblue") +
  geom_text(aes(label = views), vjust = -0.3, size = 3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Total Views of Top Films")

# Plot movie ratings
image(rating_matrix[1:20, 1:25], axes = FALSE, main = "Heatmap of first 20 rows and 25 volumns")

# Prepare data for recommendation engine
movie_ratings <- rating_matrix[rowCounts(rating_matrix) > 50,
                               colCounts(rating_matrix) > 50]
movie_ratings

minimum_movies<- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                    colCounts(movie_ratings) > minimum_users],
      main = "Heatmap of the top users and movies")

# Visualize average ratings per user
average_ratings <- rowMeans(movie_ratings)
qplot(average_ratings, fill = I("steelblue"), col = I("red")) + 
  ggtitle("Distribution of the average rating per user")

# Normalize ratings (remove bias)
normalized_ratings <- normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)

image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
                         colCounts(normalized_ratings) > minimum_users],
      main = "Normalized Ratings of the Top Users")


# Binarize data
binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)

high_rated_films <- binarize(movie_ratings, minRating = 3)
image(high_rated_films[rowCounts(movie_ratings) > binary_minimum_movies,
                       colCounts(movie_ratings) > binary_minimum_users],
      main = "Heatmap of the top users and movies")


# Item Based Collaborative Filtering
sampled_data <- sample(x = c(TRUE, FALSE), 
                       size = nrow(movie_ratings),
                       replace = TRUE,
                       prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]

# Build recommendation system
recommendation_system <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

recommendation_model <- Recommender(data = training_data,
                                    method = "IBCF",
                                    parameter = list(k = 30))
recommendation_model
class(recommendation_model)

model_info <- getModel(recommendation_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items],
      main = "Heatmap of the first rows and columns")

sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)

sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill = I("steelblue"), col = I("red")) + 
  ggtitle("Distribution of title count")

# Generate recommendations using predict() to identify similar items 
# and rank them appropriately
top_recommendations <- 10
predicted_recommendations <- predict(object = recommendation_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations

user1 <- predicted_recommendations@items[[1]]
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movie_data$title[match(movies_user1, movie_data$movieId)]
movies_user2

recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x) { as.integer(colnames(movie_ratings)[x]) })
recommendation_matrix[, 1:4]
