require(recommenderlab)
require(ggplot2)
require(data.table)
require(reshape2)

# Read in data
setwd("/Users/Bonhwang/Documents/GitHub/movierecs")
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
