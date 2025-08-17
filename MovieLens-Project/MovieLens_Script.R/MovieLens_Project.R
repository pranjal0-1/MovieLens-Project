# MovieLens Project R Script
# Author: Pranjal
# This script generates predictions and RMSE for MovieLens dataset

library(data.table)
library(dplyr)
library(stringr)
library(caret)

# Load ratings and movies
ratings <- fread(text = gsub("::", "\t", readLines("ml-10M100K/ratings.dat")),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines("ml-10M100K/movies.dat"), "\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId))

edx <- left_join(ratings, movies, by = "movieId")

# Split validation
set.seed(1)
validation_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
validation <- edx[validation_index,]
edx <- edx[-validation_index,]

# Model 1: Mean Only
mu <- mean(edx$rating)
rmse_mu <- RMSE(validation$rating, mu)

# Model 2: Movie Effect
b_i <- edx %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))
pred_movie <- validation %>% left_join(b_i, by="movieId") %>% mutate(pred = mu + b_i)
rmse_movie <- RMSE(validation$rating, pred_movie$pred)

# Model 3: Movie + User Effect
b_u <- edx %>% left_join(b_i, by="movieId") %>%
  group_by(userId) %>% summarize(b_u = mean(rating - mu - b_i))

pred_user <- validation %>% left_join(b_i, by="movieId") %>% left_join(b_u, by="userId") %>% 
  mutate(pred = mu + b_i + b_u)

rmse_user <- RMSE(validation$rating, pred_user$pred)

# Save results
rmse_results <- data.frame(
  Method = c("Mean Only", "Movie Effect", "Movie + User Effect"),
  RMSE = c(rmse_mu, rmse_movie, rmse_user)
)

print(rmse_results)
