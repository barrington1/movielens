#' MovieLens Recommendation
#' Bobby Arrington
#' @barringtontx
#' June 06/04/2019
#' 
library(tidyr)
library(dplyr)
library(tidyverse)

#data(validation)
#Create test and validation sets
###################################
# Create edx set and validation set
###################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

### Building the Recommendation System

mu_hat <- mean(edx$rating)
mu_hat

naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse

predictions <- rep(2.5, nrow(validation))
RMSE(validation$rating, predictions)

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

# fit <- lm(rating ~ as.factor(userId), data = movielens)
mu <- mean(edx$rating) 
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

## Displaying the User Avg Histogram
movie_avgs %>% qplot(b_i, geom ="histogram", 
                     bins = 10, data = ., color = I("black"))


## Creating the Movie Predictions
predicted_movie_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

## Creating the Movie Density Prediction Histogram
hist(predicted_movie_ratings, freq = F)

## Displaying the Summarized User Predictions Table
density(predicted_movie_ratings)

## Creating the RMSE Predictions for Final Table
model_1_rmse <- RMSE(predicted_movie_ratings, validation$rating)

## The Results of combined Movie + User effects Model
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

## Creating the User Avg 
user_avgs <- validation %>%
        left_join(movie_avgs, by = "movieId") %>%
        group_by(userId) %>%
        summarize(b_u = mean(rating - mu - b_i))

## Creating the User Avg Predictions
predicted_user_ratings <- validation %>%
        left_join(movie_avgs, by = "movieId") %>%
        left_join(user_avgs, by = "userId") %>%
        mutate(pred = mu + b_i + b_u) %>% 
        .$pred

## Displaying the User Avg Histogram
user_avgs %>% qplot(b_u, geom ="histogram", 
                    bins = 10, data = ., color = I("blue"))

## Creating the User Density Prediction Histogram
hist(predicted_user_ratings, freq = F)

## Displaying the Summarized User Predictions Table
density(predicted_user_ratings)

## Creating the RMSE Predictions for Final Table
model_2_rmse <- RMSE(predicted_user_ratings, validation$rating)

## The Results of combined Movie + User effects Model
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie + User effects Model",
                                     RMSE = model_2_rmse))

### Final output of the RMSE results

rmse_results %>% knitr::kable()