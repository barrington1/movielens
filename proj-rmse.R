library(tidyr)
library(dplyr)
library(tidyverse)
data(edx)
data(validation)

validation <- validation %>% 
    semi_join(edx, by = movieId) %>%
    semi_join(edx, by = userId)

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

user_avgs <- validation %>%
        left_join(movie_avgs, by = "movieId") %>%
        group_by(userId) %>%
        summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- validation %>%
        left_join(movie_avgs, by = "movieId") %>%
        left_join(user_avgs, by = "userId") %>%
        mutate(pred = mu + b_i + b_u) %>% 
        .$pred

model_2_rmse <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie + User effects Model",
                                     RMSE = model_2_rmse))

movie_avgs %>% qplot(b_i, geom ="histogram", 
                     bins = 10, data = ., color = I("black"))

user_avgs %>% qplot(b_u, geom ="histogram", 
                     bins = 10, data = ., color = I("blue"))

predicted_ratings <- mu + validation %>% 
                    left_join(movie_avgs, by='movieId') %>%
                    .$b_i

model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()