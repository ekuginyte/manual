## Hypothesis:
#     Rating average across Adventure, Comedy and Drama genres will be the same.
#     H0: mu1 = mu2 = ... = mun
#     H1: mu1 ! = mu2 ! = ... ! = mun

## Function to analyse Movie Rating data
## Inputs:
#   data - Movie Rating data frame with columns: 
#     movieId - unique ID for the movie,
#     title - movie title (not unique),
#     year - year the movie was released,
#     genres - genres associated with the movie,
#     userId - unique ID for the user,
#     rating - a rating between 0 and 5 for the movie,
#     timestamp - date and time the rating was given.

## Outputs:
#   statistic - size of statistical the test;
#   power - power of the [xx] statistic.

# Load libraries
library(dslabs)
library(tidyverse)

# Load data
data("movielens")
data <- movielens

# Check head, present in markdown
head(data)
# Check the structure of the data frame
str(data)
n <- nrow(data)
missing_data <- data[rowSums(is.na(data)) > 0,]


# Check the input of data
# Check if the input data is appropriate for the [XX] function: 
# data must be data frame, have more than two rows, have values
#if (all(is.na(data)) || !is.data.frame(data) || nrow(data) < 2 {
#  stop("Invalid data frame")
#}
#
#if (!is.numeric(data$movieId) || !is.numeric(data$year) || 
#    !is.numeric(data$userId) || !is.numeric(data$rating) ||
#    !is.numreic(data$timestamp)) {
#  stop("Invalid data column xx")
#}

# Fill missing values for genres

## Data Wrangling
# Delete rows with missing values or??????
data <- na.omit(data)

any(is.na(data$movieId))
any(is.na(data$year))
any(is.na(data$genres))
any(is.na(data$userId))
any(is.na(data$timestamp))
any(is.na(data$rating))
any(is.na(data$title))

# Instead of deleting rows with unknown title column, take movieID and check 
# what title of the movie it should be ?????????????????????????????????????????? THE FUCK
#data$title[is.na(data$title)] <- data$title[data$movieId == data$movieId[is.na(data$title)]]

#data <- data %>% 
#  group_by(movieId) %>% 
#  fill(title) %>% 
#  ungroup


# Convert timestamp to a date format and create a data column
data <- data %>%
  mutate(date = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"), 
         year = format(date, "%Y"))
# Remove timestamp as date column has been created
data <- subset(data, select = -timestamp )











## Exploratory Data Analysis
# Plot explanatory variables against rating
# movieID and rating average
rating_movieID <- aggregate(data$rating, list(data$movieId), FUN = mean) 
p1 <- ggplot() +
      geom_smooth(aes(x = rating_movieID[, 1], y = rating_movieID[, 2]), 
                  alpha = 0.2, colour = "deepskyblue4") +
      labs(title = 'Movie ID and Rating', 
           y = 'Movie ID', x = 'Rating') +
      theme_minimal()

# Year of movie and rating average
rating_year <- aggregate(data$rating, list(data$year), FUN = mean) 
p2 <- ggplot() +
      geom_col(aes(x = rating_year[, 1], y = rating_year[, 2]), 
                  alpha = 0.2, colour = "deepskyblue4") +
      ylim(c(0, 5)) + 
      labs(title = 'Year of Movies and Rating', 
           y = 'Rating', x = 'Year') +
      theme(axis.text.x = element_text(angle = 90))

# User ID and rating average
rating_userID <- aggregate(data$rating, list(data$userId), FUN = mean) 
p3 <- ggplot() +
      geom_smooth(aes(x = rating_userID[, 1], y = rating_userID[, 2]), 
                  alpha = 0.2, colour = "deepskyblue4") +
      geom_point(aes(x = rating_userID[, 1], y = rating_userID[, 2]), 
                 alpha = 0.2, colour = "deepskyblue4") +
      labs(title = 'User ID and Rating', 
           y = 'Rating', x = 'User ID') +
      theme_minimal()

# Date of rating and rating average
rating_date <- aggregate(data$rating, list(data$date), FUN = mean) 
p4 <- ggplot() +
      geom_smooth(aes(x = rating_date[, 1], y = rating_date[, 2]), 
                  alpha = 0.2, colour = "deepskyblue4") +
      labs(title = 'Rating Date and Rating', 
           y = 'Rating', x = 'Rating Date') +
      theme_minimal()



# Regroup data by genres
data_genres <- data %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(number = n()) %>%
  arrange(desc(number)) %>% 
  as.data.frame()


# Plot most popular movies by genre
ggplot() +
  geom_col(aes(x = data_genres[1:19, 1], y = data_genres[1:19, 2]), 
              alpha = 0.2, colour = "deepskyblue4") +
  labs(title = 'Rating Popularity by Genre', 
       y = 'Rating Count', x = 'Genre') +
  theme(axis.text.x = element_text(angle = 90))

# Plot average rating by genre
rating_by_genre <- data %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(number = mean(rating)) %>%
  arrange(desc(number)) %>% 
  as.data.frame()

# Plot average rating by genre
ggplot() +
geom_col(aes(x = rating_by_genre[c(1, 3:19), 1], y = rating_by_genre[c(1, 3:19), 2]), 
       colour = "deepskyblue4", fill = "white", alpha = 0.2) +
  labs(title = 'Rating by Genre', 
       y = 'Rating', x = 'Genre') +
  theme(axis.text.x = element_text(angle = 90))















# Check distribution of the data, to then sample from it?
# Rating distribution for each movie genre
# Action
action_ratings <- data$rating[str_detect(data$genres, "Action")]
# Adventure
adventure_ratings <- data$rating[str_detect(data$genres, "Adventure")]
# Animation
animation_ratings <- data$rating[str_detect(data$genres, "Animation")]
# Children
children_ratings <- data$rating[str_detect(data$genres, "Children")]
# Comedy
comedy_ratings <- data$rating[str_detect(data$genres, "Comedy")]
# Crime
crime_ratings <- data$rating[str_detect(data$genres, "Crime")]
# Documentary
documentary_ratings <- data$rating[str_detect(data$genres, "Documentary")]
# Drama
drama_ratings <- data$rating[str_detect(data$genres, "Drama")]
# Fantasy
fantasy_ratings <- data$rating[str_detect(data$genres, "Fantasy")]
# Film-Noir
filmnoir_ratings <- data$rating[str_detect(data$genres, "Film-Noir")]
# IMAX
imax_ratings <- data$rating[str_detect(data$genres, "IMAX")]
# Musical
musical_ratings <- data$rating[str_detect(data$genres, "Musical")]
# Mystery
mystery_ratings <- data$rating[str_detect(data$genres, "Mystery")]
# Romance
romance_ratings <- data$rating[str_detect(data$genres, "Romance")]
# Sci-Fi
scifi_ratings <- data$rating[str_detect(data$genres, "Sci-Fi")]
# Thriller
thriller_ratings <- data$rating[str_detect(data$genres, "Thriller")]
# War
war_ratings <- data$rating[str_detect(data$genres, "War")]
# Western
western_ratings <- data$rating[str_detect(data$genres, "Western")]






# For reproducibility
#set.seed(777)
# Simple Monte Carlo function to calculate mean of rating by genre
# Input:
#  X - samples from rating vector.
# Output:
#  mean_rating - mean of rating.
my_mc <- function(rating_vector, n) {
  # For storing random samples
  x <- rep(NA, n)
  mean_rating <- NA
  # Generate samples with replacement from the rating vector
  x <- sample(x = rating_vector, size = n, replace = TRUE)
  # Compute mean of the rating
  mean_rating <- mean(x)
  # I wouldn't use return command normally in this place, since the Tidyverse
  # Style Guide advice against it, unless I wanted the return earlier in the 
  # function. However, I have heard this sometimes gets penalised at the 
  # university, hence, the return command at the end of the function.
  return(mean_rating)
}

# For reproducibility
#set.seed(555)
# Loop across all samples
# Output:
#  boot_res - store vector of the Pr(X > Y) distribution.
my_bts <- function(rating_vector, n_repeat = 1000, n = 1000) {
  # Pre-allocate memory for storing results
  rating_means <- rep(NA, n_repeat)
  mean_res <- rep(NA, n_repeat)
  # Compute vector of the Pr(X > Y) distribution, parallelise
  #mean_res <- foreach(i = 1:n_repeat, .combine = "c") %dopar% {
  #  my_mc(n)
  #}
  for (i in seq(n_repeat)) {
    mean_res[i] <- my_mc(rating_vector, n)
  }
  # Resample with replacement
  rating_means <- sample(x = mean_res, size = n_repeat, replace = TRUE)
  # Store results
  return(rating_means)
}


# Calculate power
# Begin loop
for (i in 1:n_repeat) {
  # Generate random sample
  samplepower <- rnorm (n=30, mean=26, sd=2.4)
  # Test mean of sample, if > zcrit value=1, otherwise value=0 
  if (mean(samplepower) > zcrit) {value[i] <- 1}
} # End loop
# Calculate proportion
power <- sum(value)/100000 |> round(3)
power



action <- my_bts(rating_vector = action_ratings, n_repeat, n)
adventure <- my_bts(rating_vector = adventure_ratings, n_repeat, n)
animation <- my_bts(rating_vector = action_ratings, n_repeat, n)
children <- my_bts(rating_vector = comedy_ratings, n_repeat, n)
comedy <- my_bts(rating_vector = action_ratings, n_repeat, n)
crime <- my_bts(rating_vector = comedy_ratings, n_repeat, n)
documentary <- my_bts(rating_vector = action_ratings, n_repeat, n)
drama <- my_bts(rating_vector = comedy_ratings, n_repeat, n)
fantasy <- my_bts(rating_vector = action_ratings, n_repeat, n)
filmnoir <- my_bts(rating_vector = comedy_ratings, n_repeat, n)
imax <- my_bts(rating_vector = action_ratings, n_repeat, n)
musical <- my_bts(rating_vector = comedy_ratings, n_repeat, n)
mystery <- my_bts(rating_vector = action_ratings, n_repeat, n)
romance <- my_bts(rating_vector = comedy_ratings, n_repeat, n)
scifi <- my_bts(rating_vector = action_ratings, n_repeat, n)
thriller <- my_bts(rating_vector = comedy_ratings, n_repeat, n)
war <- my_bts(rating_vector = action_ratings, n_repeat, n)
western <- my_bts(rating_vector = comedy_ratings, n_repeat, n)


# Combined vectors to a single data frame
simulated_data <- stack(data.frame(cbind(action, adventure, animation,
                                         children, comedy, crime, documentary,
                                         drama, fantasy, filmnoir, imax, 
                                         musical, mystery, romance, scifi, thriller,
                                         war, western)))




acion_n <- sum(str_detect(data$genres, "Action") == TRUE)
action_rating_mean <- aggregate(data$rating, list(str_detect(data$genres, "Action")), 
                                FUN = mean)[2, 2]




## Statistical test
#   One-way Anova
# https://support.minitab.com/en-us/minitab/21/help-and-how-to/statistics/power-and-sample-size/how-to/linear-models/power-and-sample-size-for-one-way-anova/interpret-the-results/all-statistics-and-graphs/
#
# Power of one-way Anova
#https://www.youtube.com/watch?v=i1f6_Jz5taw
#https://www.real-statistics.com/one-way-analysis-of-variance-anova/power-for-one-way-anova/
# The power of a one-way ANOVA is the probability that the test will determine 
# that the greatest difference between group means is statistically significant, 
# assuming that difference exists.

# Run One-way Anova test on each rating by genre distributions
test_statistic <- aov(values ~ ind, data = simulated_data)

# Run summary of Anova
test_statistic %>% 
  summary()

# Check which particular genres were the outliers
TukeyHSD(test_statistic, "ind", ordered = FALSE, conf.level = 0.95)

# Run non-parametric test statistic
s <- kruskal.test(values ~ ind, data = simulated_data)







# Run non-parametric Kruskal-Wallis test statistic
#kr_statistic[i] <- kruskal.test(values ~ ind, data = simulated_data)["p.value"]
# Compute power for Kruskal-Wallis statistic
#power_kruskal <- sum(kr_statistic <= 0.05) / length(kr_statistic)





# https://www.kaggle.com/code/redroy44/movielens-dataset-analysis
# https://rpubs.com/Benardi/movielens_boot





# Plot all of the simulated data distributions
p7 <- ggplot() +
        geom_density(aes(x = simulated_data[, 1], 
                         group = simulated_data[, 2], 
                         colour = simulated_data[, 2])) +
        labs(title = 'Simulated Means Distribution of Each Genre',
             y = 'Density', 
             x = 'Rating Mean') +
        theme_minimal()









