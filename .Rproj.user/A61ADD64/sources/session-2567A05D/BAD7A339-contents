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
missing_data <- data[rowSums(is.na(data)) > 0, ]

# Data wrangling
data <- na.omit(data)
# Data with no genres listed deleted
data <- data[!(data$genres == "(no genres listed)"), ]
  
# Convert timestamp to a date format and create a data column
data <- data %>%
  mutate(date = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"), 
         year = format(date, "%Y"))
# Remove timestamp as date column has been created
data <- subset(data, select = -timestamp)

## Exploratory Data Analysis
# Year of movie and rating average
rating_year <- aggregate(data$rating, list(data$year), FUN = mean) 
p1 <- ggplot() +
  geom_col(aes(x = rating_year[, 1], y = rating_year[, 2]), 
           alpha = 0.2, colour = "deepskyblue4") +
  ylim(c(0, 5)) + 
  labs(title = 'Year of Movies and Rating', 
       y = 'Rating', x = 'Year') +
  theme(axis.text.x = element_text(angle = 90))


# User ID and rating average
rating_userID <- aggregate(data$rating, list(data$userId), FUN = mean) 
p2 <- ggplot() +
  geom_smooth(aes(x = rating_userID[, 1], y = rating_userID[, 2]), 
              alpha = 0.2, colour = "deepskyblue4") +
  geom_point(aes(x = rating_userID[, 1], y = rating_userID[, 2]), 
             alpha = 0.2, colour = "deepskyblue4") +
  labs(title = 'User ID and Rating', 
       y = 'Rating', x = 'User ID') +
  theme_minimal()


# Plot most popular movies by genre
ggplot() +
  geom_col(aes(x = data_genres[1:19, 1], y = data_genres[1:19, 2]), 
           alpha = 0.2, colour = "deepskyblue4") +
  labs(title = 'Rating Popularity by Genre', 
       y = 'Rating Count', x = 'Genre') +
  theme(axis.text.x = element_text(angle = 90))


# Plot average rating by genre
# Create data frame with average rating for each genre
rating_by_genre <- data %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(number = mean(rating)) %>%
  arrange(desc(number)) %>% 
  as.data.frame()
# Plot
ggplot() +
  geom_col(aes(x = rating_by_genre[c(1, 3:19), 1], y = rating_by_genre[c(1, 3:19), 2]), 
           colour = "deepskyblue4", fill = "white", alpha = 0.2) +
  labs(title = "Rating by Genre", 
       y = "Rating", x = "Genre") +
  theme(axis.text.x = element_text(angle = 90))

# Extract each genre listed in the data frame.
# Regroup data by genres.
data_genres <- data %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(number = n()) %>%
  arrange(desc(number)) %>% 
  as.data.frame()


# Rating probabilities for Adventure, Comedy and Drama genres
# Adventure ratings
adventure_ratings <- data$rating[str_detect(data$genres, "Adventure")]
# Comedy ratings
comedy_ratings <- data$rating[str_detect(data$genres, "Comedy")]
# Drama ratings
drama_ratings <- data$rating[str_detect(data$genres, "Drama")]
# Store vector of rating values
rating_values <- unique(data$rating) %>% 
  sort(decreasing = FALSE)

# Create data frame with all the rating values
acd_ratings <- data.frame(values = c(adventure_ratings,
                                     comedy_ratings,
                                     drama_ratings),
                          group = c(rep("Adventure", length(adventure_ratings)),
                                    rep("Comedy", length(comedy_ratings)),
                                    rep("Drama", length(drama_ratings))))
# Plot overlaid frequencies 
ggplot(acd_ratings, aes(x = values, colour = group)) +
  geom_freqpoly(position = "identity", bins = 10) +
  xlim(c(0.5, 5)) +
  labs(title = "Rating Frequencies", 
       y = "Frequency", x = "Rating") +
  theme_minimal()

# Compute initial probabilities for each value
# INPUT: 
#   genre_ratings - ratings taken from the data frame.
# OUTPUT: 
#   prob_values - probability values.
probabilities <- function(genre_ratings) {
  if (genre_ratings < )
  prob_values <- rep(NA, length(rating_values))
  for (i in rating_values) {
    prob_values <- c(prob_values, sum(genre_ratings == i) / length(genre_ratings))
  }
  return(prob_values[11:20])
}

# Store initial probabilities for the three genres
ad_prob_data <- probabilities(genre_ratings = adventure_ratings)
co_prob_data <- probabilities(genre_ratings = comedy_ratings)
dr_prob_data <- probabilities(genre_ratings = drama_ratings)

# Create data frame with all the rating values and probabilities
acd_ratings_prob <- data.frame(values = c(rating_values,
                                     rating_values,
                                     rating_values),
                          probabilities = c(ad_prob_data,
                                            co_prob_data,
                                            dr_prob_data),
                          group = c(rep("Adventure", length(ad_prob_data)),
                                    rep("Comedy", length(ad_prob_data)),
                                    rep("Drama", length(ad_prob_data))))

# Plot overlaid distributions
ggplot(acd_ratings_prob, aes(x = values, y = probabilities, colour = group)) +
  geom_path(position = "identity") +
  labs(title = "Rating Probabilities", 
       y = "Probabilities", x = "Rating") +
  theme_minimal()



# Monte Carlo simulation of new data sets, compute power and
# test size. 
# INPUTS:
#  n - sample size,
#  n_rep - times to compute power or test size,
#  H0 - null hypothesis, whether TRUE or FALSE,
#  ad_prob - probabilities of ratings for adventure genre,
#  co_prob - probabilities of ratings for comedy genre,
#  dr_prob - probabilities of ratings for adventure genre.
# OUTPUT:
#  results - data frame containing:
#     Power of Anova Statistic,
#     Test Size from Anova Statistic,
#     Power of Kruskal Wallis Statistic,
#     Test Size from Kruskal Wallis Statistic,
#     Effect Size,
#     Sample Size,
#     Number of Simulations.
my_simulation <- function(n = 1000, n_rep = 1000, H0, 
                          ad_prob, co_prob, dr_prob) {
  # Check if the number of samples n and number of simulations n_rep are
  # a single natural number
  if (n <= 0 || n %% 1 != 0 || !is.numeric(n) || length(n) > 1 || 
     !is.vector(n)) {
    stop("Invalid sample size!") 
  }
  if (n_rep <= 0 || n_rep %% 1 != 0 || is.na(n_rep) || 
      is.numeric(n_rep) == FALSE || length(n_rep) > 1 || !is.vector(n_rep)) {
    stop("Invalid simulation size!") 
  }
  # Check if the null hypothesis value is either TRUE or FALSE
  if (!H0 == TRUE && !H0 == FALSE) {
    stop("Invalid null hypothesis value!")
  }
  # Check if the probability vectors for each genre are of length 10
  if (!is.numeric(ad_prob) || any(is.na(ad_prob)) || !is.vector(ad_prob) || 
      length(ad_prob) != 10 || any(ad_prob >= 1) || any(ad_prob <= 0)) {
    stop("Invalid probabilities for adventure genre!")
  }
  if (!is.numeric(co_prob) || any(is.na(co_prob)) || !is.vector(co_prob) || 
      length(co_prob) != 10 || any(co_prob >= 1) || any(co_prob <= 0)) {
    stop("Invalid probabilities for comedy genre!")
  }
  if (!is.numeric(dr_prob) || any(is.na(dr_prob)) || !is.vector(dr_prob) || 
      length(dr_prob) != 10 || any(dr_prob >= 1) || any(dr_prob <= 0)) {
    stop("Invalid probabilities for drama genre!")
  }
  
  # To store power, effect size, test size
  power_anova <- power_kruskal <- eff_size <- size_anova <- size_kruskal <- 
    aov_statistic <- kr_statistic <- mean_eff_size <- NA
  # To store results
  results <- data.frame(rep(NA, 7))
  
  for (i in 1:n_rep) {
  # To store samples
  samp_ad <- samp_co <- samp_dr <- NA
  # T store simulated data
  simulated_data <- rep(NA, 3)
  # Generate samples from non-normal distribution with discrete values 
  # Adventure genre
  samp_ad <- sample(x = rating_values, n, replace = TRUE, prob = ad_prob) 
  # Comedy genre
  samp_co <- sample(x = rating_values, n, replace = TRUE, prob = co_prob) 
  # Drama genre
  samp_dr <- sample(x = rating_values, n, replace = TRUE, prob = dr_prob) 
  # Combined vectors to a single data frame
  simulated_data <- stack(data.frame(cbind(samp_ad, samp_co, samp_dr)))
  
  # Return the probability of making a correct decision when the null 
  # hypothesis is false
  # Run One-way Anova test on each rating by genre distributions
  aov_statistic[i] <- unlist(summary(aov(values ~ ind, 
                                         data = simulated_data)))["Pr(>F)1"]
  # Run non-parametric Kruskal-Wallis test statistic
  kr_statistic[i] <- kruskal.test(values ~ ind, data = simulated_data)["p.value"]
  
  # Effect size
  eff_size <- max(mean(samp_ad), mean(samp_co), mean(samp_dr)) - 
    min(mean(samp_ad), mean(samp_co), mean(samp_dr))
  }
  
  # Compute test size - probability of committing a Type I error, that is, of 
  # incorrectly rejecting the null hypothesis when it is true
  if (H0 == TRUE) {
    # Test size for Anova statistic
    size_anova <- sum(aov_statistic <= 0.05) / length(aov_statistic)
    # Test size for Kruskal-Wallis statistic
    size_kruskal <- sum(kr_statistic <= 0.05) / length(kr_statistic)
    # Mean effect size
    mean_eff_size <- mean(eff_size)
    # Create data frame to return
    results <- data.frame("Test_Size_Anova" = size_anova,
                          "Test_Size_Kruskal-Wallis" = size_kruskal,
                          "Mean_Effect_Size" = mean_eff_size,
                          "Sample_Size" = n * 3,
                          "Simulations" = n_rep,
                          "Null_Hypothesis" = "True")
  # Compute power - probability of correctly rejecting the null hypothesis 
  # when it is false
  }
  else {
    # Compute power for Anova statistic
    power_anova <- sum(aov_statistic <= 0.05) / length(aov_statistic)
    # Compute power for Kruskal-Wallis statistic
    power_kruskal <- sum(kr_statistic <= 0.05) / length(kr_statistic)
    # Mean effect size
    mean_eff_size <- mean(eff_size)
    # Create data frame to return
    results <- data.frame("Power_Anova" = power_anova,
                          "Power_Kruskal-Wallis" = power_kruskal,
                          "Mean_Effect_Size" = mean_eff_size,
                          "Sample_Size" = n * 3,
                          "Simulations" = n_rep,
                          "Null_Hypothesis" = "False")
  }
  # I wouldn't use return command normally in this place, since the Tidyverse
  # Style Guide advice against it, unless I wanted the return earlier in the 
  # function. However, I have heard this sometimes gets penalised at the 
  # university, hence, the return command at the end of the function.
  return(results)
}



# Monte Carlo simulation of new data sets, compute power and
# test size to display distributions.
# Selected power is from Kruskal-Wallis statistic, since it is more 
# robuts against non-normal distributions.
# INPUTS:
#  n - sample size,
#  n_rep - times to compute power or test size,
#  H0 - null hypothesis, whether TRUE or FALSE,
#  ad_prob - probabilities of ratings for adventure genre,
#  co_prob - probabilities of ratings for comedy genre,
#  dr_prob - probabilities of ratings for adventure genre.
# OUTPUT:
#  results - containing either:
#     Power of Kruskal Wallis Statistic,
#     Test Size from Kruskal Wallis Statistic.
my_simulation_distribution <- function(n = 1000, n_rep = 1000, H0, 
                                       ad_prob, co_prob, dr_prob) {
  # Check if the number of samples n and number of simulations n_rep are
  # a single natural number
  if (n <= 0 || n %% 1 != 0 || !is.numeric(n) || length(n) > 1 || 
      !is.vector(n)) {
    stop("Invalid sample size!") 
  }
  if (n_rep <= 0 || n_rep %% 1 != 0 || is.na(n_rep) || 
      is.numeric(n_rep) == FALSE || length(n_rep) > 1 || !is.vector(n_rep)) {
    stop("Invalid simulation size!") 
  }
  # Check if the null hypothesis value is either TRUE or FALSE
  if (!H0 == TRUE && !H0 == FALSE) {
    stop("Invalid null hypothesis value!")
  }
  # Check if the probability vectors for each genre are of length 10
  if (!is.numeric(ad_prob) || any(is.na(ad_prob)) || !is.vector(ad_prob) || 
      length(ad_prob) != 10 || any(ad_prob >= 1) || any(ad_prob <= 0)) {
    stop("Invalid probabilities for adventure genre!")
  }
  if (!is.numeric(co_prob) || any(is.na(co_prob)) || !is.vector(co_prob) || 
      length(co_prob) != 10 || any(co_prob >= 1) || any(co_prob <= 0)) {
    stop("Invalid probabilities for comedy genre!")
  }
  if (!is.numeric(dr_prob) || any(is.na(dr_prob)) || !is.vector(dr_prob) || 
      length(dr_prob) != 10 || any(dr_prob >= 1) || any(dr_prob <= 0)) {
    stop("Invalid probabilities for drama genre!")
  }
  
  # To store power, effect size, test size
  power_kruskal <- size_kruskal <- kr_statistic <- NA
  # To store results
  results <- NA
  
  for (i in 1:n_rep) {
    # To store samples
    samp_ad <- samp_co <- samp_dr <- NA
    # T store simulated data
    simulated_data <- rep(NA, 3)
    # Generate samples from non-normal distribution with discrete values 
    # Adventure genre
    samp_ad <- sample(x = rating_values, n, replace = TRUE, prob = ad_prob) 
    # Comedy genre
    samp_co <- sample(x = rating_values, n, replace = TRUE, prob = co_prob) 
    # Drama genre
    samp_dr <- sample(x = rating_values, n, replace = TRUE, prob = dr_prob) 
    # Combined vectors to a single data frame
    simulated_data <- stack(data.frame(cbind(samp_ad, samp_co, samp_dr)))
    
    # Return the probability of making a correct decision when the null 
    # hypothesis is false
    # Run non-parametric Kruskal-Wallis test statistic
    kr_statistic[i] <- kruskal.test(values ~ ind, data = simulated_data)["p.value"]
  }
  
  # Compute test size - probability of committing a Type I error, that is, of 
  # incorrectly rejecting the null hypothesis when it is true
  if (H0 == TRUE) {
    # Test size for Kruskal-Wallis statistic
    size_kruskal <- sum(kr_statistic <= 0.05) / length(kr_statistic)
    results <- size_kruskal
  }
  else {
    # Compute power for Kruskal-Wallis statistic
    power_kruskal <- sum(kr_statistic <= 0.05) / length(kr_statistic)
    results <- power_kruskal
  }
  return(results)
}



# Function to produce multiple power or test size values to then display
# their distribution in accordance with sample size
# INPUT:
#   samp_seq - vector with values to use as sample size for each power or
#              test size computation,
#   n_rep - times to compute power or test size,
#   H0 - null hypothesis, whether TRUE or FALSE,
#   ad_prob - probabilities of ratings for adventure genre,
#   co_prob - probabilities of ratings for comedy genre,
#   dr_prob - probabilities of ratings for adventure genre.
# OUTPUT:
#   outcome_values - either power or test size values from sample size.
outcome_distribution <- function(samp_seq, n_rep = 1000, H0, 
                                 ad_prob, co_prob, dr_prob) {
  # To store power values
  outcome_values <- NA
  # Swap samp_seq to n
  n <- samp_seq
  # Loop with sequence values
  for (i in samp_seq) {
    outcome_values <- c(outcome_values, 
                        my_simulation_distribution(n = i, n_rep, H0, ad_prob, 
                                                   co_prob, dr_prob))
  }
  return(outcome_values[-1])
}




## Hypothesis:
#     Rating average across Adventure, Comedy and Drama genres are the same.
#     H0: mu1 = mu2 = ... = mun
#     H1: mu1 ! = mu2 ! = ... ! = mun
# Hypothesis should be rejected.
# Scenario 1. samples with same probabilities as from the original data     ###
# For reproducibility.
set.seed(999)
sim1 <- my_simulation(n = 100, H0 = FALSE, 
              ad_prob = ad_prob_data, 
              co_prob = co_prob_data, 
              dr_prob = dr_prob_data)



# Scenario 2.                                                               ###
# Increasing the sample size of ratings for each genre, increases power
# For reproducibility.
set.seed(777)
sim2 <- my_simulation(n = 500, H0 = FALSE, 
              ad_prob = ad_prob_data, 
              co_prob = co_prob_data, 
              dr_prob = dr_prob_data)

# For reproducibility
set.seed(111)
# Store values of power from sample size to then plot
scenario1_2 <- outcome_distribution(samp_seq = seq(1, 600, 20), H0 = FALSE,
                                  ad_prob = ad_prob_data,
                                  co_prob = co_prob_data,
                                  dr_prob = dr_prob_data)
ggplot() +
  geom_smooth(aes(x = seq(1, 600, 20) * 3, y = scenario1_2), colour = "deepskyblue") +
  geom_hline(yintercept = 0.9, linetype = "dashed", 
             color = "deeppink", size = 0.5) +
  ylim(c(0, 1)) +
  labs(title = "Power Distribution for Scenarios 1 and 2", 
       y = "Power", x = "Sample Size") +
  theme_minimal()




# Scenario 3.                                                               ###
# The effect size will be changed when the means of each genre distribution are
# more or less different. In this case, probabilities of each rating value
# are made more similar.
# For reproducibility.
set.seed(555)
sim3 <- my_simulation(n = 500, H0 = TRUE, 
              ad_prob = rep(0.1, 10), 
              co_prob = rep(0.1, 10), 
              dr_prob = rep(0.1, 10))

# For reproducibility
set.seed(111)
scenario3 <- outcome_distribution(samp_seq = seq(1, 600, 20), H0 = TRUE,
                                  ad_prob = ad_prob_data,
                                  co_prob = co_prob_data,
                                  dr_prob = dr_prob_data)

ggplot() +
  geom_smooth(aes(x = seq(1, 600, 20) * 3, y = scenario3), colour = "deepskyblue") +
  geom_hline(yintercept = 0.9, linetype = "dashed", 
             color = "deeppink", size = 0.5) +
  ylim(c(0, 1)) +
  labs(title = "Test Size Distribution for Scenario 3", 
       y = "Test Size", x = "Sample Size") +
  theme_minimal()




# Scenario 4.                                                               ###
# Probabilities are slightly different.
set.seed(333)
sim4 <- my_simulation(n = 5000, H0 = FALSE, 
              ad_prob = rep(0.1, 10), 
              co_prob = c(0.08, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.12), 
              dr_prob = rep(0.1, 10))

# For reproducibility
set.seed(111)
scenario4 <- outcome_distribution(samp_seq = seq(1, 5500, 300), H0 = FALSE,
                                  ad_prob = rep(0.1, 10),
                                  co_prob = c(0.2, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 
                                              0.1, 0.05, 0.05),
                                  dr_prob = rep(0.1, 10))

ggplot() +
  geom_smooth(aes(x = seq(1, 5500, 300) * 3, y = scenario4), colour = "deepskyblue") +
  geom_hline(yintercept = 0.9, linetype = "dashed", 
             color = "deeppink", size = 0.5) +
  ylim(c(0, 1)) +
  labs(title = "Test Size Distribution for Scenario 4", 
       y = "Test Size", x = "Sample Size") +
  theme_minimal()