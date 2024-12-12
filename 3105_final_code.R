library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(markovchain)
library(nnet)
library(reshape2)

#downloaded individual comma seperated CSV files containing the full 24 season data for 15 pitchers from MLB's Baseball Savant 
#link: https://baseballsavant.mlb.com/statcast_search

#pulling in data for all players 

#starters
player_669373_full_data <- read.csv("~/Desktop/Pitch_Data/Starters/669373_data.csv", header = TRUE, sep = ",")
player_676979_full_data <- read.csv("~/Desktop/Pitch_Data/Starters/676979_data.csv", header = TRUE, sep = ",")
player_656557_full_data <- read.csv("~/Desktop/Pitch_Data/Starters/656557_data.csv", header = TRUE, sep = ",")
player_694973_full_data <- read.csv("~/Desktop/Pitch_Data/Starters/694973_data.csv", header = TRUE, sep = ",")
player_656427_full_data <- read.csv("~/Desktop/Pitch_Data/Starters/656427_data.csv", header = TRUE, sep = ",")

#relievers
player_621381_full_data <- read.csv("~/Desktop/Pitch_Data/Relievers/621381_data.csv", header = TRUE, sep = ",")
player_669276_full_data <- read.csv("~/Desktop/Pitch_Data/Relievers/669276_data.csv", header = TRUE, sep = ",")
player_641755_full_data <- read.csv("~/Desktop/Pitch_Data/Relievers/641755_data.csv", header = TRUE, sep = ",")
player_518585_full_data <- read.csv("~/Desktop/Pitch_Data/Relievers/518585_data.csv", header = TRUE, sep = ",")
player_573204_full_data <- read.csv("~/Desktop/Pitch_Data/Relievers/573204_data.csv", header = TRUE, sep = ",")

#closers
player_656945_full_data <- read.csv("~/Desktop/Pitch_Data/Closers/656945_data.csv", header = TRUE, sep = ",")
player_623352_full_data <- read.csv("~/Desktop/Pitch_Data/Closers/623352_data.csv", header = TRUE, sep = ",")
player_695243_full_data <- read.csv("~/Desktop/Pitch_Data/Closers/695243_data.csv", header = TRUE, sep = ",")
player_661403_full_data <- read.csv("~/Desktop/Pitch_Data/Closers/661403_data.csv", header = TRUE, sep = ",")
player_621242_full_data <- read.csv("~/Desktop/Pitch_Data/Closers/621242_data.csv", header = TRUE, sep = ",")


#processing the data 
#adding in pitch number for each game and simplifying data set

#starters
player_669373_simplified <- player_669373_full_data %>%
  select(pitcher, player_name, game_date, batter, pitch_type) %>%
  group_by(game_date) %>%
  mutate(pitch_number = rev(row_number())) %>%  
  ungroup()

player_676979_simplified <- player_676979_full_data %>%
  select(pitcher, player_name, game_date, batter, pitch_type) %>%
  group_by(game_date) %>%
  mutate(pitch_number = rev(row_number())) %>%  
  ungroup()

player_656557_simplified <- player_656557_full_data %>%
  select(pitcher, player_name, game_date, batter, pitch_type) %>%
  group_by(game_date) %>%
  mutate(pitch_number = rev(row_number())) %>%  
  ungroup()

player_694973_simplified <- player_694973_full_data %>%
  select(pitcher, player_name, game_date, batter, pitch_type) %>%
  group_by(game_date) %>%
  mutate(pitch_number = rev(row_number())) %>%  
  ungroup()

player_656427_simplified <- player_656427_full_data %>%
  select(pitcher, player_name, game_date, batter, pitch_type) %>%
  group_by(game_date) %>%
  mutate(pitch_number = rev(row_number())) %>%  
  ungroup()

#relievers
player_621381_simplified <- player_621381_full_data %>%
  select(pitcher, player_name, game_date, batter, pitch_type) %>%
  group_by(game_date) %>%
  mutate(pitch_number = rev(row_number())) %>%  
  ungroup()

player_669276_simplified <- player_669276_full_data %>%
  select(pitcher, player_name, game_date, batter, pitch_type) %>%
  group_by(game_date) %>%
  mutate(pitch_number = rev(row_number())) %>%  
  ungroup()

player_641755_simplified <- player_641755_full_data %>%
  select(pitcher, player_name, game_date, batter, pitch_type) %>%
  group_by(game_date) %>%
  mutate(pitch_number = rev(row_number())) %>%  
  ungroup()

player_518585_simplified <- player_518585_full_data %>%
  select(pitcher, player_name, game_date, batter, pitch_type) %>%
  group_by(game_date) %>%
  mutate(pitch_number = rev(row_number())) %>%  
  ungroup()

player_573204_simplified <- player_573204_full_data %>%
  select(pitcher, player_name, game_date, batter, pitch_type) %>%
  group_by(game_date) %>%
  mutate(pitch_number = rev(row_number())) %>%  
  ungroup()

#closers
player_656945_simplified <- player_656945_full_data %>%
  select(pitcher, player_name, game_date, batter, pitch_type) %>%
  group_by(game_date) %>%
  mutate(pitch_number = rev(row_number())) %>%  
  ungroup()

player_623352_simplified <- player_623352_full_data %>%
  select(pitcher, player_name, game_date, batter, pitch_type) %>%
  group_by(game_date) %>%
  mutate(pitch_number = rev(row_number())) %>%  
  ungroup()

player_695243_simplified <- player_695243_full_data %>%
  select(pitcher, player_name, game_date, batter, pitch_type) %>%
  group_by(game_date) %>%
  mutate(pitch_number = rev(row_number())) %>%  
  ungroup()

player_661403_simplified <- player_661403_full_data %>%
  select(pitcher, player_name, game_date, batter, pitch_type) %>%
  group_by(game_date) %>%
  mutate(pitch_number = rev(row_number())) %>%  
  ungroup()

player_621242_simplified <- player_621242_full_data %>%
  select(pitcher, player_name, game_date, batter, pitch_type) %>%
  group_by(game_date) %>%
  mutate(pitch_number = rev(row_number())) %>%  
  ungroup()

#combining all pitchers into 1 data frame
full_pitchers_data <- bind_rows(
  list(
    #starters
    "669373" = player_669373_simplified,
    "676979" = player_676979_simplified,
    "656557" = player_656557_simplified,
    "694973" = player_694973_simplified,
    "656427" = player_656427_simplified,
    #relievers
    "621381" = player_621381_simplified,
    "669276" = player_669276_simplified,
    "641755" = player_641755_simplified,
    "518585" = player_518585_simplified,
    "573204" = player_573204_simplified,
    #closers
    "656945" = player_656945_simplified,
    "623352" = player_623352_simplified,
    "695243" = player_695243_simplified,
    "661403" = player_661403_simplified,
    "621242" = player_621242_simplified 
  ), .id = "pitcher"
)

#adding a column of the randomized pitches
full_pitchers_data <- full_pitchers_data %>%
  group_by(pitcher) %>% 
  mutate(
    randomized_pitch = sample(
      pitch_type,  
      size = n(),  
      replace = TRUE 
    )
  ) %>%
  ungroup()

#Code used to create all summary statistics and graphs

#calculating proportions of pitches for each pitcher 
pitch_proportions <- full_pitchers_data %>%
  group_by(pitcher, player_name, game_date, pitch_type) %>%
  summarize(count = n(), .groups = "drop") %>%
  group_by(pitcher, game_date) %>%
  mutate(proportion = count / sum(count))

pitcher_order <- c("Skubal, Tarik", "Crochet, Garrett", "Houck, Tanner", "Skenes, Paul", "Flaherty, Jack", "Strahm, Matt", "Lee, Dylan", "Kinley, Tyler", "Cruz, Fernando", "Thielbar, Caleb", "Scott, Tanner", "Hader, Josh", "Miller, Mason", "Clase, Emmanuel", "Díaz, Edwin")

pitch_proportions$player_name <- factor(pitch_proportions$player_name, levels = pitcher_order)

#plotting pitch proportions - graphical summary 
ggplot(pitch_proportions, aes(x = game_date, y = proportion, color = pitch_type, group = pitch_type)) +
  geom_line() +
  facet_wrap(~ player_name, scales = "free_y", nrow = 3, ncol = 5) +
  labs(
    title = "Pitch Type Proportions by Pitcher - 2024",
    x = "Game Date",
    y = "Proportion",
    color = "Pitch Type"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10),
    legend.position = "bottom"
  )

ggplot(full_pitchers_data, aes(x = pitch_type, fill = player_name)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Pitch Type Distribution Across Pitchers",
    x = "Pitch Type",
    y = "Count",
    fill = "Pitcher"
  ) +
  theme_minimal()

#each player's pitch arsenol 
pitch_arsenal <- full_pitchers_data %>%
  group_by(player_name) %>%
  summarize(num_unique_pitches = n_distinct(pitch_type))

ggplot(pitch_arsenal, aes(x = reorder(player_name, -num_unique_pitches), y = num_unique_pitches)) +
  geom_bar(stat = "identity", fill = "#D50032", color = "#002D72") +
  labs(
    title = "Number of Pitches in Each Pitcher's Arsenal",
    x = "Pitcher",
    y = "Number of Pitches"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#Code used to do the analysis
#model

#permutation test
permutation_test <- function(pitch_types){
  
  #calculating true test stat
  true_counts <- table(pitch_types)
  true_stat <- chisq.test(true_counts)$statistic
  
  #generating random sequences
  permutation_stats <- replicate(1000, {
    random_sequence <- sample(pitch_types, size = length(pitch_types), replace = TRUE)
    random_counts <- table(random_sequence)
    chisq.test(random_counts)$statistic
  })
  
  #p-value
  p_value <- mean(permutation_stats >= true_stat)
  return(p_value)
}

#applying permutation test for each pitcher
pitcher_p_vals <- full_pitchers_data %>%
  group_by(pitcher, player_name) %>%
  summarise(p_value = permutation_test(pitch_type))

#adjusting p-vals using Bonferroni correction 
pitcher_p_vals <- pitcher_p_vals %>%
  mutate(adjusted_p_value = p.adjust(p_value, method = "bonferroni"))

#visualizing the results

#bar plot of adjusted p-values 
ggplot(pitcher_p_vals, aes(x = reorder(player_name, adjusted_p_value), y = adjusted_p_value)) +
  geom_bar(stat = "identity", fill = "#002D72") +
  geom_hline(yintercept = 0.05, color = "#D50032", linetype = "dashed") +
  annotate("text", x = length(pitcher_p_vals$player_name) + .5, y = 0.05, 
           label = "Significance Threshold (0.05)", color = "#D50032", size = 4) +
  coord_flip() +
  labs(
    title = "Adjusted P-Values for Pitcher Sequence Randomness",
    x = "Pitcher",
    y = "Adjusted P-Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

#dot plot to compare true sequence to randomized expectations
pitcher_stats <- full_pitchers_data %>%
  group_by(player_name) %>%
  summarise(
    true_stat = chisq.test(table(pitch_type))$statistic
  )

null_dist <- replicate(5000, chisq.test(table(sample(full_pitchers_data$pitch_type)))$statistic)

null_df <- data.frame(statistic = null_dist)

ggplot(null_df, aes(x = statistic)) +
  geom_histogram(bins = 30, fill = "grey", alpha = 0.6) +
  geom_vline(data = pitcher_stats, aes(xintercept = true_stat, color = player_name), linetype = "dashed") +
  labs(
    title = "Pitcher Observed Statistic vs Null Distribution",
    x = "Chi-Squared Statistic",
    y = "Frequency",
    color = "Pitcher"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

#directly comparing pitch arsenal and chi square stat
combined_stats <- pitch_arsenal %>%
  inner_join(pitcher_stats, by = "player_name")

scaling_factor <- max(combined_stats$num_unique_pitches) / max(combined_stats$true_stat)

ggplot(combined_stats, aes(x = reorder(player_name, -num_unique_pitches))) +
  geom_bar(aes(y = num_unique_pitches), stat = "identity", fill = "#002D72", alpha = 0.7) +
  geom_point(aes(y = true_stat * scaling_factor), color = "#D50032", size = 3) +
  scale_y_continuous(
    name = "Number of Pitch Types",
    sec.axis = sec_axis(~ . / scaling_factor, name = "Chi-Squared Statistic")
  ) +
  labs(
    title = "Pitch Arsenal Size and Chi-Squared Statistic by Player",
    x = "Pitcher"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "#002D72"),
    axis.title.y.right = element_text(color = "#D50032"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

#heatmap of p-vals
ggplot(pitcher_p_vals, aes(x = "Pitcher", y = reorder(player_name, adjusted_p_value), fill = adjusted_p_value)) +
  geom_tile() +
  scale_fill_gradient(low = "#002D72", high = "#D50032", name = "Adjusted P-Value") +
  geom_text(aes(label = round(adjusted_p_value, 3)), color = "#FFFFFF", size = 3) +
  labs(
    title = "Heatmap of Adjusted P-Values by Pitcher",
    x = "",
    y = "Pitcher"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )


#putting model in context - comparing model to baseline
#generating baseline pitching arsenal 

baseline_arsenal <- c("FF", "SL", "CH")
baseline_num_pitches <- 100

set.seed(123)
baseline_sequence <- sample(baseline_arsenal, size = baseline_num_pitches, replace = TRUE)
baseline_true_frequencies <- table(baseline_sequence)

expected_baseline_frequencies <- rep(baseline_num_pitches/ length(baseline_arsenal), length(baseline_arsenal))

#chi-square test
baseline_chi_square_test <- chisq.test(baseline_true_frequencies, p = expected_baseline_frequencies / sum(expected_baseline_frequencies))
cat("Chi-Square Statistic:", baseline_chi_square_test$statistic, "\n")
cat("P-value:", baseline_chi_square_test$p.value, "\n")

#permutation test 
baseline_permutation_test <- function(sequence, num_permutations = 5000) {
  observed_counts <- table(sequence)
  observed_stat <- chisq.test(observed_counts)$statistic
  
  #random sequence
  permuted_stats <- replicate(num_permutations, {
    random_sequence <- sample(sequence, size = length(sequence), replace = TRUE)
    random_counts <- table(random_sequence)
    chisq.test(random_counts)$statistic
  })
  
  #p-value
  p_value <- mean(permuted_stats >= observed_stat)
  return(list(statistic = observed_stat, p_value = p_value, permuted_stats = permuted_stats))
}

#run permutation test
test_results <- baseline_permutation_test(baseline_sequence)
cat("Permutation Test Statistic:", test_results$statistic, "\n")
cat("Permutation Test P-value:", test_results$p_value, "\n")

comparison <- data.frame(
  Model = c("Baseline Sequence", "Random Permutations"),
  Statistic = c(baseline_chi_square_test$statistic, test_results$statistic),
  P_Value = c(baseline_chi_square_test$p.value, test_results$p_value)
)
print(comparison)

null_data <- data.frame(statistic = test_results$permuted_stats)

ggplot(null_data, aes(x = statistic)) +
  geom_histogram(bins = 30, fill = "grey", alpha = 0.7) +
  geom_vline(xintercept = test_results$statistic, color = "#002D72", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = baseline_chi_square_test$statistic, color = "#D50032", linetype = "dashed", size = 1) +
  labs(
    title = "Null Distribution of Permuted Chi-Square Statistics",
    x = "Chi-Square Statistic",
    y = "Frequency"
  ) +
  theme_minimal() +
  annotate("text", x = test_results$statistic, y = 20, label = "Observed Stat", color = "#002D72") +
  annotate("text", x = baseline_chi_square_test$statistic, y = 20, label = "Baseline Stat", color = "#D50032")