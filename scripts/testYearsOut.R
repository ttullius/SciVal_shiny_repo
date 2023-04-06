library(tidyverse)
library(dplyr)
library(ggplot2)


##############     read dataset, for testing. This dataset ("full_df_papers_by_year") was produced by test_SciVal_app.R   #########

full_df_papers_by_year <- read.csv("full_df_papers_by_year.csv")
full_df_papers_by_year

################################   produce tidy dataset, for plotting  #############

tidy_df <- full_df_papers_by_year %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    values_to = "papers",
    values_drop_na = TRUE)
tidy_df

tidy_df <- tidy_df %>% mutate(across('year', str_replace, 'X', ''))
tidy_df <- transform(tidy_df, year = as.numeric(year))
tidy_df <- mutate(tidy_df, years_out = year - finish)
full_metric_list <- c('ID', 'gender', 'Tags', 'job')
tidy_df <- tidy_df %>% mutate(across(full_metric_list, ~as.factor(.)))
tidy_df <- tidy_df %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
as_tibble(tidy_df)


#############   calculate total number of papers for each value of years_out and grouping  #########

tidy_sum <- tidy_df %>% 
  group_by(job, years_out) %>%
  summarise(number = n(), papers = sum(papers))
tidy_sum

#############   calculate average number of papers for each value of years_out and grouping  #########

tidy_sum_avg <- mutate(tidy_sum, avg_papers = papers/number)
tidy_sum_avg

#############   plot total number of papers for each years_out and grouping  #########

tidy_sum %>% 
  
  ggplot(aes(x = years_out, y = papers,
             group = job, color = job)) +
  geom_line()+
  theme_minimal() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "gray")) +
  ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


#############   plot average number of papers for each years_out and grouping  #########

tidy_sum_avg %>% 
  
  ggplot(aes(x = years_out, y = avg_papers,
             group = job, color = job)) +
  geom_line()+
  theme_minimal() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "gray")) +
  ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


##########  plot everything

tidy_df %>% 
  
  ggplot(aes(x = years_out, y = papers)) +
  geom_line()+
  theme_minimal() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "gray")) +
  ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

