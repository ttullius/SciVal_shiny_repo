library(dplyr)
trainees_meta <- read.csv("BUResearchers_full.csv")
trainees_papers <- read.csv("papers_2016_2021.csv")
df <- trainees_meta %>% inner_join(trainees_papers, by = 'ID')
as_tibble(df)
df
