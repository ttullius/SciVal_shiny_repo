library(readxl)
library(dplyr)
library(purrr)
library(janitor)
library(stringr)
library(here)
library(tidyverse)
library(stringr)

df_xl <- read_excel(here::here("data", "raw", "mySciVal_Researchers_Export 2-17-23.xlsx"))
head(df_xl)

df4 <- lapply(df_xl["Author"], function(x) stringr::word(1, 2))
head(df4)
df4

firstname <- df_xl %>% sapply(strsplit(Author, ' '), function(x) x[1])
firstname 


# combine two columns #

df1 <-unite(df_xl, col= "last_first", c("Author", "first"), sepc = "_")
head(df1)

# add a new variable containing 2 concatenated variables

df2 <- df_xl %>% mutate(
  last_first = paste(Author, first, sep = "_"))
  head(df2)
  


  
lastname = sapply(strsplit(Author, ' '), function(x) x[length(x)])
lastname
  
  
  df3 = strsplit(Author, ' ')

firstname = sapply(df3, function(x) x[1])
lastname = sapply(df3, function(x) x[length(x)])

