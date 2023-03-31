library(dplyr)
library(tidyverse)
df1 <- read_csv("BUResearchers.csv")
df2 <- read_csv("student_data.csv")
df1
df2

df3<-inner_join(df1,df2, by="Author")
df3
BUResearchers_full <- df3 %>% select(Author, First, ID, Tags, enter_date,finish,months,job)
BUResearchers_full
write_csv(BUResearchers_full, "BUResearchers_full.csv")
