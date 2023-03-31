library(dplyr)
df1<-read.csv("1_75_Scholarly_Output_vs_Publication_Year.csv", header = TRUE)
head(df1)
df2<-read.csv("76_150_Scholarly_Output_vs_Publication_Year.csv", header = TRUE)
head(df2)

full_df12 <- union(df1, df2)
head(full_df12)

df3<-read.csv("150_Scholarly_Output_vs_Publication_Year.csv", header = TRUE)
df3

full_df123 <- union(full_df12, df3)
head(full_df123)
write.csv(full_df123, file = "~/all_papers_out.csv")