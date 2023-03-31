library (dplyr)

df1<-read.csv("graduated_students_SciVal_cleaned.csv", header = TRUE)
head(df1)

df2<-read.csv("2023_03_Bioinformatics_metadata_cleaned.csv", header = TRUE)
head(df2)

df12<- left_join(df1, df2, by = 'Author')
head(df12)

head(df12)
write.csv(df12, file = "BUResearchers_full.csv")