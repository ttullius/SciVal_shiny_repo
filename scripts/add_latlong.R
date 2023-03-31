library(dplyr)
df1 <- read.csv("Bioinformatics_metadata_noforeign.csv", header = TRUE)
head(df1)
str(df1)
df2 <- read.csv("zip_codes_states.csv", header = TRUE)
head(df2)
str(df2)

df3 <- inner_join(df1, df2, by = "zip_code")
df3
write.csv(df3, file = "trainee_locations.csv")