# function to read in the csv file containing trainee names, Scopus ID's,and other metadata (start, finish, gender, URM, etc.)

require(vroom)
require(tools)
require(stats)
require(dplyr)

load_file <- function(name, path) {
  ext <- tools::file_ext(name)
  switch(ext,
         csv = vroom::vroom(path, delim = ","),
         tsv = vroom::vroom(path, delim = "\t"),
         validate("Invalid file; Please upload a .csv or .tsv file")
  )
}


#   Example usage, for a file named "BUResearchers.csv" that is in the currtent working directory:

#Trainees <- load_file("BUResearchers.csv", "BUResearchers.csv")
#concat_IDvector <- Trainees %>% pull(ID) %>% paste(collapse =",")
#concat_IDvector
