library(readxl)
library(dplyr)
library(purrr)
library(janitor)
library(stringr)

path <- here::here("SciVal PhD Alumni Data.xlsx")
df_excel <- path %>% 
  excel_sheets() %>%
  map_df(~read_excel(.x, path = path) %>%
           mutate(sheet_name = .x)) 

bioinformatics_meta_data_raw <- df_excel
bioinformatics_meta_data_processed <- bioinformatics_meta_data_raw
bioinformatics_meta_data_processed %>% 
  tibble::glimpse()
bioinformatics_meta_data_processed <- bioinformatics_meta_data_processed %>% 
  janitor::clean_names() 
#%>%  base::names()
bioinformatics_meta_data_processed %>% names()

bioinformatics_meta_data_processed %>% 
  clean_names() %>% 
  set_names(names(.) %>% str_replace_all("1st", "first")) %>%
  set_names(names(.) %>% str_replace_all("x", "")) %>% 
  glimpse()