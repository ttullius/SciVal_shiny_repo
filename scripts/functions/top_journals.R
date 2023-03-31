library(tidyverse)
library(DT)
library(dplyr)
require(httr)
library(xml2)
library(XML)
library(ggplot2)


Trainees <- read_csv("BUResearchers_full.csv")

concat_IDvector <- Trainees %>% pull(ID) %>% paste(collapse =",")

require(httr)

headers = c(
  `Accept` = 'application/xml'
)

params = list(
  `metricTypes` = 'PublicationsInTopJournalPercentiles',
  `authors` = '6701858763,20433296900',
  `yearRange` = '10yrs',
  `includeSelfCitations` = 'true',
  `byYear` = 'true',
  `includedDocs` = 'AllPublicationTypes',
  `journalImpactType` = 'CiteScore',
  `showAsFieldWeighted` = 'false',
  `indexType` = 'hIndex',
  `apiKey` = '7f59af901d2d86f78a1fd60c1bf9426a'
)

url_xml <- httr::GET(url = 'https://api.elsevier.com/analytics/scival/author/metrics', httr::add_headers(.headers=headers), query = params)
  


  raw_xml <- read_xml(url_xml)
  my_xml = xmlParse(raw_xml)
  
  df_years <- xmlToDataFrame(my_xml, colClasses = c(rep("integer",10)), homogeneous = NA, 
                             collectNames = FALSE, nodes = getNodeSet(my_xml, "//valueByYear"))
  df_id <- xmlToDataFrame(my_xml, colClasses = c("numeric"), homogeneous = NA,
                          collectNames = FALSE, nodes = getNodeSet(my_xml, "//id"))
  df_names <- xmlToDataFrame(my_xml, colClasses = c("character"), homogeneous = NA,
                             collectNames = FALSE, nodes = getNodeSet(my_xml, "//name"))
  df_dataSource <- xmlToDataFrame(my_xml, colClasses = c("character", "integer", "integer", "character"), homogeneous = NA,
                             collectNames = FALSE, nodes = getNodeSet(my_xml, "//dataSource"))
  df_SciValMetric <- bind_cols(df_names, df_id, df_years)
  df_SciValMetric
  
  colnames(df_SciValMetric) <- 
    c("name", "ID", df_dataSource$metricStartYear:df_dataSource$metricEndYear)
  df_SciValMetric
  
  #add in the enter_date column from the original BUReseachers dataframe using dplyr inner_join
  #df_SciValMetric$ID <- as.double(df_SciValMetric$ID)
  
  df_papers_by_year <- Trainees %>% inner_join(df_SciValMetric, by="ID") %>% select(-name)
  
  df_papers_by_year$ID<-as.factor(df_papers_by_year$ID)
  df_papers_by_year$enter_date<-as.factor(df_papers_by_year$enter_date)
  df_papers_by_year$finish<-as.factor(df_papers_by_year$finish)
  df_papers_by_year$gender<-as.factor(df_papers_by_year$gender)
  df_papers_by_year$Tags<-as.factor(df_papers_by_year$Tags)
  df_papers_by_year$job<-as.factor(df_papers_by_year$job)
  
  as_tibble(df_papers_by_year)
  

  
  summarised <- 
  df_papers_by_year %>% 
    group_by(job) %>% 
    summarise('2012' = sum(`2012`), '2013' = sum(`2013`),'2014' = sum(`2014`),'2015' = sum(`2015`),'2016' = sum(`2016`),'2017' = sum(`2017`), '2018' = sum(`2018`), '2019' = sum(`2019`), '2020' = sum(`2020`), '2021' = sum(`2021`)) 
#%>%mutate_if(is.numeric, round, digits = 2)
summarised

tidy_metrics <- gather(data = summarised, 
                       key = year, value = papers, -job)
tidy_metrics

ggplot(tidy_metrics) +
  geom_line(aes(x = year, y = papers, 
                group = job, color = job)) +
  theme_minimal() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "gray")) +
  ylab("") +
  ggtitle("Number of papers published each year by BU Bioinformatics alumni, by job ", 
          ) + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
