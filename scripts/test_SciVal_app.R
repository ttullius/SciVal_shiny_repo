# Load packages ----

library(tidyverse)
library(DT)
library(dplyr)
require(httr)
library(xml2)
library(XML)
library(ggplot2)
require(vroom)
require(tools)
require(stats)
library(leaflet)
library(leaflet.extras)

# Source helper functions ----

source("helpers.R")

Trainees <- read_csv("/Users/tom/Dropbox (BOSTON UNIVERSITY)/R/Shiny_map/data/BUResearchers_full.csv")

ID_list <- Trainees %>% pull(ID) %>% paste(collapse =",")
ID_list

num_rows <- nrow(Trainees)
num_rows

  
####### collect and summarise metrics for each trainee from SciVal
  
   
##############  produce dataframes for each SciVal metric of interest. These are examples - more can be added! ##############
    
    
    SciValMetric <-  "ScholarlyOutput"
    metric_name <- "number_of_papers"
    full_df_scholarly_output <- makeSciValMetricDF(ID_list, num_rows, SciValMetric = "ScholarlyOutput", metric_name = "number_of_papers")
    full_df_scholarly_output
    
    
    SciValMetric <-  "FieldWeightedCitationImpact"
    metric_name <- "FWCI"
    full_df_fwci <- makeSciValMetricDF(ID_list, num_rows, SciValMetric = "FieldWeightedCitationImpact", metric_name = "FWCI")
    full_df_fwci
    
    
    SciValMetric <-  "CitationCount"
    metric_name <- "number_of_citations"
    full_df_citation_count <- makeSciValMetricDF(ID_list, num_rows, SciValMetric = "CitationCount", metric_name = "number_of_citations")
    full_df_citation_count
    
    
    SciValMetric <-  "HIndices"
    metric_name <- "H_index"
    full_df_h_index <- makeSciValMetricDF(ID_list, num_rows, SciValMetric = "HIndices", metric_name = "H_index")
    full_df_h_index
    
   
#######  combine the metric dataframes, merge with the trainee metadata datframe that was read in as a CSV, and clean up variable types  ############### 
    
    df_list <- list(full_df_scholarly_output, full_df_citation_count, full_df_fwci, full_df_h_index) 
    all_df <- df_list %>% reduce(full_join, by = 'ID')
    all_df <- Trainees %>% inner_join(all_df, by = 'ID')
    all_df
    
    factor_list <- c('ID', 'enter_date', 'finish', 'gender', 'Tags', 'job')
    all_df <- all_df %>% mutate(across(factor_list, ~as.factor(.)))
    
    double_list <- c('number_of_papers', 'number_of_citations', 'FWCI', 'H_index')
    all_df <- all_df %>% mutate(across(double_list, ~as.double(.)))
    
    all_df <- all_df %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
    all_df
    
    
  ####  group entries by selected criterion and summarise  ################

  
    metrics_summarised <- all_df %>% 
      group_by(enter_date) %>% 
      summarise(number = n(), mean_number_of_citations = mean(number_of_citations), mean_H_index = mean(H_index), mean_FWCI = mean(FWCI), mean_number_of_papers = mean(number_of_papers)) %>%
      mutate_if(is.numeric, round, digits = 0)
    metrics_summarised 
      
  


    
    
    
    
####  Call the makeSciValPapersAllYearsDF helper function to retrieve number of papers by year for each trainee   ###########
     
     full_df_papers_by_year <- makeSciValPapersAllYearsDF(ID_list, num_rows)
     full_df_papers_by_year
     
     full_df_papers_by_year <- Trainees %>% inner_join(full_df_papers_by_year, by="ID") 
     full_df_papers_by_year

     
     #%>% select(-name)
     
     full_metric_list <- c('ID', 'gender', 'Tags', 'job')
     full_df_papers_by_year <- full_df_papers_by_year %>% mutate(across(full_metric_list, ~as.factor(.)))
     full_df_papers_by_year <- full_df_papers_by_year %>%
       mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
     
     
     as_tibble(full_df_papers_by_year)
     
   
   ####  group entries by selected criterion  ################
   
   allPapers_summarised <- full_df_papers_by_year %>%
       group_by(finish) %>% 
       summarise(number = n(), '2012' = sum(`2012`), '2013' = sum(`2013`),'2014' = sum(`2014`),'2015' = sum(`2015`),'2016' = sum(`2016`),'2017' = sum(`2017`), '2018' = sum(`2018`), '2019' = sum(`2019`), '2020' = sum(`2020`), '2021' = sum(`2021`)) 
     allPapers_summarised
     
  
  #############  make tidy dataframe for plotting  ##############   
     
  
    tidy_metrics <- gather(data = allPapers_summarised, 
                            key = year, value = papers, -finish, -number)
     tidy_metrics <- transform(tidy_metrics,
                                  year = as.numeric(year))
     tidy_metrics
     
     #view structure of new data frame
     str(tidy_metrics)
  
     tidy_metrics <- mutate(tidy_metrics, years_out = year - finish)
     tidy_metrics
     
     tidy_sum <- tidy_metrics %>% 
       group_by(years_out) %>%
       summarise(papers = sum(papers))
     tidy_sum
     
##########################  make plots  ##################################    
     
    tidy_sum %>% 
       
       ggplot(aes(x = years_out, y = papers, 
                  group = gender, color = gender)) +
       geom_line()+
       theme_minimal() +
       theme(panel.border = element_blank(), 
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(), 
             axis.line = element_line(colour = "gray")) +
       ylab("") +
       ggtitle("Number of papers published each year by BU Bioinformatics alumni ", 
       ) + 
       theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
     
     
     
    
    metrics_summarised %>%
      
      ggplot(aes(x = enter_date, y = mean_number_of_papers)) + 
      geom_col() +
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
    #    +   ggtitle("average number of papers by .data[[input$group]]")
  
    
    metrics_summarised %>%
      
      ggplot(aes(x = enter_date, y = mean_FWCI)) + 
      geom_col() +
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
    #    +  ggtitle("average field-weighted citation impact by .data[[input$group]]")
    
  
    metrics_summarised %>%
      
      ggplot(aes(x = enter_date, y = mean_H_index)) + 
      geom_col() +
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
    #    +  ggtitle("average H_index by .data[[input$group]]")
    
  
    metrics_summarised %>%
      
      ggplot(aes(x = enter_date, y = mean_number_of_citations)) + 
      geom_col() +
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
    #    +  ggtitle("average number of citations by .data[[input$group]]")
    
  
 
  
    
    df_jobs <- Trainees %>% group_by(job) %>% summarise(n = n())
    
    ggplot(df_jobs, aes(x = "", y = n, fill = job)) +
      geom_col(color = "black") +
      geom_text(aes(label = n),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      scale_fill_brewer() +
      theme_void()
  