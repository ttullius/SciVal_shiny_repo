
#######       function to collect a metric for each trainee, from SciVal       ######################

library(tidyverse)
library(DT)
library(dplyr)
require(httr)
library(xml2)
library(XML)
library(ggplot2)


### use https://curlconverter.com/r/ to generate a SciVal XML call using the curl command from SciVal (e.g. https://dev.elsevier.com/scival.html#!/SciVal_Author_Lookup_API/authorMetrics)

    getSciValMetric <- function(SciValMetric, author_ID_list, metric_name)    {
    headers = c(
      `Accept` = 'application/xml'
    )
    
    params = list(
      `metricTypes` = SciValMetric,
      `authors` = author_ID_list,
      `yearRange` = '10yrs',
      `includeSelfCitations` = 'true',
      `byYear` = 'false',
      `includedDocs` = 'AllPublicationTypes',
      `journalImpactType` = 'CiteScore',
      `showAsFieldWeighted` = 'false',
      `indexType` = 'hIndex',
      `apiKey` = '7f59af901d2d86f78a1fd60c1bf9426a'
    )
    
    url_xml <- httr::GET(url = 'https://api.elsevier.com/analytics/scival/author/metrics', httr::add_headers(.headers=headers), query = params) 
    raw_xml <- read_xml(url_xml)
    my_xml = xmlParse(raw_xml)
    
    df_metrics <- xmlToDataFrame(my_xml, homogeneous = NA,
                                 collectNames = FALSE, nodes = getNodeSet(my_xml, "//metrics"))
    df_id <- xmlToDataFrame(my_xml, homogeneous = NA,
                            collectNames = FALSE, nodes = getNodeSet(my_xml, "//id"))
    df_names <- xmlToDataFrame(my_xml, homogeneous = NA,
                               collectNames = FALSE, nodes = getNodeSet(my_xml, "//name"))
    
    df_SciValMetric <- bind_cols(df_names, df_id, df_metrics)
    colnames(df_SciValMetric) <- c("name", "ID", "metric", metric_name)
    df_SciValMetric <- as_tibble(df_SciValMetric)
    
    }
    
    #######################   function to produce a dataframe for each SciVal metric    ###########################
    
    makeSciValMetricDF <- function(ID_list,num_rows)    {
      
      df_metric_list <- lapply(ID_list, getSciValMetric, SciValMetric = SciValMetric, metric_name = metric_name)
      
      if(num_rows > 100) {
        full_df_metric <- bind_rows(df_metric_list[[1]], df_metric_list[[2]]) %>% select(-metric, -name)
      }  else  {
        full_df_metric <- df_metric_list[[1]] %>% select(-metric, -name)
      }
      
      full_df_metric$ID <- as.double(full_df_metric$ID)
      return(full_df_metric)
    }
   
    
     ############################  function to produce a list of Scopus ID's from the trainee metadata CSV that is read in    #####################################  
   
    get_ID_list <- function(trainee_list)    {
      author_ID_list <- trainee_list %>% pull(ID) %>% paste(collapse =",")
      
    }
    
    ############################  end functions    #####################################  
    
    
#####  code to use for trainee metadata list with greater than 100 trainees   ###############  
    
    Trainees <- read_csv("BUResearchers_full.csv", show_col_types = FALSE)
    
    num_rows <- nrow(Trainees)
    num_rows
    
    if(num_rows > 100) {
     Trainees1 <- Trainees[1:100, ]
     Trainees2 <- Trainees[num_rows-100:num_rows, ]
     trainee_list <- list(Trainees1, Trainees2)
    }  else  {
      trainee_list <- list(Trainees)
    }
    trainee_list
    
    
    ID_list <- sapply(trainee_list, get_ID_list)
    ID_list
    

   ##############  produce dataframes for each SciVal metric  ##############
    
    
    SciValMetric <-  "ScholarlyOutput"
    metric_name <- "number_of_papers"
    full_df_scholarly_output <- makeSciValMetricDF(ID_list, num_rows)
    full_df_scholarly_output
    
    
    SciValMetric <-  "FieldWeightedCitationImpact"
    metric_name <- "FWCI"
    full_df_fwci <- makeSciValMetricDF(ID_list, num_rows)
    full_df_fwci
    
    
    SciValMetric <-  "CitationCount"
    metric_name <- "number_of_citations"
    full_df_citation_count <- makeSciValMetricDF(ID_list, num_rows)
    full_df_citation_count
    
    
   #######  combine the metric dataframes, merge with the trainee metadata datframe that was read in as a CSV, and clean up variable types  ############### 
    
    df_list <- list(full_df_scholarly_output, full_df_citation_count, full_df_fwci) 
    df_list
    
    all_df <- df_list %>% reduce(full_join, by='ID')
    all_df
    
    all_df <- Trainees %>% inner_join(all_df, by="ID")
    all_df
    
    factor_list <- c('ID', 'enter_date', 'finish', 'gender', 'Tags', 'job')
    all_df <- all_df %>% mutate(across(factor_list, ~as.factor(.)))
    
    double_list <- c('number_of_papers', 'number_of_citations', 'FWCI')
    all_df <- all_df %>% mutate(across(double_list, ~as.double(.)))
    
    all_df <- all_df %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
    print(all_df, n=200)
    
 #######################   summarise metrics after grouping by metadata  ###################   
    
    metrics_summarised <- 
      all_df %>% 
      group_by(gender) %>% 
      summarise(mean_number_of_citations=mean(number_of_citations), mean_FWCI=mean(FWCI), mean_number_of_papers = mean(number_of_papers)) %>%
      mutate_if(is.numeric, round, digits = 2)
    
    metrics_summarised
    
    
 
    
    
    
    
    ############################  OLD CODE!!!!!#######################################
    
    
    
    
    
    
    df_scholarly_output_list <- lapply(ID_list, getSciValMetric, SciValMetric = "ScholarlyOutput", metric_name = "number_of_papers")
    df_scholarly_output_list
    
    if(num_rows > 199) {
      full_df_scholarly_output <- bind_rows(df_scholarly_output_list[[1]], df_scholarly_output_list[[2]]) %>% select(-metric, -name)
    }  else  {
      full_df_scholarly_output <- df_scholarly_output_list[[1]] %>% select(-metric, -name)
    }
  
  full_df_scholarly_output$ID <- as.double(full_df_scholarly_output$ID)
  full_df_scholarly_output
  
  
   
  df_citation_count_list <- lapply(ID_list, getSciValMetric, SciValMetric = "CitationCount", metric_name = "number_of_citations")
  df_citation_count_list
  
  if(num_rows > 199) {
    full_df_citation_count <- bind_rows(df_citation_count_list[[1]], df_citation_count_list[[2]]) %>% select(-metric, -name)
  }  else  {
    full_df_citation_count <- df_citation_count_list[[1]] %>% select(-metric, -name)
  }
  
  full_df_citation_count$ID <- as.double(full_df_citation_count$ID)
  full_df_citation_count

  
  
  df_fwci_list <- lapply(ID_list, getSciValMetric, SciValMetric = "FieldWeightedCitationImpact", metric_name = "FWCI")
  df_fwci_list

  if(num_rows > 199) {
    full_df_fwci <- bind_rows(df_fwci_list[[1]], df_fwci_list[[2]]) %>% select(-metric, -name)
  }  else  {
    full_df_fwci <- df_fwci_list[[1]] %>% select(-metric, -name)
  }
 
  full_df_fwci$ID <- as.double(full_df_fwci$ID)
  full_df_fwci

  
  df_list <- list(full_df_scholarly_output, full_df_citation_count, full_df_fwci) 
  df_list
  
  all_df <- df_list %>% reduce(full_join, by='ID')
  all_df
  
  all_df <- Trainees %>% inner_join(all_df, by="ID")
  all_df
  
  factor_list <- c('ID', 'enter_date', 'finish', 'gender', 'Tags', 'job')
  all_df <- all_df %>% mutate(across(factor_list, ~as.factor(.)))
  double_list <- c('number_of_papers', 'number_of_citations', 'FWCI')
  all_df <- all_df %>% mutate(across(double_list, ~as.double(.)))
  all_df <- all_df %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
  print(all_df, n=200)
  
  metrics_summarised <- 
    all_df %>% 
    group_by(enter_date) %>% 
    summarise(mean_number_of_citations=mean(number_of_citations), mean_FWCI=mean(FWCI), mean_number_of_papers = mean(number_of_papers)) %>%
    mutate_if(is.numeric, round, digits = 2)
  
  metrics_summarised
   
  
  
  
  
  
  
  
  
  
  ############################   OLD CODE!!!     #################################
  
   SciValMetric <-  "CitationCount"
   metric_name <- "number_of_citations"
   df_citation_count <- getSciValMetric(SciValMetric, author_ID_list, metric_name) %>% select(-metric, -name)
   df_citation_count$ID <- as.double(df_citation_count$ID)
   df_citation_count$number_of_citations <- as.double(df_citation_count$number_of_citations)
   df_citation_count
   
   
   SciValMetric <-  "FieldWeightedCitationImpact"
   metric_name <- "FWCI"
   df_fwci <- getSciValMetric(SciValMetric, author_ID_list, metric_name) %>% select(-metric, -name)
   df_fwci$ID <- as.double(df_fwci$ID)
   df_fwci$FWCI <- as.double(df_fwci$FWCI)
   df_fwci
   
   
   df_list <- list(df_scholarly_output, df_citation_count, df_fwci) 
   all_df <- df_list %>% reduce(full_join, by='ID')
   all_df <- Trainees %>% inner_join(all_df, by="ID")
   
   factor_list <- c('ID', 'enter_date', 'finish', 'gender', 'Tags', 'job')
   all_df <- all_df %>% mutate(across(factor_list, ~as.factor(.)))
   double_list <- c('number_of_papers', 'number_of_citations', 'FWCI')
   all_df <- all_df %>% mutate(across(double_list, ~as.double(.)))
   all_df <- all_df %>%
     mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
   print(all_df, n=200)

   metrics_summarised <- 
     all_df %>% 
     group_by(enter_date) %>% 
     summarise(mean_number_of_citations=mean(number_of_citations), mean_FWCI=mean(FWCI), mean_number_of_papers = mean(number_of_papers)) %>%
     mutate_if(is.numeric, round, digits = 2)
   
metrics_summarised
   
   
   
   
   ##################   ORIGINAL CODE - DON'T USE!!!!    ####################################
   
   
   ### add the enter_date, finish, gender, months, and job columns from the original Trainees dataframe to the Scholarly Output dataframe using dplyr inner_join
   
   M1 <- Trainees %>% inner_join(df_scholarly_output, by="ID") %>% select(-metric, -name)
   
   ### add the enter_date, finish, gender, months, and job columns from the original Trainees dataframe to the h-index dataframe using dplyr inner_join
   
   M2 <- Trainees %>% inner_join(df_citation_count, by="ID") %>% select(-metric, -name)
   
   ### add the enter_date, finish, gender, months, and job columns from the original Trainees dataframe to the FWCI dataframe using dplyr inner_join
   
   M3 <- Trainees %>% inner_join(df_fwci, by="ID") %>% select(-metric, -name)
   
   ### now combine the three metrics and trainee information into one dataframe, using the Scopus ID (ID) as the key to join the dataframes
   
   M1
   
   M1_M2 <- M2 %>% inner_join(M1, by=c("ID","Author","Tags","enter_date","gender","finish","months","First","job"))
   
   M1_M2
   
   M1_M2_M3 <- M3 %>% inner_join(M1_M2, by=c("ID","Author","Tags","enter_date","gender","finish","months","First","job"))
   
   M1_M2_M3
   
   ####   change ID, gender, enter_date, finish, and Tags to factors, so we can group on them. Change the three metrics to double.
   
   M1_M2_M3$ID<-as.factor(M1_M2_M3$ID)
   M1_M2_M3$enter_date<-as.factor(M1_M2_M3$enter_date)
   M1_M2_M3$finish<-as.factor(M1_M2_M3$finish)
   M1_M2_M3$gender<-as.factor(M1_M2_M3$gender)
   M1_M2_M3$Tags<-as.factor(M1_M2_M3$Tags)
   M1_M2_M3$FWCI<-as.double(M1_M2_M3$FWCI)
   M1_M2_M3$number_of_citations<-as.double(M1_M2_M3$number_of_citations)
   M1_M2_M3$number_of_papers<-as.double(M1_M2_M3$number_of_papers)
   M1_M2_M3 <- M1_M2_M3 %>%
     mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
   
   ####  group entries, then summarise H-index, FWCI, and number of papers for each group
   
   metrics_summarised <- 
     M1_M2_M3 %>% 
     group_by(enter_date) %>% 
     summarise(mean_number_of_citations=mean(number_of_citations), mean_FWCI=mean(FWCI), mean_number_of_papers = mean(number_of_papers)) %>%
     mutate_if(is.numeric, round, digits = 2)
   
   metrics_summarised
   
   
   metrics_summarised %>%
     ggplot(aes(x = .data$enter_date, y = .data$mean_number_of_papers)) + 
     geom_col()
     
     
  df_jobs <- Trainees %>% group_by(job) %>% summarise(n = n())
  
  ggplot(df_jobs, aes(x = "", y = n, fill = job)) +
    geom_col(color = "black") +
    geom_text(aes(label = n),
              position = position_stack(vjust = 0.5)) +
    coord_polar(theta = "y") +
    scale_fill_brewer() +
    theme_void()
  

   
   
 df_jobs <- Trainees() %>% group_by(job) %>% summarise(n = n())
     
     ggplot(df_jobs, aes(x = "", y = n, fill = job)) +
       geom_col(color = "black") +
       geom_text(aes(label = n),
                 position = position_stack(vjust = 0.5)) +
       coord_polar(theta = "y") +
       scale_fill_brewer() +
       theme_void()
     