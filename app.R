# Load packages ----

library(shiny)
#library(gridlayout)
library(tidyverse)
library(DT)
library(dplyr)
require(httr)
library(xml2)
library(XML)
library(ggplot2)
library(bslib)
require(vroom)
require(tools)
require(stats)
library(leaflet)
library(leaflet.extras)
library(shinythemes)
library(thematic)

# Source helper functions ----

source("helpers.R")

my_theme <- bs_theme(bootswatch = "cerulean",
                     base_font = font_google("Open Sans"))

thematic_shiny(font = "auto")

# import location data  ----

data_locations <- read.csv("trainee_locations.csv")


# Define ui  ----

ui <- fluidPage(
  theme = my_theme,
  #shinythemes::themeSelector(),
  #theme = bslib::bs_theme(bootswatch = "sandstone"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1",
                "Choose CSV File with Trainee metadata",
                accept = ".csv"
      ),
      checkboxInput(
        inputId = "header",
        label = "Header",
        value = TRUE,
        width = "100%"
      ),
      radioButtons("group", "Group data by",
                   choices = c(Cohort = "enter_date",
                               Gender = "gender",
                               Job = "job"),
                   selected = "enter_date"),
      
      tags$hr(),
      
      
      checkboxGroupInput("checkGroup", label = h3("Choose jobs for 'years out' plots"), 
                         choices = list("Academia" = "Academia", "Academia (non-faculty)" = "Academia (non-faculty)", "Industry" = "Industry", "Research Institute" = "Research Institute", "Government Lab" = "Government Lab", "Entrepreneur" = "Entrepreneur"), 
                         selected = "Academia"),
     
      
      tags$hr(),
      
      #verbatimTextOutput("print"),
      
      #tags$hr(),
      
      plotOutput("plotPie")
      
    ),
    
    mainPanel(
      h1(paste0("BU Bioinformatics Trainees")),
      h2("Scholarly accomplishments"),
      #p("Some text"),
      tabsetPanel(
        
        tabPanel("map",h2("Where are our PhD graduates?"),leafletOutput(outputId = "mymap")),
        
        tabPanel("trainees", DT::dataTableOutput("researcherTable")),
        
        tabPanel("metrics", DT::dataTableOutput("metricsTable")),
        tabPanel("metrics plots", plotOutput("plotPapers"), plotOutput("plotFWCI"), plotOutput("plotCitations"), plotOutput("plotH_index")),
        
        tabPanel("years out", DT::dataTableOutput("yearsOutTable")),
        tabPanel("years out plots", plotOutput("plotYearsOutPapersAveraged"), plotOutput("plotYearsOutPapers")),
        
       
        tabPanel("papers by year", plotOutput("plotPapers_all_years"), DT::dataTableOutput("allPapersTable"))
    )
  )))




# Define server logic  ----

server <- function(input, output) {
  
  output$value <- renderPrint({ input$checkGroup })
  
  
# Define the color for  graduates' jobs  ----
  
  pal2 <- colorFactor(
    palette = c('blue', 'yellow', 'red', 'green'),
    domain = data_locations$job
  )
  
# create the map  ----
  
  output$mymap <- renderLeaflet({
    leaflet(data_locations) %>% 
      setView(lng = -99, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
      addTiles() %>% 
      #addCircles(data = data_locations, lat = ~ latitude, lng = ~ longitude, weight = 5, radius = 2000, popup = ~as.character(job), label = ~as.character(paste0("Job: ", sep = " ", job)), color = ~pal2(job), fillOpacity = 0.5)
      #addCircles(data = data_locations, lat = ~ latitude, lng = ~ longitude, weight = 5, radius = 2000, popup = ~as.character(paste0(company, sep = ", ",city)), label = ~as.character(job), color = ~pal2(job), fillOpacity = 0.5)
    addCircles(data = data_locations, lat = ~ latitude, lng = ~ longitude, weight = 5, radius = 2000, popup = ~as.character(paste0(city)), label = ~as.character(job), color = ~pal2(job), fillOpacity = 0.5)
  })
  


  #########  Use the load_file helper function (from helpers.R file) to load the user-supplied file with trainee metadata (Scopus ID, gender, enter date, finish date, job type. etc.)  #####
  
  Trainees <- reactive({
    req(input$file1)
    load_file(input$file1$datapath)
  }) 
  
  output$researcherTable <- DT::renderDataTable(
  
    Trainees(), 
    
    #filter='top', options = list(pageLength = 25, autowidth = TRUE)
    
  )
  
  num_rows <- reactive ({
    num_rows <- nrow(Trainees())
                     })
  
  ##############  SciVal throws an error if the number of Scopus ID's in the request is >100   ###       
  ##############  Check how many trainees are listed in the trainee metadata file. If >100, subset into two lists of trainees.  #########
  ##############  If number of trainees is <101, make a single list of trainees for input into code for making Scopus ID list   ####### 
  
  trainee_list <- reactive ({
    
    if(num_rows() > 100) {
      Trainees1 <- Trainees()[1:100, ]
      Trainees2 <- Trainees()[101:num_rows(), ]
      trainee_list <- list(Trainees1, Trainees2)
    }  else  {
      trainee_list <- list(Trainees())
    }
    
  })
  
  ###  construct a list of Scopus ID's from the trainee metadata file for submitting the SciVal API calls   #######
  ###  uses get_ID_list function in helpers.R
  
  ID_list <- reactive ({
    sapply(trainee_list(), get_ID_list)
    })
  
####### collect and summarise metrics for each trainee from SciVal    #####################
  
  metrics_df <- reactive ({
    
##############  produce dataframes for each SciVal metric of interest. Here are four example metrics - more can easily be added! 
##############  uses functions in helpers.R
    
    
    SciValMetric <-  "ScholarlyOutput"
    metric_name <- "number_of_papers"
    full_df_scholarly_output <- makeSciValMetricDF(ID_list(), num_rows(), SciValMetric = "ScholarlyOutput", metric_name = "number_of_papers")
    
    
    SciValMetric <-  "FieldWeightedCitationImpact"
    metric_name <- "FWCI"
    full_df_fwci <- makeSciValMetricDF(ID_list(), num_rows(), SciValMetric = "FieldWeightedCitationImpact", metric_name = "FWCI")
    
    
    SciValMetric <-  "CitationCount"
    metric_name <- "number_of_citations"
    full_df_citation_count <- makeSciValMetricDF(ID_list(), num_rows(), SciValMetric = "CitationCount", metric_name = "number_of_citations")
    
    
    SciValMetric <-  "HIndices"
    metric_name <- "H_index"
    full_df_h_index <- makeSciValMetricDF(ID_list(), num_rows(), SciValMetric = "HIndices", metric_name = "H_index")
    
   
#######  combine the metrics dataframes, merge with the trainee metadata dataframe that was read in as a CSV, and clean up variable types  ############### 
    
    #df_list <- list(full_df_scholarly_output, full_df_citation_count, full_df_fwci, full_df_h_index) 
    df_list <- mget(ls(pattern = "full_df"))
    all_df <- df_list %>% reduce(full_join, by = 'ID')
    all_df <- Trainees() %>% inner_join(all_df, by = 'ID')
    
    factor_list <- c('ID', 'enter_date', 'finish', 'gender', 'Tags', 'job')
    all_df <- all_df %>% mutate(across(factor_list, ~as.factor(.)))
    
    double_list <- c('number_of_papers', 'number_of_citations', 'FWCI', 'H_index')
    all_df <- all_df %>% mutate(across(double_list, ~as.double(.)))
    
    all_df <- all_df %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
    
  }) 
    
  ####  group entries by selected criterion and summarise  ################
  
    metrics_summarised <- reactive ({
      metrics_df() %>% 
      group_by(.data[[input$group]]) %>% 
      summarise(number = n(), mean_number_of_citations=mean(number_of_citations), mean_H_index=mean(H_index), mean_FWCI=mean(FWCI), mean_number_of_papers = mean(number_of_papers)) %>%
      mutate_if(is.numeric, round, digits = 0)
    
  })
  

  
  ####  Call the makeSciValPapersAllYearsDF helper function to retrieve number of papers by year for each trainee   ###########
  
   papers_all_years_df <- reactive ({
    
     
     full_df_papers_by_year <- makeSciValPapersAllYearsDF(ID_list(), num_rows())
     
     full_df_papers_by_year <- Trainees() %>% inner_join(full_df_papers_by_year, by="ID") 
     
     ##  %>% select(-name)
     
    # metric_list <- c('ID', 'enter_date', 'finish', 'gender', 'Tags', 'job')
     metric_list <- c('ID', 'gender', 'Tags', 'job')
     full_df_papers_by_year <- full_df_papers_by_year %>% mutate(across(metric_list, ~as.factor(.)))
     full_df_papers_by_year <- full_df_papers_by_year %>%
       mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
     
     
     as_tibble(full_df_papers_by_year)
     
   })   
   
   ####  group entries by selected criterion  ################
   
   allPapers_summarised <- reactive ({
     
     papers_all_years_df() %>% 
       group_by(.data[[input$group]]) %>% 
       summarise(number = n(), '2012' = sum(`2012`), '2013' = sum(`2013`),'2014' = sum(`2014`),'2015' = sum(`2015`),'2016' = sum(`2016`),'2017' = sum(`2017`), '2018' = sum(`2018`), '2019' = sum(`2019`), '2020' = sum(`2020`), '2021' = sum(`2021`)) 
     
   })   
   
  
  
  #################  transform "papers by year" dataset (papers_all_years_df()) into numbers of papers published each year relative to year of finishing PhD (years out)
  
  papers_years_out_df <- reactive ({
    
    tidy_df <- papers_all_years_df() %>%
    pivot_longer(
      cols = starts_with("2"),
      names_to = "year",
      values_to = "papers",
      values_drop_na = TRUE)
  
  tidy_df <- transform(tidy_df, year = as.numeric(year))
  tidy_df <- mutate(tidy_df, years_out = year - finish)
  full_metric_list <- c('ID', 'gender', 'Tags', 'job')
  tidy_df <- tidy_df %>% mutate(across(full_metric_list, ~as.factor(.)))
  tidy_df <- tidy_df %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
  as_tibble(tidy_df)
  
  
  ############.  subset  by list of jobs ##########
  job_list <- input$checkGroup
  tidy_df <- filter(tidy_df, job %in% job_list)
  
  as_tibble(tidy_df)
  
  })
  
  #############   calculate total number of papers for each value of years_out and grouping  #########
  
  papers_years_out_summarised_df <- reactive ({
  
  tidy_sum <- papers_years_out_df() %>% 
    group_by(job, years_out) %>%
    summarise(number = n(), papers = sum(papers))
 
  as_tibble(tidy_sum)
  
  })
  
  #############   calculate average number of papers for each value of years_out and grouping  #########
  
  papers_years_out_summarised_averaged_df <- reactive ({
    
    tidy_sum_avg <- mutate(papers_years_out_summarised_df(), avg_papers = papers/number)
  
  as_tibble(tidy_sum_avg)
  
  })
   
   ###############   produce tables  ####################
   
  
  #output$print <- renderPrint(input$checkGroup)
  
  
   
  output$allPapersTable <- DT::renderDataTable({
     
  allPapers_summarised()
    
 })

  output$metricsTable <- DT::renderDataTable({
  
  metrics_summarised()
 
})

  output$yearsOutTable <- DT::renderDataTable({
    
    papers_years_out_summarised_averaged_df()
    
  })
  
  ############# make plots ##################
  
  
  output$plotYearsOutPapers <- renderPlot({
    
    papers_years_out_summarised_df() %>% 
    
    ggplot(aes(x = years_out, y = papers,
               group = job, color = job)) +
    geom_line()+
    theme_minimal() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "gray")) +
    ylab("") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
  }) 
  
    
  output$plotYearsOutPapersAveraged <- renderPlot({
      
    papers_years_out_summarised_averaged_df() %>% 
    
    ggplot(aes(x = years_out, y = avg_papers,
               group = job, color = job)) +
    geom_line()+
    theme_minimal() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "gray")) +
    ylab("") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
  }) 
  
  
output$plotPapers <- renderPlot({
    
  metrics_summarised() %>%
      
      ggplot(aes(x = .data[[input$group]], y = mean_number_of_papers)) + 
      geom_col() +
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
    #    +   ggtitle("average number of papers by .data[[input$group]]")
    
  }) 
  
  
output$plotFWCI <- renderPlot({
    
    metrics_summarised() %>%
      
      ggplot(aes(x = .data[[input$group]], y = mean_FWCI)) + 
      geom_col() +
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
    #    +  ggtitle("average field-weighted citation impact by .data[[input$group]]")
    
  }) 
  
output$plotH_index <- renderPlot({
    
    metrics_summarised() %>%
      
      ggplot(aes(x = .data[[input$group]], y = mean_H_index)) + 
      geom_col() +
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
    #    +  ggtitle("average H_index by .data[[input$group]]")
    
  }) 
  
output$plotCitations <- renderPlot({
    
    metrics_summarised() %>%
      
      ggplot(aes(x = .data[[input$group]], y = mean_number_of_citations)) + 
      geom_col() +
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
    
    #    +  ggtitle("average number of citations by .data[[input$group]]")
    
  }) 
  
output$plotPapers_all_years <- renderPlot({
    
  tidy_metrics <- gather(data = allPapers_summarised(), 
                         key = year, value = papers, -number, -.data[[input$group]])
  
  ggplot(tidy_metrics) +
    geom_line(aes(x = year, y = papers, 
                  group = .data[[input$group]], color = .data[[input$group]])) +
    theme_minimal() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "gray")) +
    ylab("") +
    ggtitle("Number of papers published each year by BU Bioinformatics alumni ", 
    ) + 
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
  
}) 

output$plotPie <- renderPlot({
    
    df_jobs <- Trainees() %>% group_by(job) %>% summarise(n = n())
    
    ggplot(df_jobs, aes(x = "", y = n, fill = job)) +
      geom_col(color = "black") +
      geom_text(aes(label = n),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      scale_fill_brewer() +
      theme_void()
    
  })
  
}

shinyApp(ui, server)
