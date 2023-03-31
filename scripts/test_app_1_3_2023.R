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
      plotOutput("plotPie")
    ),
    
    mainPanel(
      h1(paste0("BU Bioinformatics Trainees")),
      h2("Scholarly accomplishments"),
      #p("Some text"),
      tabsetPanel(
        tabPanel("map",h2("Where are our PhD graduates? (fake data!)"),leafletOutput(outputId = "mymap")),
        tabPanel("trainees", DTOutput("researcherTable")),
        tabPanel("metrics", DTOutput("metricsTable")),
        tabPanel("papers", plotOutput("plotPapers")),
        tabPanel("citations", plotOutput("plotCitations")),
        tabPanel("FWCI", plotOutput("plotFWCI")),
        tabPanel("H_index", plotOutput("plotH_index")),
        tabPanel("papers by year", DTOutput("allPapersTable"), plotOutput("plotPapers_all_years")),
        tabPanel("papers since PhD", DTOutput("papers_since_PhD_Table")),
        tabPanel("total papers by year since PhD", plotOutput("plot_total_Papers_years_since_PhD")),
        tabPanel("average papers by year since PhD", plotOutput("plot_average_Papers_years_since_PhD"))
    )
  )
)
)


# Define server logic  ----

server <- function(input, output) {
  
  
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
      addCircles(data = data_locations, lat = ~ latitude, lng = ~ longitude, weight = 5, radius = 2000, popup = ~as.character(paste0(company, sep = ", ",city)), label = ~as.character(job), color = ~pal2(job), fillOpacity = 0.5)
  })
  
  
  #########  Use the load_file helper function (from helpers.R file) to load the user-supplied file with trainee metadata (Scopus ID, gender, enter date, finish date, job type. etc.)  #####
  
  Trainees <- reactive({
    req(input$file1)
    load_file(input$file1$datapath)
  }) 
  
  output$researcherTable <- renderDataTable(

    Trainees(), 
    
    #filter='top', options = list(pageLength = 25, autowidth = TRUE)
    
  )
  
  
  num_rows <- reactive ({
    num_rows <- nrow(Trainees())
                     })
  
  
  ##############  SciVal throws an error if the number of Scopus ID's in the request is >199 
  ##############  Check how many trainees are listed in the trainee metadata file. 
  ##############  If >199, subset into two lists of trainees.  
  ##############  If <200, make a single list of trainees, for input into code for making Scopus ID list
  
  trainee_list <- reactive ({
    
    if(num_rows() > 199) {
      Trainees1 <- Trainees()[1:199, ]
      Trainees2 <- Trainees()[num_rows()-199:num_rows(), ]
      trainee_list <- list(Trainees1, Trainees2)
    }  else  {
      trainee_list <- list(Trainees())
    }
    
  })
  
  ###  construct a list of Scopus ID's from the trainee metadata file for submitting the SciVal API calls   #######
  
  ID_list <- reactive ({
    sapply(trainee_list(), get_ID_list)
    })
  
####### collect and summarise metrics for each trainee from SciVal    #####################
  
  
  metrics_df <- reactive ({
    
##############  produce dataframes for each SciVal metric of interest. Here are four example metrics - more can easily be added! ##############
    
    
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
    
    df_list <- list(full_df_scholarly_output, full_df_citation_count, full_df_fwci, full_df_h_index) 
    all_df <- df_list %>% reduce(full_join, by = 'ID')
    all_df <- Trainees() %>% inner_join(all_df, by = 'ID')
    
    factor_list <- c('ID', 'enter_date', 'finish', 'gender', 'Tags', 'job')
    all_df <- all_df %>% mutate(across(factor_list, ~as.factor(.)))
    
    double_list <- c('number_of_papers', 'number_of_citations', 'FWCI', 'H_index')
    all_df <- all_df %>% mutate(across(double_list, ~as.double(.)))
    
    all_df <- all_df %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
    
  }) 
    
  ####  group entries by selected criterion and summarise. This is output to a Table  ################
  
    metrics_summarised <- reactive ({
      metrics_df() %>% 
      group_by(.data[[input$group]]) %>% 
      summarise(number = n(), mean_number_of_citations=mean(number_of_citations), mean_H_index=mean(H_index), mean_FWCI=mean(FWCI), mean_number_of_papers = mean(number_of_papers)) %>%
      mutate_if(is.numeric, round, digits = 0)
    
  })
  
  ##############  produce a dataframe with the number of papers each year, for the past 10 years   ##############
  
   papers_all_years_df <- reactive ({
    
      ####  Call the makeSciValPapersAllYearsDF helper function to retrieve number of papers by year for each trainee   ###########
     
     full_df_papers_by_year <- makeSciValPapersAllYearsDF(ID_list(), num_rows())
    
     full_df_papers_by_year <- Trainees() %>% inner_join(full_df_papers_by_year, by="ID") 
     
     ##  %>% select(-name)
     
     #metric_list <- c('ID', 'enter_date', 'finish', 'gender', 'Tags', 'job')
     metric_list <- c('ID', 'enter_date', 'gender', 'Tags', 'job')
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
   
   
   #######################################    transform the "year" variable in the "papers by year" dataframe to the variable "years_out" (number of years since a trainee finished the PhD)
   
   papers_since_PhD <- reactive({
     
     tidy_df <- papers_all_years_df() %>%
       pivot_longer(
         cols = starts_with("20"),
         names_to = "year",
         values_to = "papers",
         values_drop_na = TRUE)
  
     double_list_tidy <- c('year')
     tidy_df <- tidy_df %>% mutate(across(double_list_tidy, ~as.double(.)))
     tidy_df <- mutate(tidy_df, years_out = year - finish)
     full_metric_list <- c('ID', 'gender', 'Tags', 'job')
     tidy_df <- tidy_df %>% mutate(across(full_metric_list, ~as.factor(.)))
     tidy_df <- tidy_df %>%
       mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
     
     as_tibble(tidy_df)
     
   })
     
     #############   calculate total number of papers for each value of years_out, for the user-selected group  #########
     
     papers_since_PhD_sum <-  reactive({
       
       tidy_df_sum <- papers_since_PhD() %>% 
       group_by(.data[[input$group]], years_out) %>%
       summarise(number = n(), papers = sum(papers))
       
       as_tibble(tidy_df_sum)
       
     })
     
     #############   calculate average number of papers for each value of years_out, for the user-selected group  #########

     papers_since_PhD_sum_avg <- reactive({
       
       papers_since_PhD_sum() %>% mutate(avg_papers = papers/number)
     
     })
     
     
 
   
   
###########   Tables   ###############

output$papers_since_PhD_Table   <- renderDataTable({
     
     papers_since_PhD_sum_avg()
     
   })
   
output$allPapersTable <- renderDataTable({
     
     allPapers_summarised()
    
 })

output$metricsTable <- renderDataTable({
  
  metrics_summarised()
 
})


###########   Plots   ###############

  output$plotPapers <- renderPlot({
    
    metrics_summarised() %>%
      
      ggplot(aes(x = .data[[input$group]], y = mean_number_of_papers)) + 
      geom_col(color = "#000000", fill = "#0099F8") +
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
    #    +   ggtitle("average number of papers by .data[[input$group]]")
    
  }) 
  
  
  output$plotFWCI <- renderPlot({
    
    metrics_summarised() %>%
      
      ggplot(aes(x = .data[[input$group]], y = mean_FWCI)) + 
      geom_col(color = "#000000", fill = "#0099F8") +
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
    #    +  ggtitle("average field-weighted citation impact by .data[[input$group]]")
    
  }) 
  
  output$plotH_index <- renderPlot({
    
    metrics_summarised() %>%
      
      ggplot(aes(x = .data[[input$group]], y = mean_H_index)) + 
      geom_col(color = "#000000", fill = "#0099F8") +
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
    #    +  ggtitle("average H_index by .data[[input$group]]")
    
  }) 
  
  output$plotCitations <- renderPlot({
    
    metrics_summarised() %>%
      
      ggplot(aes(x = .data[[input$group]], y = mean_number_of_citations)) + 
      geom_col(color = "#000000", fill = "#0099F8") +
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))
    
    #    +  ggtitle("average number of citations by .data[[input$group]]")
    
  }) 
  
  output$plotPapers_all_years <- renderPlot({
    
  tidy_metrics <- gather(data = allPapers_summarised(), 
                         key = year, value = papers, -.data[[input$group]])
  
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
  
  
  output$plot_total_Papers_years_since_PhD <- renderPlot({
  
    papers_since_PhD_sum() %>% 
    
    ggplot(aes(x = years_out, y = papers,
               group = .data[[input$group]], color = .data[[input$group]])) +
    geom_line()+
    theme_minimal() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "gray")) +
    ylab("") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
  })
  
  
  output$plot_average_Papers_years_since_PhD <- renderPlot({
    
    papers_since_PhD_sum_avg() %>% 
    
    ggplot(aes(x = years_out, y = avg_papers,
               group = .data[[input$group]], color = .data[[input$group]])) +
    geom_line()+
    theme_minimal() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "gray")) +
    ylab("") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
  })
}

shinyApp(ui, server)
