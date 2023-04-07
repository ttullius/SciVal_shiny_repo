library(shiny)
  
  ui <- fluidPage(
  
  # Copy the chunk below to make a group of checkboxes
  #checkboxGroupInput("checkGroup_test", label = h3("Checkbox group"), 
                     #choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                     #selected = 1),
  
  checkboxGroupInput("checkGroup", label = h3("Choose job types"), 
                     choices = list("Academia" = "Academia", "Academia (non-faculty)" = "Academia (non-faculty)", "Industry" = "Industry", "Research Institute" = "Research Institute"),
                     selected = "academia"),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value")))
  
)

server <- function(input, output) {
  
  # You can access the values of the widget (as a vector)
  # with input$checkGroup, e.g.
  output$value <- renderPrint({ input$checkGroup })
  
}

shinyApp(ui, server)