#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
library(shinyjs)

# Create synthetic 5x5 data
synthetic_data <- as.data.frame(matrix(round(runif(25, 1, 100), 0), nrow = 5, ncol = 5))
colnames(synthetic_data) <- paste("Column", 1:5)
rownames(synthetic_data) <- paste("Row", 1:5)

ui <- fluidPage(
  useShinyjs(),  # Enables JavaScript utilities
  titlePanel("Shiny Table Example"),
  
  tabsetPanel(
    tabPanel("Tab 1",
             br(),
             DTOutput("data_table"),
             br(),
             tags$div(
               id = "about_container",
               style = "margin-top: 15px;",
               actionLink("toggle_about", label = tags$span("About", HTML("&rarr;")), style = "font-weight: bold; font-size: 16px;"),
               hidden(
                 div(
                   id = "about_text",
                   style = "margin-top: 10px;",
                   p("This table presents a 5x5 grid of synthetic numerical data. It is meant to simulate a basic dataset for demonstration purposes. The rows and columns are labeled generically to show how tabular data can be displayed in a clean, professional format in a Shiny application.")
                 )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  output$data_table <- renderDT({
    datatable(synthetic_data,
              options = list(dom = 't',  # Removes filter and pagination controls
                             ordering = FALSE,
                             columnDefs = list(list(className = 'dt-center', targets = "_all"))),
              rownames = TRUE,
              class = 'cell-border stripe compact hover')
  })
  
  observeEvent(input$toggle_about, {
    toggle("about_text")
  })
}

shinyApp(ui, server)