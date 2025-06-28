# Loading Libraries

library(shiny)
library(bslib)
library(tidyverse)
library(fredr)
library(hrbrthemes)

# Updating all real-time data to update the summary table
source("Summary_Sheet_Calc.R")

# Loading Summary Table Data

summary_table <- read_csv("Summary_Sheet.csv")

# Loading Important Dates

important_dates <- as_date(c("2021-01-20","2022-08-09", "2022-08-16", "2025-01-20", "2025-04-02"))

# Downloading Manufacturing Goal One Data

Total_Manufacturing <- read_csv("./Data/goal_one_data.csv")
Manufacturing_ISM_PMI <- read_csv("./Data/ism_pmi.csv")
Manufacturing_Employment <- read_csv("./Data/MANEMP.csv")
Manufacturing_New_Orders <- read_csv("Data/AMTMNO.csv")
Manufacturing_New_Shipments <- read_csv("Data/AMTMVS.csv")
Manufacturing_Industrial_Production <- read_csv("Data/IPMAN.csv")
Manufacturing_Industrial_Capacity <- read_csv("Data/CAPGMFS.csv")
Manufacturing_Factory_Investments <- read_csv("Data/C307RX1Q020SBEA.csv")
Manufacturing_datasets <- list(
  "ISM PMI" = Manufacturing_ISM_PMI,
  "Manufacturing Employment" = Manufacturing_Employment,
  "Manufacturing New Orders" = Manufacturing_New_Orders,
  "Manufacturing Value of Shipments" = Manufacturing_New_Shipments,
  "Industrial Production" = Manufacturing_Industrial_Production,
  "Industrial Capacity" = Manufacturing_Industrial_Capacity,
  "Factory Investments" = Manufacturing_Factory_Investments
)

# Downloading Global Public Goods Two data
GPC_dollars <- read_csv("Data/DTWEXBGS.csv")
Military_spending <- read_csv("Data/military_spending.csv")

# Downloading Adherence Goal Three Data and sorting into vectors

adherence <- read_csv("Data/TTT_Countries.csv")
green_countries <- adherence %>% 
  filter(Adherence == "Full") %>% 
  arrange(desc(GDP))
green_countries <- green_countries$Country
yellow_countries <- adherence %>% 
  filter(Adherence == "Partial") %>% 
  arrange(desc(GDP))
yellow_countries <- yellow_countries$Country
red_countries <- adherence %>% 
  filter(Adherence == "No") %>% 
  arrange(desc(GDP))
red_countries <- red_countries$Country

# Downloading Economic Goal Four Data

Total_Economic <- read_csv("./Data/goal_four_data.csv")
Economy_Financial_Stress_Index <- read_csv("./Data/STLFSI4.csv")
Economy_Unemployment <- read_csv("./Data/U5RATE.csv")
Economy_Expenditures <- read_csv("Data/A794RX0Q048SBEA.csv")
Economy_Wages <- read_csv("Data/LES1252881600Q.csv")
Economy_Sentiment <- read_csv("Data/UMCSENT.csv")
Economy_Inflation <- read_csv("Data/MEDCPIM158SFRBCLE.csv")
Economy_datasets <- list(
  "Financial Stress Index" = Economy_Financial_Stress_Index,
  "Unemployment (U5)" = Economy_Unemployment,
  "Real Personal Consumption Expenditures" = Economy_Expenditures,
  "Median Usual Weekly Earnings" = Economy_Wages,
  "Consumer Sentiment" = Economy_Sentiment,
  "Median CPI (Inflation)" = Economy_Inflation
)

# Downloading Trade Goal Six Data

Total_Trade <- read_csv("Data/goal_six_data.csv")
Trade_BOP <- read_csv("Data/USABCAGDPBP6.csv")
Trade_CA <- read_csv("Data/IEABC.csv")
Trade_datasets <- list(
  "Balance of Payments" = Trade_BOP,
  "Current Account" = Trade_CA
)

# Downloading Treasury Goal Seven Data
Debt_Servicing_GDP <- read_csv("Data/goal_seven_data.csv")

# Downloading Tax Goal Eight Data
Total_Tax <- read_csv("Data/goal_eight_data.csv")

# Loading Custom Functions

# Function 1
plot_manufacturing_function <- function(data, labs, dates){ #manufacturing for now
  data <- data %>% 
    filter(date > "2021-01-19") %>% 
    ggplot(aes(x = date)) +
    geom_point(aes(y = value)) +
    geom_point(aes(y = value_10yravg), color = "green") +
    geom_point(aes(y = value_1yravg), color = "blue") +
    geom_line(aes(y = value, color = "Current Data"), size = 1) + #color = ", #A7C7E7"
    geom_line(aes(y = value_10yravg, color = "Long Term (10year) Average"), size = 1) +
    geom_line(aes(y = value_1yravg, color = "Short Term (1year) Average"), size = 1) +
    scale_color_manual(values = c(
      `Current Data` = "black",
      `Long Term (10year) Average` = "green",
      `Short Term (1year) Average` = "blue"
    )) +
    geom_vline(xintercept = important_dates, 
               color = "red", linetype = "dashed", size = 1) +
    labs +
    scale_y_comma() +
    theme(panel.grid.major = element_line(color = "black",
                                          size = 0.5,
                                          linetype = 2))
  return(data)
}

# Function 2
make_country_list <- function(countries, max_display = 8) {
  display <- head(countries, max_display)
  remainder <- length(countries) - length(display)
  items <- lapply(display, tags$li)
  if (remainder > 0) {
    items <- c(items, list(tags$li(paste("... ",remainder, "more countries"))))
  }
  tags$ul(tagList(items))
}

ui <- fluidPage(
  theme = bs_theme(), 
  
  titlePanel(
    div(h2("The Trump Tariff Achievement Tracker", align = "center"))
  ),
  
  fluidRow(
    column(12,
           p("This is some introductory text about the app or its purpose.")
    )
  ),
  
  # Tabs
  tabsetPanel(
    tabPanel("Home", p("Content for Home tab")),
    tabPanel("About", p("Content for About tab")),
    tabPanel("Goal 1", # US Manufacturing
             p("Content for Goal 1"),
             fluidRow(
               column(6, plotOutput("Goal_One_Graph")),
               column(6, selectInput("man_data", "Select Manufacturing Data Component", 
                                     choices = names(Manufacturing_datasets)),
                      plotOutput("man_plot"))
             )),
    tabPanel("Goal 2", # Global Public Goods Burden Sharing
             p("Content for Goal 2"),
             fluidRow(
               column(6, plotOutput("Goal_Two_Graph")),
               column(6, plotOutput("military_spending"))
             )),
    tabPanel("Goal 3", # Global Adherence
             p("Content for Goal 3"),
             fluidRow(
               tags$head(
                 tags$style(HTML(".group-box {
                 border-radius: 15px;
                 padding: 15px;
                 color: white;
                 min-height: 150px;
                 margin-bottom: 20px;
                 }
                 .green-box {background-color: #32ab58;}
                 .yellow-box {background-color: #ffc107; color: black;}
                 .red-box {background-color: #e83f4f; }
    "))), 
               column(4,
                      h4("Full Formal Adherence"),
                      div(class = "group-box green-box", 
                          make_country_list(green_countries)
                 )),
               column(4,
                      h4("Partial Formal Adherence"),
                      div(class = "group-box yellow-box", 
                          make_country_list(yellow_countries)
                          
                      )),
               column(4,
                      h4("No Formal Adherence"),
                      div(class = "group-box red-box", 
                          make_country_list(red_countries)
                      ))
             ),
             fluidRow(
               plotOutput("adherence_piechart")
             )),
    tabPanel("Goal 4", 
             p("Content for Goal 4"),
             fluidRow(
               column(6, plotOutput("Goal_Four_Graph")),
               column(6, selectInput("econ_data", "Select Economic Data Component", 
                                     choices = names(Economy_datasets)),
                      plotOutput("econ_plot"))
             )),
    tabPanel("Goal 5", p("Content for Goal 5")),
    tabPanel("Goal 6", 
             p("Content for Goal 6"),
             fluidRow(
               column(6, plotOutput("Goal_Six_Graph")),
               column(6, selectInput("trade_data", "Select Balance of Payments Data", 
                                     choices = names(Trade_datasets)),
                      plotOutput("trade_plot"))
             )),
    tabPanel("Goal 7", 
             p("Content for Goal 7"),
             plotOutput("Goal_Seven_Graph")),
    tabPanel("Goal 8", 
             p("Content for Goal 8"),
             plotOutput("Goal_Eight_Graph")),
    tabPanel("History", p("Content for History tab"))
  ),
  
  br(),  # New line
  
  # Empty table
  fluidRow(
    tableOutput("summary"),
    fluidRow(br(), br()),
    fluidRow(
      column(2, align = "center", icon("home", "fa-2x")),
      column(2, align = "center", icon("info-circle", "fa-2x")),
      column(2, align = "center", icon("flag", "fa-2x")),
      column(2, align = "center", icon("check-circle", "fa-2x")),
      column(2, align = "center", icon("chart-bar", "fa-2x")),
      column(2, align = "center", icon("history", "fa-2x"))
    )
  )
)

server <- function(input, output, session) {
  bs_themer()
  # Create an empty 8x5 table with headers
  output$summary <- renderTable(summary_table, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # Goal One Renders
  
  output$Goal_One_Graph <- renderPlot({
    Total_Manufacturing %>% 
      filter(date > "2021-01-19") %>% 
      ggplot(aes(x = date)) +
      geom_point(aes(y = total_weighted_z), color = "black") +
      geom_line(aes(y = total_weighted_z), color = "black", size = 1) + #color = ", #A7C7E7"
      geom_vline(xintercept = important_dates, 
                 color = "red", linetype = "dashed", size = 1) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today(), ymin = -Inf, ymax = -1, fill = "red", alpha = 0.2) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today(), ymin = 1, ymax = Inf, fill = "green", alpha = 0.2) +
      labs(x = "Monthly Reporting", y = "Z score", title = "US Manufacturing Relative Status", caption = "Weighted Average") +
      theme(panel.grid.major = element_line(color = "black",
                                            size = 0.5,
                                            linetype = 2)) +
      coord_cartesian(ylim=c(-2,2))
    })
  
  output$man_plot <- renderPlot({
    selected_data <- Manufacturing_datasets[[input$man_data]]
    
    if(input$man_data == "ISM PMI"){
      selected_data <- selected_data %>% 
        filter(date > "2021-01-19") %>% 
        ggplot(aes(x = date)) +
        geom_point(aes(y = value)) +
        geom_point(aes(y = value_1yravg), color = "blue") +
        geom_line(aes(y = value, color = "Current Data"), size = 1) + #color = ", #A7C7E7"
        geom_line(aes(y = value_1yravg, color = "Short Term (1year) Average"), size = 1) +
        scale_color_manual(values = c(
          `Current Data` = "black",
          `Short Term (1year) Average` = "blue"
        )) +
        geom_vline(xintercept = important_dates, 
                   color = "red", linetype = "dashed", size = 1) +
        labs(x = "Monthly Reporting", y = "Purchasing Manager's Index", title = "ISM PMI", caption = "Index scores <50 indicates contraction and >50 is expansion", color = "Legend") + 
        scale_y_comma() +
        theme(panel.grid.major = element_line(color = "black",
                                              size = 0.5,
                                              linetype = 2))
      return(selected_data)
    } else if (input$man_data == "Manufacturing Employment"){
      plot_manufacturing_function(selected_data, 
                                  labs(x = "Monthly Reporting", y = "Thousands of Persons, Seasonally Adjusted", title = "Manufacturing Employment", caption = "All Employees", color = "Legend"), 
                                  important_dates)
    } else if (input$man_data == "Manufacturing New Orders"){
      plot_manufacturing_function(selected_data, 
                                  labs(x = "Monthly Reporting", y = "Millions of Dollars, Seasonally Adjusted", title = "Manufacturer New Orders", caption = "Total Manufacturing", color = "Legend"), 
                                  important_dates)
    } else if (input$man_data == "Manufacturing Value of Shipments"){
      plot_manufacturing_function(selected_data, 
                                  labs(x = "Monthly Reporting", y = "Millions of Dollars, Seasonally Adjusted", title = "Manufacturer Value of Shipments", caption = "Total Manufacturing", color = "Legend"), 
                                  important_dates)
    } else if (input$man_data == "Industrial Production"){
      plot_manufacturing_function(selected_data, 
                                  labs(x = "Monthly Reporting", y = "Units, Not Seasonally Adjusted", title = "Manufacturing Production", caption = "Index 2017 = 100", color = "Legend"), 
                                  important_dates)
    } else if (input$man_data == "Industrial Capacity"){
      plot_manufacturing_function(selected_data, 
                                  labs(x = "Monthly Reporting", y = "Units, Not Seasonally Adjusted", title = "Manufacturing Industrial Capacity", caption = "Index 2017 = 100", color = "Legend"), 
                                  important_dates)
    } else if(input$man_data == "Factory Investments") {
      plot_manufacturing_function(selected_data, 
                                  labs(x = "Quarterly Reporting", y = "Billions of Chained 2017 Dollars", title = "Real Private Fixed Investment into Nonresidential\nManufacturing Structures (i.e. Factories)", caption = "Seasonally Adjusted Annual Rate", color = "Legend"), 
                                  important_dates)
      }
  })
  
# Goal Two Renders
  
  output$Goal_Two_Graph <- renderPlot({
    plot_manufacturing_function(GPC_dollars,
                                labs(x = "Daily Reporting", y = "Index Jan 2006 = 100", title = "Nominal Broad U.S. Dollar Index", caption = "Not seasonally adjusted", color = "Legend"),
                                important_dates)
  })
  
  output$military_spending <- renderPlot({
    Military_spending %>% 
      ggplot(aes(x = Year, y = Share)) +
      geom_line()
  })

# Goal Three Renders
  
  output$adherence_piechart <- renderPlot({
    adherence %>% 
      group_by(Adherence) %>% 
      summarise(Percentage = sum(`GDP Percentage`)) %>% 
      ggplot(aes(x="", y = Percentage, fill = Adherence)) +
      geom_bar(stat="identity", width=1) +
      labs(title = "Percentage of Global GDP adhering to US trade policy") +
      scale_fill_manual(values = c(
        Full = "#32ab58",
        Partial = "#ffc107",
        No = "#e83f4f")) +
      coord_polar("y", start = 0) +
      theme_minimal()
  })

# Goal Four Renders
  
  output$Goal_Four_Graph <- renderPlot({
    Total_Economic %>% 
      filter(date > "2021-01-19") %>% 
      ggplot(aes(x = date)) +
      geom_point(aes(y = total_weighted_z), color = "black") +
      geom_line(aes(y = total_weighted_z), color = "black", size = 1) + #color = ", #A7C7E7"
      geom_vline(xintercept = important_dates, 
                 color = "red", linetype = "dashed", size = 1) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today(), ymin = -Inf, ymax = -1, fill = "red", alpha = 0.2) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today(), ymin = 1, ymax = Inf, fill = "green", alpha = 0.2) +
      labs(x = "Monthly Reporting", y = "Z score", title = "Avoiding Domestic Economic/Financial Turmoil", caption = "Weighted Average") +
      theme(panel.grid.major = element_line(color = "black",
                                            size = 0.5,
                                            linetype = 2)) +
      coord_cartesian(ylim=c(-2,2))
  })
  
  output$econ_plot <- renderPlot({
    selected_data_3 <- Economy_datasets[[input$econ_data]]
    
    if (input$econ_data == "Financial Stress Index"){
      plot_manufacturing_function(selected_data_3, 
                                  labs(x = "Weekly Reporting", y = "Index", title = "Financial Stress Index", caption = "Not Seasonally adjusted", color = "Legend"), 
                                  important_dates)
    } else if (input$econ_data ==  "Unemployment (U5)"){
      plot_manufacturing_function(selected_data_3, 
                                  labs(x = "Monthly Reporting", y = "Percent, Seasonally Adjusted", title = "Unemployment Rate", caption = "U5 Unemployment Rate", color = "Legend"), 
                                  important_dates)
    } else if (input$econ_data == "Real Personal Consumption Expenditures"){
      plot_manufacturing_function(selected_data_3, 
                                  labs(x = "Quarterly Reporting", y = "Chained 2017 Dollars", title = "Real Personal Consumption Expenditures per Capita", caption = "Seasonally adjusted annual rate", color = "Legend"), 
                                  important_dates)
    } else if (input$econ_data == "Median Usual Weekly Earnings"){
        plot_manufacturing_function(selected_data_3, 
                                    labs(x = "Quarterly Reporting", y = "1982-84 CPI Adjusted Dollars", title = "Median usual weekly real earnings", caption = "Seasonally adjusted", color = "Legend"), 
                                    important_dates)
    } else if (input$econ_data == "Consumer Sentiment"){
      plot_manufacturing_function(selected_data_3, 
                                  labs(x = "Monthly Reporting", y = "Units: Index 1966 (Q1) = 100", title = "Consumer Sentiment", caption = "Not Seasonally adjusted", color = "Legend"), 
                                  important_dates)
    } else if (input$econ_data == "Median CPI (Inflation)"){
      plot_manufacturing_function(selected_data_3, 
                                  labs(x = "Monthly Reporting", y = "Percent Change at Annual Rate", title = "Median Consumer Price Index (inflation)", caption = "Seasonally adjusted", color = "Legend"), 
                                  important_dates)
    } 

  })
  
  # Goal Six Renders
  
  output$Goal_Six_Graph <- renderPlot({
    Total_Trade %>% 
      filter(date > "2021-01-19") %>% 
      ggplot(aes(x = date)) +
      geom_point(aes(y = zscore_1yravg), color = "black") +
      geom_line(aes(y = zscore_1yravg), color = "black", size = 1) + #color = ", #A7C7E7"
      geom_vline(xintercept = important_dates, 
                 color = "red", linetype = "dashed", size = 1) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today(), ymin = -Inf, ymax = -1, fill = "red", alpha = 0.2) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today(), ymin = 1, ymax = Inf, fill = "green", alpha = 0.2) +
      labs(x = "Monthly Reporting", y = "Z score", title = "Balance of Trade", caption = "Trade Balance: Goods and Services, Balance of Payments Basis", color = "Legend") +
      theme(panel.grid.major = element_line(color = "black",
                                            size = 0.5,
                                            linetype = 2)) +
      coord_cartesian(ylim=c(-3,3))
  })
  
  output$trade_plot <- renderPlot({
    selected_data_2 <- Trade_datasets[[input$trade_data]]
    
    if (input$trade_data == "Balance of Payments"){
      selected_data_2 %>% 
        filter(date > "2020-12-31") %>% 
        ggplot(aes(x = date)) +
        geom_point(aes(y = value)) +
        geom_point(aes(y = value_10yravg), color = "green") +
        geom_line(aes(y = value, color = "Current Data"), size = 1) + 
        geom_line(aes(y = value_10yravg, color = "Long Term (10year) Average"), size = 1) +
        scale_color_manual(values = c(
          `Current Data` = "black",
          `Long Term (10year) Average` = "green"
        )) +
        geom_vline(xintercept = important_dates, 
                   color = "red", linetype = "dashed", size = 1) +
        labs(x = "Annual Reporting", y = "Percent of GDP in U.S. Dollars", title = "Balance of Payments", caption = "Not Seasonally adjusted", color = "Legend") +
        scale_y_comma() +
        theme(panel.grid.major = element_line(color = "black",
                                              size = 0.5,
                                              linetype = 2))
      } else if (input$trade_data ==  "Current Account"){
      plot_manufacturing_function(selected_data_2, 
                                  labs(x = "Quarterly Reporting", y = "Millions of Dollars", title = "Current Account", caption = "Seasonally Adjusted", color = "Legend"), 
                                  important_dates)
        }
    })
  
  # Goal Seven Renders
  
  output$Goal_Seven_Graph <- renderPlot({
    Debt_Servicing_GDP %>% 
      filter(date > "2021-01-19") %>% 
      ggplot(aes(x = date)) +
      geom_point(aes(y = zscore_1yravg), color = "black") +
      geom_line(aes(y = zscore_1yravg), color = "black", size = 1) + #color = ", #A7C7E7"
      geom_vline(xintercept = important_dates, 
                 color = "red", linetype = "dashed", size = 1) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today(), ymin = -Inf, ymax = -1, fill = "red", alpha = 0.2) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today(), ymin = 1, ymax = Inf, fill = "green", alpha = 0.2) +
      labs(x = "Quarterly Reporting", y = "Z score", title = "Interest Payments as a Percentage of Real GDP", caption = "Weighted Average") +
      theme(panel.grid.major = element_line(color = "black",
                                            size = 0.5,
                                            linetype = 2)) +
      coord_cartesian(ylim=c(-2,2))
  })
  
  # Goal Eight Renders
  output$Goal_Eight_Graph <- renderPlot({
    Total_Tax %>% 
      filter(record_date > "2021-01-19") %>% 
      ggplot(aes(x = record_date)) +
      geom_point(aes(y = value)) +
      geom_point(aes(y = value_5yravg), color = "green") +
      geom_point(aes(y = value_1yravg), color = "blue") +
      geom_line(aes(y = value, color = "Current Data"), size = 1) + #color = ", #A7C7E7"
      geom_line(aes(y = value_5yravg, color = "Medium Term (5year) Average"), size = 1) +
      geom_line(aes(y = value_1yravg, color = "Short Term (1year) Average"), size = 1) +
      scale_color_manual(values = c(
        `Current Data` = "black",
        `Medium Term (5year) Average` = "green",
        `Short Term (1year) Average` = "blue"
      )) +
      geom_vline(xintercept = important_dates, 
                 color = "red", linetype = "dashed", size = 1) +
      labs(x = "Monthly Reporting", y = "Percent of revenue collected from customs duties", title = "Tariff Revenue as Percentage of Total Revenue", color = "Legend") +
      scale_y_comma() +
      theme(panel.grid.major = element_line(color = "black",
                                            size = 0.5,
                                            linetype = 2))
  })
}

shinyApp(ui, server)

# Run the application 
shinyApp(ui = ui, server = server)
