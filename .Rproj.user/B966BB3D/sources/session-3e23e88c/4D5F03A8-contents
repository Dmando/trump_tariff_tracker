# Loading Libraries

library(shiny)
library(shinyjs)
library(bslib)
library(tidyverse)
library(fredr)
library(hrbrthemes)

# Updating all real-time data to update the summary table
source("Summary_Sheet_Calc.R")

# Loading Summary Table Data

summary_table <- read_csv("Summary_Sheet.csv")
results <- read_csv("all_goal_data.csv")

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

adherence_countries <- read_csv("Data/TTT_Countries.csv")
green_countries <- adherence_countries %>% 
  group_by(Country) %>% 
  slice_tail(n = 1) %>% 
  filter(Adherence == "Full") %>% 
  arrange(desc(GDP))
green_countries <- green_countries$Country
yellow_countries <- adherence_countries %>% 
  group_by(Country) %>% 
  slice_tail(n = 1) %>% 
  filter(Adherence == "Partial") %>% 
  arrange(desc(GDP))
yellow_countries <- yellow_countries$Country
red_countries <- adherence_countries %>% 
  group_by(Country) %>% 
  slice_tail(n = 1) %>% 
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

# Downloading Currency Goal Five Data

Total_Currency <- read_csv("Data/goal_five_data.csv")
Currency_Reserves <- read_csv("Data/COFER.csv")
Currency_Debt <- read_csv("Data/debt_currency.csv")
Currency_Loans <- read_csv("Data/loans_currency.csv")
Currency_Payments <- read_csv("Data/IntlPayments_qtr.csv")
Currency_datasets <- list(
  "USD in Foreign Exchange Currency Reserves" = Currency_Reserves,
  "International Debt denominated in USD" = Currency_Debt,
  "International Loans denominated in USD" = Currency_Loans,
  "International Payments in USD" = Currency_Payments
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
    geom_point(aes(y = value_2yravg), color = "blue") +
    geom_line(aes(y = value, color = "Current Data"), size = 1) + 
    geom_line(aes(y = value_10yravg, color = "Long Term (10year) Average"), size = 1) +
    geom_line(aes(y = value_2yravg, color = "Short Term (2year) Average"), size = 1) +
    geom_text_repel(
      data = slice_tail(data, n = 1), aes(y = value, label = format(date, "%b %d, %y")), size = 3, nudge_x = -300) +
    scale_color_manual(values = c(
      `Current Data` = "black",
      `Long Term (10year) Average` = "green",
      `Short Term (2year) Average` = "blue"
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
make_country_list <- function(countries, max_display = 4) {
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
           p("This dashboard identifies eight major goals that the Trump administration is trying to achieve with its tariff policy and measures them in relation to how much better the US is in relation to the past year.")
    )
  ),
  
  # Tabs
  tabsetPanel(
    tabPanel("Home",
             fluidRow(br()),
             fluidRow(
                   column(2, align = "center", icon("home", "fa-2x")),
                   column(2, align = "center", icon("info-circle", "fa-2x")),
                   column(2, align = "center", icon("flag", "fa-2x")),
                   column(2, align = "center", icon("check-circle", "fa-2x")),
                   column(2, align = "center", icon("chart-bar", "fa-2x")),
                   column(2, align = "center", icon("history", "fa-2x"))),
             tableOutput("summary"),
             fluidRow(br()),
             fluidRow(
               p("Results Breakdown"),
               plotOutput("Results")),
             fluidRow(
               tags$head(
                 tags$style(HTML("
      .toggle-text {
        display: none;
        margin-top: 10px;
      }
      .toggle-button {
        cursor: pointer;
        font-weight: bold;
        color: #007bff;
        user-select: none;
      }
    "))),
               tags$div(
                 class = "toggle-button",
                 id = "toggle-button",
                 HTML("&#9654; More Info")  # ▶ More Info
               ),
               tags$div(
                 id = "info-text",
                 class = "toggle-text",
                 "This dashboard tries to quantitatively determine the efficacy of the Trump Administration's tariff policy based on commonly stated administration goals. These goals come from statements and related media made by President Trump, Treasury Secretary Bessent, and CEA Chairman Miran.
                 All goals utilize relevant, real-time parameters (including individual datapoints, composite indices, and media assessment) to judge whether progress outlook towards a goal is negative, stable, or positive.\nSix of eight goals are assessed using a three step process. 
                 First, an individual or weighted number, a score, is calculated on a monthly or quarterly basis (depending on data availability) is outputted after ingesting relevant data. Second, this score is compared on a normal distribution to the yearly average of the score. If the score is one standard deviation higher or lower, then it is assigned a positive or negative grade accordingly.
                 \nOne of eight goals (global adherence to new tariff regime) characterizes it's metrics by what percentage of global gdp adheres to the new US tariff regime by signing a partial (executive/handshake/informal) deal and/or a formal (legislatively ratified) deal. The threshold, 50%, is roughly equivalent to the size of the global economies which signed previous trade regime altering agreements such as the Plaza and Louvre Accords.
                 \nOne of eight goals (transforming the Internal Revenue Service into the External Revenue Service) sets its thresholds at levels by which one could feasily say or not say that a significant portion of the government spending is financed by external vs. internal revenue. For the methodologies, data, and academic sources used to create this dashboard, alongside suggestions for methods to improve the dashboard, please refer to the history tab."
               ),
               tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      const toggleBtn = document.getElementById('toggle-button');
      const infoText = document.getElementById('info-text');
      
      toggleBtn.addEventListener('click', function() {
        if (infoText.style.display === 'none' || infoText.style.display === '') {
          infoText.style.display = 'block';
          toggleBtn.innerHTML = '&#9660; Less Info';  // ▼
        } else {
          infoText.style.display = 'none';
          toggleBtn.innerHTML = '&#9654; More Info';  // ▶
        }
      });
    });
  "))),
             fluidRow(br(), br())),
    tabPanel("Goal 1", # US Manufacturing
             p("Goal one measures whether U.S. manufacturing is increasing or decreasing my combining several factors which when (after being appropriately weighted) combined measure the state of the manufacturing sector. Toggle more info for an in-depth explanation."),
             fluidRow(
               column(6, plotOutput("Goal_One_Graph")),
               column(6, selectInput("man_data", "Select Manufacturing Data Component", 
                                     choices = names(Manufacturing_datasets)),
                      plotOutput("man_plot"))),
             fluidRow(
               tags$head(
                 tags$style(HTML("
      .toggle-text {
        display: none;
        margin-top: 10px;
      }
      .toggle-button {
        cursor: pointer;
        font-weight: bold;
        color: #007bff;
        user-select: none;
      }
    "))), 
               tags$div(
                 class = "toggle-button",
                 id = "toggle-button2",
                 HTML("&#9654; More Info")  # ▶ More Info
),
               tags$div(
                 id = "info-text2",
                 class = "toggle-text",
                 "The U.S. manufacturing sector score is broken down into seven components: The US ISM manufacturing purchasing managers index (PMI), manufacturing employment, new orders by manufacturers, value of shipments by manufacturers, industrial production, industrial capacity, and real private investment in fixed manufacturing structures (i.e. factories).
                 ISM PMI data comes from dbnomics while the remaining datapoints are sourced from FRED. The six latter datapoints represent backwards looking data; how is the manufacturing faring so far? Each of these parameters are given a weight of 1/12 and together account for half of the final score. 
                 The Manufacturing ISM PMI parameter surveys how manufacturers think their business will do in the future. Thus, it is a forward looking paramter which measure how will the manufacturing sector fare. ISM PMI is weighted at 1/2 of the score. Together, the parameters measure the manufacturing sector based on how it has performed and how it will perform. 
                 Thus, the month to month score variation vs. yearly score average measures how the current and future health of the sector compared to the past year. If this final score is one standard deviation above/below the average, then the sector is faring positively/negatively."
               ),
               tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      const toggleBtn = document.getElementById('toggle-button2');
      const infoText = document.getElementById('info-text2');
      toggleBtn.addEventListener('click', function() {
        if (infoText.style.display === 'none' || infoText.style.display === '') {
          infoText.style.display = 'block';
          toggleBtn.innerHTML = '&#9660; Less Info';  // ▼
        } else {
          infoText.style.display = 'none';
          toggleBtn.innerHTML = '&#9654; More Info';  // ▶
        }
      });
    });
  "))),

fluidRow(br(), br())),

    tabPanel("Goal 2", # Global Public Goods Burden Sharing
             p("Goal two measures the level to which other nations are contributing to global public goods which the Trump administration has identified as important. Toggle more info for an in-depth explanation."),
             fluidRow(
               column(6, plotOutput("Goal_Two_Graph")),
               column(6, plotOutput("military_spending"))
             ),
             fluidRow(
               tags$head(
                 tags$style(HTML("
      .toggle-text {
        display: none;
        margin-top: 10px;
      }
      .toggle-button {
        cursor: pointer;
        font-weight: bold;
        color: #007bff;
        user-select: none;
      }"))),
               tags$div(
                 class = "toggle-button",
                 id = "toggle-button3",
                 HTML("&#9654; More Info")  # ▶ More Info
               ),
               tags$div(
                 id = "info-text3",
                 class = "toggle-text",
                 "The Trump administration has focused on getting foreign and allied states to pay their fair share (i.e. burden sharing) of two global public goods (defined by the IMF as goods 'whose benefits affect all citizens of the world'): Global currency markets and security. For the former, the Trump administration believes that the designation of the US Dollar as the global currency reserve has forcibly raised the value of the currency, making it more expensive for the US to export domestic goods & services. 
                 The value of the US dollar relative to the globe is measured here by the nominal broad U.S. dollar index, which is an index that measures the value of the U.S. dollar relative to other world currencies based on the amount of trade the US conducts with the country (i.e. the Chinese renminbi features more prominently in the index than the Chilean peso because the US trades much more with China than Chile). In the latter, the administration believes that America's security partners (i.e. NATO, Japan, South Korea)
                 have their security subsidized by the US and wants them to pay more for their own security. However, this goal does not factor into the final measurement of the goal yet because the data does not include 2025 yet and thus does not measure any developments made during Trump's 2nd term. 
                 Once the data appears, then this goal will be measured by combining the total percentage of GDP dedicated to military spending for all of the US's formal military allies. These allies include countries in NATO, ANZUS, Japan, South Korea, and the Phillippines.Then, the military spending and currency data will be weighted 50/50 (right now it's just the currency data) and measured using the standard deviation approach as explained in the home and goal one tabs.
                 Data for the nominal broad U.S. dollar index comes from FRED while military spending as a percentage of GDP comes from SIPRI."
               ),
               tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      const toggleBtn = document.getElementById('toggle-button3');
      const infoText = document.getElementById('info-text3');
      toggleBtn.addEventListener('click', function() {
        if (infoText.style.display === 'none' || infoText.style.display === '') {
          infoText.style.display = 'block';
          toggleBtn.innerHTML = '&#9660; Less Info';  // ▼
        } else {
          infoText.style.display = 'none';
          toggleBtn.innerHTML = '&#9654; More Info';  // ▶
        }
      });
    });"))),
             fluidRow(br(), br())),
    tabPanel("Goal 3", # Global Adherence
             p("Goal three measures the amount of countries as a percentage of Global GDP which have signed informal and/or formal trade deals. Toggle more info for an in-depth explanation."),
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
                 .red-box {background-color: #e83f4f; }"))), 
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
             ),
             fluidRow(
               tags$head(
                 tags$style(HTML("
      .toggle-text {
        display: none;
        margin-top: 10px;
      }
      .toggle-button {
        cursor: pointer;
        font-weight: bold;
        color: #007bff;
        user-select: none;
      }"))),
               tags$div(
                 class = "toggle-button",
                 id = "toggle-button4",
                 HTML("&#9654; More Info")  # ▶ More Info
               ),
               tags$div(
                 id = "info-text4",
                 class = "toggle-text",
                 "Previous US trade regimes created at Bretton Woods, the Plaza Hotel, etc., required global buy-in. The signatories of the Plaza and Louvre Accords of '85 and '87 constituted around 50% of global GDP. 
                 Thus, this goal is classified as 'positive' if more than or equal to 50% of global GDP formally signs a multi or bilateral trade agreement with the US and 'stable' if 50% of global GDP formally and informally signs an agreement. The data is updated semi-regularly based on news reports."
               ),
               tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      const toggleBtn = document.getElementById('toggle-button4');
      const infoText = document.getElementById('info-text4');
      toggleBtn.addEventListener('click', function() {
        if (infoText.style.display === 'none' || infoText.style.display === '') {
          infoText.style.display = 'block';
          toggleBtn.innerHTML = '&#9660; Less Info';  // ▼
        } else {
          infoText.style.display = 'none';
          toggleBtn.innerHTML = '&#9654; More Info';  // ▶
        }
      });
    });"))),
             fluidRow(br(), br())),
    tabPanel("Goal 4", 
             p("Goal four assesses whether the economy is in turmoil by measuring the health of the economy for the average american and the health of markets. Toggle more info for an in-depth explanation."),
             fluidRow(
               column(6, plotOutput("Goal_Four_Graph")),
               column(6, selectInput("econ_data", "Select Economic Data Component", 
                                     choices = names(Economy_datasets)),
                      plotOutput("econ_plot"))
             ),
             fluidRow(
               tags$head(
                 tags$style(HTML("
      .toggle-text {
        display: none;
        margin-top: 10px;
      }
      .toggle-button {
        cursor: pointer;
        font-weight: bold;
        color: #007bff;
        user-select: none;
      }"))),
               tags$div(
                 class = "toggle-button",
                 id = "toggle-button5",
                 HTML("&#9654; More Info")  # ▶ More Info
               ),
               tags$div(
                 id = "info-text5",
                 class = "toggle-text",
                 "The avoid domestic economic turmoil score is broken down into six components: The OFR Financial Stress Index, The U5 unemployment rate (unemployed + discouraged workers + people marginally attached to the labor force), real personal consumption expenditures per capita, median usual weekly real earnings, consumenr sentiment, and median consumer price index.
                 The Financial Stress Index measures the health of financial markets and is weighted at 1/2 of the score. The latter five parameters measure the strength of the economy from the perspective of the individual, average American and are weighted at 1/10. The parameters are then measured using the standard deviation approach described in the home and goal sections. All the datapoints are sourced from FRED.
                 "
               ),
               tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      const toggleBtn = document.getElementById('toggle-button5');
      const infoText = document.getElementById('info-text5');
      toggleBtn.addEventListener('click', function() {
        if (infoText.style.display === 'none' || infoText.style.display === '') {
          infoText.style.display = 'block';
          toggleBtn.innerHTML = '&#9660; Less Info';  // ▼
        } else {
          infoText.style.display = 'none';
          toggleBtn.innerHTML = '&#9654; More Info';  // ▶
        }
      });
    });"))),
             fluidRow(br(), br())),
    tabPanel("Goal 5", 
             p("Goal five measures whether the dollar remains the global reserve currency. 
               Toggle more info for an in-depth explanation."),
             fluidRow(
               column(6, plotOutput("Goal_Five_Graph")),
               column(6, selectInput("currency_data", "Select Currency Data Component", 
                                     choices = names(Currency_datasets)),
                      plotOutput("currency_plot"))
             ),
             fluidRow(br(), br())),
    tabPanel("Goal 6", 
             p("Goal six measures whether the state of the trade deficit. Toggle more info for an in-depth explanation."),
             fluidRow(
               column(6, plotOutput("Goal_Six_Graph")),
               column(6, selectInput("trade_data", "Select Balance of Payments Data", 
                                     choices = names(Trade_datasets)),
                      plotOutput("trade_plot"))),
               fluidRow(
                 tags$head(
                   tags$style(HTML("
      .toggle-text {
        display: none;
        margin-top: 10px;
      }
      .toggle-button {
        cursor: pointer;
        font-weight: bold;
        color: #007bff;
        user-select: none;
      }"))),
                 tags$div(
                   class = "toggle-button",
                   id = "toggle-button7",
                   HTML("&#9654; More Info")  # ▶ More Info
                 ),
                 tags$div(
                   id = "info-text7",
                   class = "toggle-text",
                   "Often a goal unto itself, goal six measures whether the trade deficit is standard deviation higher (negative) or lower (positive). Specifically, the data used is the balance of trade. However, also included in this tab is the current account and balance of payments; these are more expansive definitions of defining the trade deficit and they include more financial transactions while the trade balance is strictly focused on the export and import of goods and services. Data is sourced from FRED."
                 ),
                 tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      const toggleBtn = document.getElementById('toggle-button7');
      const infoText = document.getElementById('info-text7');
      toggleBtn.addEventListener('click', function() {
        if (infoText.style.display === 'none' || infoText.style.display === '') {
          infoText.style.display = 'block';
          toggleBtn.innerHTML = '&#9660; Less Info';  // ▼
        } else {
          infoText.style.display = 'none';
          toggleBtn.innerHTML = '&#9654; More Info';  // ▶
        }
      });
    });"))),
             fluidRow(br(), br())),
    tabPanel("Goal 7", 
             p("Goal seven measures if the burden of paying down the national debt will decrease. Toggle more info for an in depth explanation"),
             plotOutput("Goal_Seven_Graph"),
             fluidRow(
               tags$head(
                 tags$style(HTML("
      .toggle-text {
        display: none;
        margin-top: 10px;
      }
      .toggle-button {
        cursor: pointer;
        font-weight: bold;
        color: #007bff;
        user-select: none;
      }"))),
               tags$div(
                 class = "toggle-button",
                 id = "toggle-button8",
                 HTML("&#9654; More Info")  # ▶ More Info
               ),
               tags$div(
                 id = "info-text8",
                 class = "toggle-text",
                 "Historically, during periods of uncertainty, investors buy treasuries causing bond yields to go down. This in turn allows the US to auction bonds at lower rates, relatively lowering the cost of interest payments. This section measures the financial impact of the potential uncertainty effects of a new tariff regime (and even forced purchase/selling of treasuries by the US) by measuring the percentage of total US expenditures dedicated to interest payments utilizing the standard deviation approach described in the Home and Goal One sections.
                 Data is sourced from FRED."
               ),
               tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      const toggleBtn = document.getElementById('toggle-button8');
      const infoText = document.getElementById('info-text8');
      toggleBtn.addEventListener('click', function() {
        if (infoText.style.display === 'none' || infoText.style.display === '') {
          infoText.style.display = 'block';
          toggleBtn.innerHTML = '&#9660; Less Info';  // ▼
        } else {
          infoText.style.display = 'none';
          toggleBtn.innerHTML = '&#9654; More Info';  // ▶
        }
      });
    });
  "))),
             fluidRow(br(), br())),
    tabPanel("Goal 8", 
             p("Goal eight measures whether enough government revenue is being raised through tariffs to replace income taxes with tariffs. Toggle more info for a more in-depth explanation."),
             plotOutput("Goal_Eight_Graph"),
             fluidRow(
               tags$head(
                 tags$style(HTML("
      .toggle-text {
        display: none;
        margin-top: 10px;
      }
      .toggle-button {
        cursor: pointer;
        font-weight: bold;
        color: #007bff;
        user-select: none;
      }
    "))),
               tags$div(
                 class = "toggle-button",
                 id = "toggle-button9",
                 HTML("&#9654; More Info")  # ▶ More Info
               ),
               tags$div(
                 id = "info-text9",
                 class = "toggle-text",
                 "Government revenue is raised through a variety of methods, one of which is taxation on individuals and corporations through the internal revenue service (IRS). One of the Trump administration's goals is to replace the IRS with the ERS, the external revenue service (i.e. tariffs). This goal is asssessed by measuring whether a certain threshold of government revenue comes from tariffs. If it is below 10%, then it is negative and if it is above 50% then compliance towards the goal is 'positive.' Data is sourced from the US Treasury Fiscal Data API."
               ),
               tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      const toggleBtn = document.getElementById('toggle-button9');
      const infoText = document.getElementById('info-text9');
      toggleBtn.addEventListener('click', function() {
        if (infoText.style.display === 'none' || infoText.style.display === '') {
          infoText.style.display = 'block';
          toggleBtn.innerHTML = '&#9660; Less Info';  // ▼
        } else {
          infoText.style.display = 'none';
          toggleBtn.innerHTML = '&#9654; More Info';  // ▶
        }
      });
    });
  "))),
             fluidRow(br(), br())),
    tabPanel("History", p("Content for History tab"))
  )
)

server <- function(input, output, session) {
  bs_themer()
  
  # Home Renders
  
  output$summary <- renderTable(summary_table,
                                align = "c",
                                striped = TRUE, 
                                bordered = TRUE, 
                                hover = TRUE)
  
  observeEvent(input$toggle_about, {
    toggle("about_text")
  })
  
  output$Results <- renderPlot({
    results %>% 
      filter(date > "2021-01-19") %>% 
      arrange(date) %>% 
      ggplot(aes(x = date)) +
      geom_point(aes(y = goal_score), color = "black") +
      geom_line(aes(y = goal_score), color = "black", size = 1) + 
      geom_vline(xintercept = important_dates, 
                 color = "red", linetype = "dashed", size = 1) +
      geom_text_repel(
        data = slice_tail(results, n = 1), aes(y = goal_score, label = format(date, "%b %d, %y")), size = 3) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today() + months(3), ymin = -Inf, ymax = 0.49, fill = "red", alpha = 0.2) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today() + months(3), ymin = 0.57, ymax = Inf, fill = "green", alpha = 0.2) +
      labs(x = "Monthly Reporting", y = "Percentage", title = "Tariff Scoring Timeseries") +
      theme(panel.grid.major = element_line(color = "black",
                                            size = 0.5,
                                            linetype = 2)) +
      coord_cartesian(ylim=c(0, 1.5))
  })
  
  # Goal One Renders
  
  output$Goal_One_Graph <- renderPlot({
    Total_Manufacturing %>% 
      filter(date > "2021-01-19") %>% 
      arrange(date) %>% 
      ggplot(aes(x = date)) +
      geom_point(aes(y = total_weighted_z), color = "black") +
      geom_line(aes(y = total_weighted_z), color = "black", size = 1) + #color = ", #A7C7E7"
      geom_vline(xintercept = important_dates, 
                 color = "red", linetype = "dashed", size = 1) +
      geom_text_repel(
        data = slice_tail(Total_Manufacturing, n = 1), aes(y = total_weighted_z, label = format(date, "%b %d, %y")), size = 3) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today() + months(3), ymin = -Inf, ymax = -1, fill = "red", alpha = 0.2) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today() + months(3), ymin = 1, ymax = Inf, fill = "green", alpha = 0.2) +
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
        geom_point(aes(y = value_2yravg), color = "blue") +
        geom_line(aes(y = value, color = "Current Data"), size = 1) + #color = ", #A7C7E7"
        geom_line(aes(y = value_2yravg, color = "Short Term (2year) Average"), size = 1) +
        geom_text_repel(
          data = slice_tail(selected_data, n = 1), aes(y = value, label = format(date, "%b %d, %y")), size = 3, nudge_x = -300) +
        scale_color_manual(values = c(
          `Current Data` = "black",
          `Short Term (2year) Average` = "blue"
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

    GPC_dollars %>% 
      filter(date > "2021-01-19") %>% 
      ggplot(aes(x = date)) +
      geom_point(aes(y = zscore_2yravg), color = "black") +
      geom_line(aes(y = zscore_2yravg), color = "black", size = 1) + 
      geom_vline(xintercept = important_dates, 
                 color = "red", linetype = "dashed", size = 1) +
      geom_text_repel(
        data = slice_tail(GPC_dollars, n = 1), aes(y = zscore_2yravg, label = format(date, "%b %d, %y")), size = 3, nudge_x = -300) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today() + months(3), ymin = -Inf, ymax = -1, fill = "red", alpha = 0.2) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today() + months(3), ymin = 1, ymax = Inf, fill = "green", alpha = 0.2) +
      labs(x = "Monthly Reporting", y = "Z score", title = "Nominal Broad U.S. Dollar Index", caption = "Weighted Average") +
      theme(panel.grid.major = element_line(color = "black",
                                            size = 0.5,
                                            linetype = 2)) +
      coord_cartesian(ylim=c(-2,2))
  })
  
  output$military_spending <- renderPlot({
    Military_spending %>% 
      ggplot(aes(x = Year, y = Share)) +
      geom_line()
  })

# Goal Three Renders
  
  output$adherence_piechart <- renderPlot({
    adherence_countries %>% 
      group_by(Country) %>% 
      slice_tail(n = 1) %>% 
      ungroup() %>% 
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
      geom_text_repel(
        data = slice_tail(Total_Economic, n = 1), aes(y = total_weighted_z, label = format(date, "%b %d, %y")), size = 3, nudge_x = -300) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today() + months(3), ymin = -Inf, ymax = -1, fill = "red", alpha = 0.2) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today() + months(3), ymin = 1, ymax = Inf, fill = "green", alpha = 0.2) +
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
  
  # Goal Five Renders
  
  output$Goal_Five_Graph <- renderPlot({
    Total_Currency %>% 
      filter(date > "2021-01-19") %>% 
      ggplot(aes(x = date)) +
      geom_point(aes(y = total_weighted_z), color = "black") +
      geom_line(aes(y = total_weighted_z), color = "black", size = 1) + 
      geom_vline(xintercept = important_dates, 
                 color = "red", linetype = "dashed", size = 1) +
      geom_text_repel(
        data = slice_tail(Total_Currency, n = 1), aes(y = total_weighted_z, label = format(date, "%b %d, %y")), size = 3, nudge_x = -300) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today() + months(3), ymin = -Inf, ymax = -1, fill = "red", alpha = 0.2) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today() + months(3), ymin = 1, ymax = Inf, fill = "green", alpha = 0.2) +
      labs(x = "Quarterly Reporting", y = "Z score", title = "Maintain Global Dollar Supremacy", caption = "Weighted Average") +
      theme(panel.grid.major = element_line(color = "black",
                                            size = 0.5,
                                            linetype = 2)) +
      coord_cartesian(ylim=c(-2,2))
  })
  
  output$currency_plot <- renderPlot({
    selected_data_4 <- Currency_datasets[[input$currency_data]]
    
    if (input$currency_data == "USD in Foreign Exchange Currency Reserves"){
      plot_manufacturing_function(selected_data_4, 
                                  labs(x = "Quarterly Reporting", y = "Percentage", title = "Official Foreign Exchange Reserves by Currency", color = "Legend"), 
                                  important_dates)
    } else if (input$currency_data ==  "International Debt denominated in USD"){
      plot_manufacturing_function(selected_data_4, 
                                  labs(x = "Quarterly Reporting", y = "US dollar (Millions)", title = "Debt Securities Issuance by Currency", color = "Legend"),  
                                  important_dates)
    } else if (input$currency_data == "International Loans denominated in USD"){
      plot_manufacturing_function(selected_data_4, 
                                  labs(x = "Quarterly Reporting", y = "Percentage", title = "Cross-border total liabilities of banks", color = "Legend") , 
                                  important_dates)
    } else if (input$currency_data == "International Payments in USD"){
      plot_manufacturing_function(selected_data_4, 
                                  labs(x = "Monthly Reporting", y = "Percentage", title = "International Payments denominated in USD", color = "Legend"), 
                                  important_dates)
    } 
    
  })
  
  
  
  # Goal Six Renders
  
  output$Goal_Six_Graph <- renderPlot({
    Total_Trade %>% 
      filter(date > "2021-01-19") %>% 
      ggplot(aes(x = date)) +
      geom_point(aes(y = zscore_2yravg), color = "black") +
      geom_line(aes(y = zscore_2yravg), color = "black", size = 1) + 
      geom_vline(xintercept = important_dates, 
                 color = "red", linetype = "dashed", size = 1) +
      
      geom_text_repel(
        data = slice_tail(Total_Trade, n = 1), aes(y = zscore_2yravg, label = format(date, "%b %d, %y")), size = 3, nudge_x = -300) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today() + months(3), ymin = -Inf, ymax = -1, fill = "red", alpha = 0.2) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today() + months(3), ymin = 1, ymax = Inf, fill = "green", alpha = 0.2) +
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
        geom_text_repel(
          data = slice_tail(selected_data_2, n = 1), aes(y = value, label = format(date, "%b %d, %y")), size = 3, nudge_x = -300) +
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
      geom_point(aes(y = zscore_2yravg), color = "black") +
      geom_line(aes(y = zscore_2yravg), color = "black", size = 1) + 
      geom_vline(xintercept = important_dates, 
                 color = "red", linetype = "dashed", size = 1) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today() + months(3), ymin = -Inf, ymax = -1, fill = "red", alpha = 0.2) +
      annotate("rect", xmin = as_date("2021-01-01"), xmax = today() + months(3), ymin = 1, ymax = Inf, fill = "green", alpha = 0.2) +
      geom_text_repel(
        data = slice_tail(Debt_Servicing_GDP, n = 1), aes(y = zscore_2yravg, label = format(date, "%b %d, %y")), size = 3, nudge_x = -300) +
      labs(x = "Quarterly Reporting", y = "Z score", title = "Interest Payments as a Percentage of Real GDP", caption = "Weighted Average") +
      theme(panel.grid.major = element_line(color = "black",
                                            size = 0.5,
                                            linetype = 2)) +
      coord_cartesian(ylim=c(-2,2))
  })
  
  # Goal Eight Renders
  output$Goal_Eight_Graph <- renderPlot({
    Total_Tax %>% 
      filter(date > "2021-01-19") %>% 
      ggplot(aes(x = date)) +
      geom_point(aes(y = value)) +
      geom_point(aes(y = value_2yravg), color = "blue") +
      geom_line(aes(y = value, color = "Current Data"), size = 1) + 
      geom_line(aes(y = value_2yravg, color = "Short Term (2year) Average"), size = 1) +
      geom_text_repel(
        data = slice_tail(Total_Tax, n = 1), aes(y = value, label = format(date, "%b %d, %y")), size = 3, nudge_x = -300) +
      scale_color_manual(values = c(
        `Current Data` = "black",
        `Short Term (2year) Average` = "blue"
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
