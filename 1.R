# Load required libraries
library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)
library(ggplot2)
library(leaflet)
library(plotly)
library(DT)
library(shinyjs)
library(scales)
library(RColorBrewer)

# Generate dummy data
set.seed(123)

# Poverty data
poverty_data <- data.frame(
  Province = c("Kigali", "Southern", "Western", "Northern", "Eastern"),
  Poverty_Rate = c(12.3, 45.6, 38.2, 42.1, 39.8),
  Extreme_Poverty = c(3.2, 18.4, 15.6, 17.2, 16.1),
  Gini_Coefficient = c(0.42, 0.38, 0.41, 0.39, 0.40)
)

# Employment data
employment_data <- data.frame(
  Sector = c("Agriculture", "Industry", "Services", "Public Sector"),
  Employment_Rate = c(68.5, 12.3, 15.8, 3.4),
  Male = c(65.2, 15.1, 16.2, 3.5),
  Female = c(71.8, 9.5, 15.4, 3.3)
)

# Education data
education_data <- data.frame(
  Level = c("Primary", "Secondary", "Tertiary", "No Education"),
  Urban = c(85.6, 42.3, 18.7, 8.2),
  Rural = c(78.9, 28.4, 6.3, 18.5),
  Male = c(82.1, 36.8, 13.9, 12.4),
  Female = c(81.8, 33.2, 10.8, 14.8)
)

# Health data
health_data <- data.frame(
  Indicator = c("Health Insurance Coverage", "Access to Healthcare", 
                "Child Mortality (per 1000)", "Maternal Mortality (per 100,000)"),
  Value = c(87.5, 78.3, 32, 248),
  Urban = c(92.1, 85.6, 28, 210),
  Rural = c(85.8, 75.2, 34, 265)
)

# Housing data
housing_data <- data.frame(
  Type = c("Electricity", "Clean Water", "Improved Sanitation", "Internet Access"),
  Urban = c(96.5, 89.2, 87.4, 65.3),
  Rural = c(45.8, 72.1, 68.9, 18.7),
  National = c(58.9, 76.8, 74.2, 32.4)
)

# Agriculture data
agriculture_data <- data.frame(
  Crop = c("Beans", "Maize", "Sweet Potato", "Irish Potato", "Rice", "Coffee"),
  Production_MT = c(485000, 520000, 890000, 1200000, 95000, 28000),
  Households_Pct = c(78.5, 65.3, 82.1, 45.6, 12.8, 35.2)
)

# Demographics data
demographics_data <- data.frame(
  Age_Group = c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  Male = c(20.1, 12.8, 11.5, 9.2, 7.8, 5.4, 4.2),
  Female = c(19.8, 13.2, 12.1, 9.8, 8.1, 5.8, 4.8),
  Population_Pct = c(39.9, 26.0, 23.6, 19.0, 15.9, 11.2, 9.0)
)

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$title("EICV7 Dashboard - NISR"),
    tags$style(HTML("
      body {
        font-family: 'Roboto', sans-serif;
        background-color: #f9f9f9;
        margin: 0;
        padding: 0;
        color: #0047AB;
      }
      .header {
        background: linear-gradient(135deg, #003b71, #2176ae);
        color: white;
        padding: 2rem;
        text-align: center;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      }
      .nav-tabs {
        background-color: #003b71;
        border: none;
      }
      .nav-tabs > li > a {
        color: white;
        background-color: #003b71;
        border: none;
        border-radius: 0;
      }
      .nav-tabs > li.active > a {
        background-color: #2176ae;
        color: white;
        border: none;
      }
      .nav-tabs > li > a:hover {
        background-color: #2176ae;
        border: none;
      }
      .content-panel {
        background-color: white;
        padding: 2rem;
        margin: 1rem 0;
        border-radius: 10px;
        box-shadow: 0 4px 10px rgba(0, 0, 0, 0.05);
      }
      .metric-box {
        background: linear-gradient(135deg, #e3f2fd, #bbdefb);
        padding: 1rem;
        border-radius: 8px;
        text-align: center;
        margin: 0.5rem;
      }
      .metric-value {
        font-size: 2rem;
        font-weight: bold;
        color: #0047AB;
      }
    "))
  ),
  
  div(class = "header",
      h1("Rwanda Household Living Conditions Survey (EICV7)"),
      p("National Institute of Statistics of Rwanda (NISR) - Interactive Dashboard")
  ),
  
  navbarPage("",
             tabPanel("Poverty",
                      div(class = "content-panel",
                          h2("Poverty and Inequality Indicators"),
                          fluidRow(
                            column(4,
                                   div(class = "metric-box",
                                       h4("National Poverty Rate"),
                                       div(class = "metric-value", "35.4%")
                                   )
                            ),
                            column(4,
                                   div(class = "metric-box",
                                       h4("Extreme Poverty"),
                                       div(class = "metric-value", "14.1%")
                                   )
                            ),
                            column(4,
                                   div(class = "metric-box",
                                       h4("Gini Coefficient"),
                                       div(class = "metric-value", "0.40")
                                   )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(6, plotlyOutput("poverty_plot")),
                            column(6, DT::dataTableOutput("poverty_table"))
                          )
                      )
             ),
             
             tabPanel("Employment",
                      div(class = "content-panel",
                          h2("Employment and Labour Market"),
                          fluidRow(
                            column(3,
                                   div(class = "metric-box",
                                       h4("Employment Rate"),
                                       div(class = "metric-value", "82.3%")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Agriculture Share"),
                                       div(class = "metric-value", "68.5%")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Youth Employment"),
                                       div(class = "metric-value", "74.8%")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Gender Gap"),
                                       div(class = "metric-value", "3.2%")
                                   )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(6, plotlyOutput("employment_plot")),
                            column(6, plotlyOutput("employment_gender_plot"))
                          )
                      )
             ),
             
             tabPanel("Education",
                      div(class = "content-panel",
                          h2("Education and Literacy Rates"),
                          fluidRow(
                            column(3,
                                   div(class = "metric-box",
                                       h4("Literacy Rate"),
                                       div(class = "metric-value", "82.0%")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Primary Completion"),
                                       div(class = "metric-value", "81.9%")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Secondary Enrollment"),
                                       div(class = "metric-value", "35.4%")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Tertiary Access"),
                                       div(class = "metric-value", "12.4%")
                                   )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(6, plotlyOutput("education_plot")),
                            column(6, DT::dataTableOutput("education_table"))
                          )
                      )
             ),
             
             tabPanel("Health",
                      div(class = "content-panel",
                          h2("Health and Insurance Coverage"),
                          fluidRow(
                            column(3,
                                   div(class = "metric-box",
                                       h4("Insurance Coverage"),
                                       div(class = "metric-value", "87.5%")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Healthcare Access"),
                                       div(class = "metric-value", "78.3%")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Child Mortality"),
                                       div(class = "metric-value", "32/1000")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Maternal Mortality"),
                                       div(class = "metric-value", "248/100k")
                                   )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(6, plotlyOutput("health_plot")),
                            column(6, DT::dataTableOutput("health_table"))
                          )
                      )
             ),
             
             tabPanel("Housing",
                      div(class = "content-panel",
                          h2("Housing and Basic Utilities"),
                          fluidRow(
                            column(3,
                                   div(class = "metric-box",
                                       h4("Electricity Access"),
                                       div(class = "metric-value", "58.9%")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Clean Water"),
                                       div(class = "metric-value", "76.8%")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Sanitation"),
                                       div(class = "metric-value", "74.2%")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Internet Access"),
                                       div(class = "metric-value", "32.4%")
                                   )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(6, plotlyOutput("housing_plot")),
                            column(6, leafletOutput("housing_map"))
                          )
                      )
             ),
             
             tabPanel("Agriculture",
                      div(class = "content-panel",
                          h2("Agricultural Production and Activities"),
                          fluidRow(
                            column(3,
                                   div(class = "metric-box",
                                       h4("Agric Households"),
                                       div(class = "metric-value", "72.4%")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Land Ownership"),
                                       div(class = "metric-value", "68.9%")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Crop Diversity"),
                                       div(class = "metric-value", "4.2")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Livestock Own"),
                                       div(class = "metric-value", "45.6%")
                                   )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(6, plotlyOutput("agriculture_plot")),
                            column(6, DT::dataTableOutput("agriculture_table"))
                          )
                      )
             ),
             
             tabPanel("Demographics",
                      div(class = "content-panel",
                          h2("Demographics and Household Composition"),
                          fluidRow(
                            column(3,
                                   div(class = "metric-box",
                                       h4("Population"),
                                       div(class = "metric-value", "13.2M")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Avg Household Size"),
                                       div(class = "metric-value", "4.2")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Urban Population"),
                                       div(class = "metric-value", "17.8%")
                                   )
                            ),
                            column(3,
                                   div(class = "metric-box",
                                       h4("Dependency Ratio"),
                                       div(class = "metric-value", "76.2%")
                                   )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(6, plotlyOutput("demographics_plot")),
                            column(6, plotlyOutput("age_pyramid"))
                          )
                      )
             )
  )
)





# Server
server <- function(input, output, session) {
  
  # Poverty plots
  output$poverty_plot <- renderPlotly({
    p <- ggplot(poverty_data, aes(x = Province, y = Poverty_Rate, fill = Province)) +
      geom_col() +
      scale_fill_brewer(palette = "Blues") +
      labs(title = "Poverty Rate by Province", y = "Poverty Rate (%)", x = "") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$poverty_table <- DT::renderDataTable({
    DT::datatable(poverty_data, options = list(pageLength = 10, dom = 't'))
  })
  
  # Employment plots
  output$employment_plot <- renderPlotly({
    p <- ggplot(employment_data, aes(x = Sector, y = Employment_Rate, fill = Sector)) +
      geom_col() +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "Employment by Sector", y = "Employment Rate (%)", x = "") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45))
    ggplotly(p)
  })
  
  output$employment_gender_plot <- renderPlotly({
    emp_gender <- employment_data %>%
      select(Sector, Male, Female) %>%
      tidyr::pivot_longer(cols = c(Male, Female), names_to = "Gender", values_to = "Rate")
    
    p <- ggplot(emp_gender, aes(x = Sector, y = Rate, fill = Gender)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("#2176ae", "#87ceeb")) +
      labs(title = "Employment by Gender", y = "Rate (%)", x = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45))
    ggplotly(p)
  })
  
  # Education plots
  output$education_plot <- renderPlotly({
    edu_long <- education_data %>%
      select(Level, Urban, Rural) %>%
      tidyr::pivot_longer(cols = c(Urban, Rural), names_to = "Area", values_to = "Rate")
    
    p <- ggplot(edu_long, aes(x = Level, y = Rate, fill = Area)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("#2176ae", "#87ceeb")) +
      labs(title = "Education Levels by Area", y = "Rate (%)", x = "") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$education_table <- DT::renderDataTable({
    DT::datatable(education_data, options = list(pageLength = 10, dom = 't'))
  })
  
  # Health plots
  output$health_plot <- renderPlotly({
    health_subset <- health_data[1:2, ]
    p <- ggplot(health_subset, aes(x = Indicator, y = Value, fill = Indicator)) +
      geom_col() +
      scale_fill_brewer(palette = "Greens") +
      labs(title = "Health Coverage Indicators", y = "Percentage", x = "") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45))
    ggplotly(p)
  })
  
  output$health_table <- DT::renderDataTable({
    DT::datatable(health_data, options = list(pageLength = 10, dom = 't'))
  })
  
  # Housing plots
  output$housing_plot <- renderPlotly({
    housing_long <- housing_data %>%
      select(Type, Urban, Rural) %>%
      tidyr::pivot_longer(cols = c(Urban, Rural), names_to = "Area", values_to = "Access")
    
    p <- ggplot(housing_long, aes(x = Type, y = Access, fill = Area)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("#2176ae", "#87ceeb")) +
      labs(title = "Utilities Access by Area", y = "Access Rate (%)", x = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45))
    ggplotly(p)
  })
  
  output$housing_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 30.0619, lat = -1.9403, zoom = 8) %>%
      addMarkers(lng = 30.0619, lat = -1.9403, popup = "Kigali - Urban Center")
  })
  
  # Agriculture plots
  output$agriculture_plot <- renderPlotly({
    p <- ggplot(agriculture_data, aes(x = reorder(Crop, Production_MT), y = Production_MT/1000, fill = Crop)) +
      geom_col() +
      scale_fill_brewer(palette = "YlOrRd") +
      labs(title = "Crop Production (1000s MT)", y = "Production (1000s MT)", x = "") +
      theme_minimal() +
      theme(legend.position = "none") +
      coord_flip()
    ggplotly(p)
  })
  
  output$agriculture_table <- DT::renderDataTable({
    DT::datatable(agriculture_data, options = list(pageLength = 10, dom = 't'))
  })
  
  # Demographics plots
  output$demographics_plot <- renderPlotly({
    p <- ggplot(demographics_data, aes(x = Age_Group, y = Population_Pct, fill = Age_Group)) +
      geom_col() +
      scale_fill_brewer(palette = "Spectral") +
      labs(title = "Population by Age Group", y = "Percentage", x = "Age Group") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$age_pyramid <- renderPlotly({
    demo_pyramid <- demographics_data %>%
      select(Age_Group, Male, Female) %>%
      tidyr::pivot_longer(cols = c(Male, Female), names_to = "Gender", values_to = "Percentage") %>%
      mutate(Percentage = ifelse(Gender == "Male", -Percentage, Percentage))
    
    p <- ggplot(demo_pyramid, aes(x = Age_Group, y = Percentage, fill = Gender)) +
      geom_col() +
      scale_fill_manual(values = c("#ff6b6b", "#4ecdc4")) +
      labs(title = "Population Pyramid", y = "Percentage", x = "Age Groups") +
      theme_minimal() +
      coord_flip()
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)