library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)

df <- read_csv("../data/raw/Energy_consumption_by_fuel_EJ.csv")

Layout
ui <- page_fluid( 
  theme = bs_theme(),
  
  h1("Gloabl Energy Consumption Dashboard"),
  
  fluidRow(
    # Column for Filters 
    column(
      width = 3, 
      selectInput("country", 
                  "Select Country", 
                  choices = unique(df$Country), 
                  selected = "Canada"),
      
      sliderInput("year_range", 
                  "Select Year Range", 
                  min(df$Year, na.rm = TRUE),
                  max(df$Year, na.rm = TRUE), 
                  value = c(min(df$Year), max(df$Year)), 
                  sep = "",
                  step = 1),
      
      selectInput("fuel_type", 
                  "Select Fuel Type", 
                  choices = unique(df$`Fuel type`), 
                  selected = "Coal")
      ),
    
    # Column for Graphs
    column(
      width = 9, 
      fluidRow(div(plotOutput("line_plot"))),
      fluidRow(div(plotOutput("bar_plot")))
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df |>
      filter(Country == input$country,
             Year >= input$year_range[1],
             Year <= input$year_range[2],
             `Fuel type` == input$fuel_type)
  })
  
  # Line plot (graph #1)
  output$line_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Year, y = `Energy Consumption EJ`)) +
      geom_line(color = "blue") +
      labs(x = "Year", y = "Exajoules") +
      ggtitle("Energy Consumption Over Time") +
      theme(
        axis.title = element_text(size = 16, face = "bold"),  
        axis.text = element_text(size = 14, face = "bold"),   
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
      )
  })
  
  # Bar Chart (graph #2)
  output$bar_plot <- renderPlot({
    df |>
      filter(Country == input$country, Year == max(input$year_range)) |>
      ggplot(aes(x = `Fuel type`, y = `Energy Consumption EJ`, fill = `Fuel type`)) +
      geom_bar(stat = "identity") +
      labs(x = "Fuel Type", 
           y = "Exajoules") +
      ggtitle("Energy Consumption by Fuel Type") +
      theme(
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 14, face = "bold"),  
        legend.text = element_text(size = 12)  
      )
  })
}

shinyApp(ui, server)

