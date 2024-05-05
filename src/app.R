library(shiny)
library(shinyWidgets)
library(readr)
library(dplyr)
library(plotly)
library(bslib)
library(shinydashboard)

covid <- read_csv("../data/preprocessed/covid.csv", col_types = cols(
  date = col_date(format = "%Y-%m-%d")
))

min_date <- min(covid$date)
max_date <- max(covid$date)

country_choices <- unique(covid$country)
country_choices <- as.character(country_choices)
country_choices <- country_choices[!is.na(country_choices)]

covid_summary <- read_csv('../data/preprocessed/covid_summary.csv')

metric2_radiobuttons <- radioButtons("xvar",
                                     "Metric",
                                     choices = c("Total cases" = "total_confirmed",
                                                 "Total deaths" = "total_deaths",
                                                 "Total tests" = "total_tests",
                                                 "Cases per 1M" = "total_cases_per_1m_population",
                                                 "Deaths per 1M" = "total_deaths_per_1m_population",
                                                 "Tests per 1M" = "total_tests_per_1m_population"),
                                     selected = "total_confirmed")

virtual <- virtualSelectInput(
  inputId = "country_select",
  label = "Countries",
  placeholder = "Select region(s)",
  choices = country_choices,
  showValueAsTags = TRUE,
  search = TRUE,
  multiple = TRUE
)

cases_deaths_radiobuttons <-  radioButtons("display_var",
                                           "Metric",
                                           choices = c("New cases" = "daily_new_cases",
                                                       "New deaths" = "daily_new_deaths"),
                                           selected = "daily_new_cases")

select_date <- airDatepickerInput(
                   inputId = "date_range",
                   label = "Date",
                   placeholder = "Select a date (range)",
                   range = TRUE,
                   clearButton = TRUE,
                   minDate = min_date,
                   maxDate = max_date
                 )

box_title_global <- box(title = "Filter Global View", status = "primary", solidHeader = TRUE,
                        cases_deaths_radiobuttons,
                        select_date,
                        p("*Daily data summed up for the selected period", style = "font-size: 12px;"),
                        width = 12)

box_summary_text <- box(title = "Summary", status = "primary", solidHeader = TRUE,
                        htmlOutput("summary_text"), 
                        p("*For the selected time range", style = "font-size: 12px;"),
                        width = 12) 

box_global_graph <- box(title = "Global View", status = "warning", solidHeader = TRUE,
                        plotlyOutput("plot", height = "370px"), width = 12)

box_filter_statistics <- box(title = "Filter Statistics", status = "primary", solidHeader = TRUE,
                             virtual, metric2_radiobuttons, 
                             p("*Data ranges from 2020-2-15 until 2022-05-14",  style = "font-size: 12px;"),
                             width = 12)

box_bar_chart <- box(title = "Statistics", status = "warning", solidHeader = TRUE,
                     plotlyOutput("bar_chart", height = "280px"), width = 12)

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Data Dashboard"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        #summary_text {
          font-size: 16px; /* Adjust font size as needed */
        }
      "))
    ),
    fluidRow(
      column(width = 3,
             box_title_global,
             box_summary_text),
      column(width = 9,
             box_global_graph)
    ),
    fluidRow(
      column(width = 3,
             box_filter_statistics),
      column(width = 9,
             box_bar_chart)
      
    )
  )
)

# Server logic
server <- function(input, output, session) {
  filtered_data <- reactive({
    if (is.null(input$date_range) || length(input$date_range) < 2) {
      min_date <- min(covid$date, na.rm = TRUE)
      max_date <- max(covid$date, na.rm = TRUE)
    } else {
      min_date <- as.Date(input$date_range[1])
      max_date <- as.Date(input$date_range[2])
    }
    
    covid %>%
      filter(date >= min_date & date <= max_date)
  })
  
  output$plot <- renderPlotly({
    legend_title <- if(input$display_var == "daily_new_cases") {
      "New Cases"
    } else if(input$display_var == "daily_new_deaths") {
      "New Deaths"
    } 
    data <- filtered_data() %>%
      group_by(CODE, country) %>%
      summarise(sum_new_cases = sum(as.numeric(!!sym(input$display_var)), 
                                    na.rm = TRUE), .groups = "drop") %>%
      mutate(
        tooltip = paste(
          country,
          sprintf("<br>%s: ", legend_title),
          case_when(
            sum_new_cases >= 1e6 ~ paste0(round(sum_new_cases / 1e6, 1), "M"),
            sum_new_cases >= 1e4 ~ paste0(round(sum_new_cases / 1e3, 1), "K"),
            TRUE ~ as.character(sum_new_cases)
          )
        )
      )
    
    
    plot_ly(data, type = 'choropleth',
            locations = ~CODE, z = ~sum_new_cases,
            text = ~tooltip, colorscale = 'YIOrRd',
            reversescale = TRUE,
            colorbar = list(title = legend_title),
            hoverinfo = 'text'
            
    ) %>%
      layout(
        geo = list(
          projection = list(type = 'natural earth'),
          scope = 'world',
          bgcolor = 'rgba(0,0,0,0)',  
          lataxis = list(fixedrange = TRUE),
          lonaxis = list(fixedrange = TRUE),
          showcoastlines = FALSE, 
          showland = TRUE,
          landcolor = 'rgb(217, 217, 217)' 
        )
      ) %>%
      config(displayModeBar = FALSE,
             scrollZoom = FALSE)
  })
  
  output$summary_text <- renderUI({
    data <- filtered_data() 
    sum_metric <- round(sum(as.numeric(data[[input$display_var]]), na.rm = TRUE), 2)
    formatted_sum_metric <- format(sum_metric, big.mark = ".", decimal.mark = ",", nsmall = 0)
    if (input$display_var == "daily_new_cases") {
      metric_name <- "Cases"
    } else {
      metric_name <- "Deaths"
    }
  
    HTML(paste(
      "Global Number of", metric_name, ":<br>",
      "<span style='font-size: 24px;'><b>", formatted_sum_metric, "</b></span>"
    ))
  })

  output$bar_chart <- renderPlotly({
    selected_countries <- input$country_select
    zero <- FALSE  
    

    if (length(selected_countries) == 0) {
      selected_countries <- country_choices
      zero <- TRUE  
    }
    
    y_column <- input$xvar
    
    data_bar <- covid_summary %>%
      filter(country %in% selected_countries) %>%
      arrange(desc(!!sym(y_column))) %>%
      select(country, all_of(y_column))
    
    data_bar$country <- factor(data_bar$country, levels = unique(data_bar$country))
    
    if (zero) {
      data_bar <- head(data_bar, n=10)
    }
    
    
    get_readable_name <- function(column_name) {
      switch(column_name,
             total_tests = "Total Tests",
             total_deaths = "Total Deaths",
             total_confirmed = "Total Cases",
             total_cases_per_1m_population = "Cases per 1M",
             total_deaths_per_1m_population = "Deaths per 1M",
             total_tests_per_1m_population = "Tests per 1M",
             column_name) 
    }
    readable_name <- get_readable_name(y_column)
    
    # Add tooltip formatting
    data_bar <- data_bar %>%
      mutate(
        tooltip = case_when(
          .data[[y_column]] >= 1e9 ~ paste(readable_name, ": ", format(round(.data[[y_column]] / 1e9, 1), nsmall = 1), "B"),
          .data[[y_column]] >= 1e6 ~ paste(readable_name, ": ", format(round(.data[[y_column]] / 1e6, 1), nsmall = 1), "M"),
          .data[[y_column]] >= 1e4 ~ paste(readable_name, ": ", format(round(.data[[y_column]] / 1e3, 1), nsmall = 1), "K"),
          TRUE ~ paste(readable_name, ": ", format(.data[[y_column]]))
        ),
        tooltip = paste(country, "<br>", tooltip)
      )
    
    
    p <- plot_ly(data = data_bar, 
                 x = ~country, 
                 y = as.formula(paste0("~`", y_column, "`")), 
                 type = 'bar',
                 marker = list(color = '#f9df8e'),
                 text = ~tooltip,  # Apply the custom tooltip
                 hoverinfo = 'text', # Display tooltip on hover
                 textposition = 'none'  # Hide text labels on the bars
    ) %>%
      config(displayModeBar = FALSE, scrollZoom = FALSE)
    p <- layout(p, 
                xaxis = list(title = FALSE), 
                yaxis = list(title = get_readable_name(y_column)))
    p
  })
  
}



# Run the application 
shinyApp(ui, server)