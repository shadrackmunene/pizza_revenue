
# Shiny
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(plotly)

# Get data----

pizza_raw_df <- readRDS("00_data/pizza_category_tbl.rds")

pizza_order_tbl <- reactive({
    pizza_raw_df
})

# UI----
ui <- navbarPage(
    title = "Pizza Analysis",
    inverse = FALSE,
    collapsible = TRUE,
    theme = shinytheme("darkly"),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    fluidRow(
        title = "Dashboard",
        
        # 1.0 Header----
        div(
            class = "jumbotron",
            id = "header",
            style = "background:url('pizza.jpg'); background-size:cover;",
            h1(class = "page-header", " Pizza Revenue Dashboard")
        ),
        #2.0 Controls & Summaries----
        div(
            id = "Controls",
            
            column(
                class = "container",
                id = "range_buttons",
                width = 3,
                ## 2.1 User Inputs----
                dateRangeInput(
                    inputId = "date_range_1",
                    label = "Enter a Data Range",
                    start = pizza_raw_df$date %>% min(),
                    end = pizza_raw_df$date %>% max()
                ),
                hr(),
                shinyWidgets::pickerInput(
                    inputId = "categories",
                    label = h4("Select Type"),
                    choices = pizza_raw_df$category %>% unique(),
                    selected = pizza_raw_df$category %>% unique(),
                    multiple = TRUE,
                    options = list(
                        `actions-box` = TRUE,
                        size = 10,
                        `selected-text-format` = "count > 3"
                    )
                ),
                ## 2.2 Revenue----
                hr(),
                h4("Revenues"),
                
                div(
                    uiOutput("revenue_summary")
                )
            )
        ),
        
        # 3.0 Custom Plot----
        column(
            width = 9,
            div(
                id = "range_settings",
                class = "hidden",
                selectInput(
                    inputId = "range_analysis",
                    label = "Type of analysis",
                    choices = c("Daily" = "day", "Weekly" = "week", "Monthly" = "month"),
                    selected = "day"
                )
            ),
            ##3.1 Time series UI buttons----
            div(
                class = "container-row",
                align = "pull-left",
                justified = TRUE,
                actionButton(inputId = "day", label = "Daily", icon = icon("sun")),
                actionButton(inputId = "week", label = "Weekly", icon = icon("calendar-week")),
                actionButton(inputId = "month", label = "Monthly", icon = icon("calendar-days")),
                actionButton(inputId = "reset_index", label = "Reset", icon = icon("rotate"))
            ),
            ## 3.2 Plot Time series----
            br(),
            
            uiOutput("plot")
        )
    ),
    hr(),
    fluidRow(
        
        # 4.0 Comments----
        div(
            class = "",
            id = "comments",
            column(
                width = 12,
                div(
                    class = "panel",
                    h4(" Analysis powered by data from", tags$small(a(href = "https://mavenanalytics.io/blog/maven-pizza-challenge", 
                                                                      targer = "_blank", "Maven Analytics")))
                )
                
            )
        )
        
    )
)