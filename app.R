

# MAVEN PIZZA ANALYTICS

# lIBRARIES

# Shiny
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)

# Analysis
library(tidyverse)
library(timetk)
#library(lubridate)
#library(plotly)
library(DT)

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
    theme=shinytheme("darkly"),
    tags$head(
        tags$link(rel="stylesheet", type="text/css",href="styles.css")
    ),
    
    fluidRow(
        title = "Dashboard",
        
        # CSS
        #shinythemes::themeSelector(),
        
        # 1.0 Header----
        div(
            class = "jumbotron",
            id ="header",
            style ="background:url('pizza.jpg'); background-size:cover;",
            h1(class="page-header"," Pizza Revenue Dashboard")
        ),
        #2.0 Controls & Summaries----
        div(
            id ="Controls",
            
            column(
                class = "container",
                id    ="range_buttons",
                width = 3,
                ## 2.1 User Inputs----
                dateRangeInput(inputId = "date_range_1",
                               label   = "Enter a Data Range",
                               start   = pizza_raw_df$date %>% min(),
                               end     = pizza_raw_df$date %>% max()
                ),
                hr(),
                shinyWidgets::pickerInput(
                    inputId  = "categories", 
                    label    = h4("Select Type"), 
                    choices  = pizza_raw_df$category %>% unique(), 
                    selected = pizza_raw_df$category %>% unique(), 
                    multiple = TRUE,
                    options  = list(
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
                ),
              )
            ),
            
            # 3.0 Custom Plot----
            column(
                width=9,
                div(
                    id= "range_settings",
                    class = "hidden",
                    selectInput(inputId = "range_analysis",
                                label = "Type of analysis",
                                choices = c("Daily"="day","Weekly"="week","Monthly"="month"), selected = "day")
                ),
                ##3.1 Time series UI buttons----
                div(
                    class = "container-row",
                    align = "pull-left", justified=TRUE,
                    actionButton(inputId = "day",label="Daily",  icon = icon("sun")),
                    actionButton(inputId = "week",label="Weekly", icon = icon("calendar-week")),
                    actionButton(inputId = "month",label="Monthly",icon = icon("calendar-days")),
                    actionButton(inputId = "reset_index",label="Reset", icon=icon("rotate"))
                ),
                ## 3.2 Plot Time series----
                br(),

               uiOutput("plot")
            ),
        ),
    hr(),
    fluidRow(
      
      # 4.0 Comments----
      div(
        class="",
        id   ="comments",
        column(
          width = 12,
          div(class="panel",
              h4(" Analysis powered by data from", tags$small(a(href="https://mavenanalytics.io/blog/maven-pizza-challenge", 
                                                               targer="_blank","Maven Analytics")))
          )
          
        )
      )
      
    ),

    # tabPanel(
    #     title = "Prediction"
    # )
    
    
    
)

#SERVER----
server <- function(input, output, session){
    
    # 1.0 Settings----
    
    observe({
        updateDateInput(session=session,
                        inputId = "date_range_1")
    })
    
    observeEvent(input$week, {
        updateSelectInput(session, "range_analysis", choices = c("Daily" = "day", "Weekly" = "week", "Monthly" = "month"), selected = "week")
    })
    # 
    observeEvent(input$month, {
        updateSelectInput(session, "range_analysis", choices = c("Daily" = "day", "Weekly" = "week", "Monthly" = "month"), selected = "month")
    })
    
    observeEvent(input$day, {
        updateSelectInput(session, "range_analysis", choices = c("Daily" = "day", "Weekly" = "week", "Monthly" = "month"), selected = "day")
    })
    
    # Reset 
    observeEvent(input$reset_index,{
        updateSelectInput(session, "range_analysis", choices = c("Daily" = "day", "Weekly" = "week", "Monthly" = "month"), selected = "day")
        updateDateInput(session = session,
                        inputId = "date_range_1", 
                        min     =  pizza_raw_df$date %>% min(), 
                        max     = pizza_raw_df$date %>% max())
    })
    
   
    
    # Plot fun----
    
    output$plot <-renderUI({
      if(nrow(revenue())==0){
        renderText("Please select category of Pizza to display the plot")
      }else{
        renderPlotly({
          g <- pizza_order_tbl() %>% 
            group_by(category) %>%
            summarise_by_time(date,.by=input$range_analysis, value=sum(revenue)) %>%
            filter(date >= input$date_range_1[1] & date <= input$date_range_1[2]) %>%
            filter(category %in% input$categories) %>%
            plot_time_series(date,value, .smooth = F,
                             .color_var   = category,
                             .facet_ncol  = 1,
                             .title       = str_glue("Revenue by type of pizza per {input$range_analysis}"),
                             .interactive = F)+
            theme_dark()+
            theme(
              plot.background = element_rect(fill = "#303030"),
              panel.background = element_rect(fill = "#303030"),
              legend.background = element_rect(fill = "#303030"),
              legend.title = element_text(color = "white"),
              legend.text = element_text(color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(color = "white"),
              axis.title.x = element_text(color = "white"),
              axis.title.y = element_text(color = "white"),
              axis.text.x = element_text(color = "white"),
              axis.text.y = element_text(color = "white")
            )
          ggplotly(g)
        })
      }
    })
    
    # 1.2 Data Utilities----

    revenue <- reactive({

      pizza_order_tbl() %>%
        filter(date >= input$date_range_1[1] & date <= input$date_range_1[2]) %>%
        filter(category %in% input$categories) %>%
        group_by(category) %>%
        summarise(revenue=sum(revenue))
    })
    
    output$revenue_summary <- renderUI({
      
      summary_data <- revenue()
      
      tags$table(
        class = "table table-striped",
        tags$thead(
          tags$tr(
            tags$th("Pizza Type"),
            tags$th("Revenue")
          )
        ),
        tags$tbody(
          lapply(1:nrow(summary_data), function(i) {
            tags$tr(
              tags$td(summary_data$category[i]),
              tags$td(summary_data$revenue[i])
            )
          })
        )
      )
    })
}

shinyApp(ui, server)

