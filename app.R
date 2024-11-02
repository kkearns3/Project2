
# Katy Kearns
# ST 558 - Project 2
# 11/6/2024

# Libraries and source file
library(shiny)
library(shinyalert)
library(bslib)
library(tidyverse)
library(readr)
source("helpers.R")

# UI layout

ui <- fluidPage(
  
  # App title
  titlePanel("US Census PUMS Sample Data: North Carolina, 2023"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      
      h3("Choose Categories for Subset"),
      radioButtons("year_built",
                   "Year Built",
                   selected = "all",
                   choiceValues = c("all"),
                   choiceNames = c("All")
      ),
      radioButtons("unit_type",
                   "Type of Unit",
                   selected = "all",
                   choiceValues = c("all"),
                   choiceNames = c("All")
      ),
      
      h3("Choose Numeric Variables for Subset"),
      selectizeInput("num_var_1",
                     "Numeric Variable 1",
                     choices = c("(none)", 
                                 "INSP",
                                 "VALP"),
                     selected = "(none)"
                     ),
      sliderInput("num_range_1",
                  "Numeric Variable 1 (Min and Max)",
                  min = 0,
                  max = 100,
                  value = c(0, 100)
                  ),
      selectizeInput("num_var_2",
                     "Numeric Variable 2",
                     choices = c("(none)",
                                 "INSP",
                                 "VALP"),
                     selected = "(none)"
                     ),
      sliderInput("num_range_2",
                  "Numeric Variable 2 (Min and Max)",
                  min = 0,
                  max = 1000000,
                  value = c(0, 1000000)
                  ),
      actionButton("subset_button", "Run Subset")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("About"
                 
        ),
        tabPanel("Data Download",
                 card(h3("test"))
                 
        ),
        
        tabPanel("Data Exploration",
                 card(
                   card_header(h2("Summaries")),
                   fluidRow(
                     column(width = 4, 
                            
                            # choose which summaries to display
                            radioButtons("summ_types",
                                         "Display Summaries",
                                         choices = c("Categorical",
                                                     "Numerical",
                                                     "Both",
                                                     "None"),
                                         selected = "Both")
                            ),
                     column(width = 4,
                            
                            # choose categorical variable to summarize by
                            selectizeInput("summ_cat",
                                           "Choose Categorical Variable",
                                           choices = c("SEX",
                                                       "YRBLT"))
                            ),
                     column(width = 4,
                          
                            # choose numeric variable to summarize by
                            selectizeInput("summ_num",
                                          "Choose Numeric Variable",
                                          choices = c("INSP"))
                            )),
                   fluidRow("Categorical Summary (One-Way Contingency Table)"
                            
                            ),
                   fluidRow("Categorical Summary (Two-Way Contingency Table)"
                            
                            ),
                   fluidRow("Numerical Summary"
                            
                            )
                   ),
                 card(
                   card_header(h2("Plots")),
                   fluidRow(
                     column(width = 4,
                            selectizeInput("plot_x",
                                           "X variable",
                                           choices = c())
                            ),
                     column(width = 4,
                            selectizeInput("plot_y",
                                           "Y variable",
                                           choices = c())
                            ),
                     column(width = 4,
                            selectizeInput("plot_fill",
                                           "Fill color",
                                           choices = c())
                            )
                     )
                   )
                )
        )
        )
      )      
    )

server <- function(input, output, session) {
  
}


shinyApp(ui = ui, server = server)
