
# Katy Kearns
# ST 558 - Project 2
# 11/6/2024

# Libraries and source file
library(shiny)
library(shinyalert)
library(bslib)
#library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyselect)
#library(readr)
library(markdown)
library(shinycssloaders)

# global options
options(spinner.type = 8)

# Read required files
data_dictionary_names <- readRDS("dd_names.rds")
census <- readRDS("census.rds")

# source helpers script
source("helpers.R")

#-------- UI layout

ui <- fluidPage(
  
  # App title
  titlePanel(title = "US Census PUMS Sample Data: North Carolina, 2023",
             windowTitle = "PUMS NC 2023"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      
      h3("Choose Categories for Subset"),
      radioButtons("year_built",
                   "Year Built",
                   selected = "All",
                   choices = append("All", cat_sub_1[[2]])
      ),
      radioButtons("unit_type",
                   "Building Unit Type",
                   selected = "All",
                   choices = append("All", cat_sub_2[[2]])
      ),
      
      h3("Choose Numeric Variables for Subset"),
      selectizeInput("num_var_1",
                     "Numeric Variable 1",
                     choices = append("(none)", num_vars[,2]),
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
                     choices = append("(none)", num_vars[,2]),
                     selected = "(none)"
                     ),
      sliderInput("num_range_2",
                  "Numeric Variable 2 (Min and Max)",
                  min = 0,
                  max = 100,
                  value = c(0, 100)
                  ),
      actionButton("run_subset", "Run Subset",
                   style = "color: white; background-color: SteelBlue;"),
      br(),
      h3("Data Exploration Options"),
      radioButtons("view_exploration",
                         "Select Items to View",
                         choices = c("Summaries",
                                     "Map Plots",
                                     "Numeric Plots",
                                     "Categorical Plots"),
                         selected = c("Summaries"))
      
    ),
    mainPanel(
      tabsetPanel(selected = "About",
        tabPanel("About",
                 fluidRow(
                   includeMarkdown("include.md")
                 )
        ),
        tabPanel("Data Download",
                 card(
                   card_header(h2("Download PUMS Data Subset")),
                   fluidRow(
                     column(width = 11, offset = 1,
                     downloadButton("download_table", "Download")
                     )
                   ),
                   br(),
                   fluidRow(
                     column(width = 11, offset = 1,
                     DT::dataTableOutput("census_table")
                     )
                   )
                 )
                 
        ),
        
        tabPanel("Data Exploration",
              conditionalPanel("input.view_exploration.includes('Summaries')",
                 card(
                   card_header(h2("Summaries")),
                   fluidRow(
                     column(width = 3,
                            # choose which summaries to display
                            checkboxGroupInput("summ_types",
                                               "Display Summaries",
                                               choices = c("Categorical",
                                                           "Numeric"),
                                               selected = c("Categorical",
                                                            "Numeric"))
                            ),
                     column(width = 3,
                            
                            # choose categorical variable to summarize by
                            selectizeInput("summ_cat_1",
                                           "Choose Categorical Variable 1",
                                           choices = cat_vars[[1]],
                                           selected = "MV")
                            ),
                     column(width = 3,
                            
                            # choose categorical variable to summarize by
                            selectizeInput("summ_cat_2",
                                           "Choose Categorical Variable 2",
                                           choices = cat_vars[[1]],
                                           selected = "SEX")
                     ),
                     column(width = 3,
                          
                            # choose numeric variable to summarize by
                            selectizeInput("summ_num",
                                          "Choose Numeric Variable",
                                          choices = num_vars[[1]],
                                          selected = "INSP")
                            )),
                   conditionalPanel("input.summ_types.includes('Categorical')",
                     fluidRow(
                       column(width = 12, 
                              h3("Categorical Summary (One-Way Contingency Table)"),
                              withSpinner(tableOutput("oneway_cont"))
                              )
                     ),
                     fluidRow(
                       column(width = 12, 
                              h3("Categorical Summary (Two-Way Contingency Table)"),
                              withSpinner(tableOutput("twoway_cont"))
                              )
                     )
                   ),
                   conditionalPanel("input.summ_types.includes('Numeric')",
                     fluidRow(
                       column(width = 12, 
                              h3("Numerical Summary"),
                              withSpinner(tableOutput("summary"))
                              )
                     )
                   )
                 )
              )
                 ,
              conditionalPanel("input.view_exploration.includes('Map Plots')",
                 h2("Map Plots"),
                 # Map plots, with controls
                 card(
                   card_header(h3("Map Plots")),
                   p("Note: X and Y variables are plotted on the heatmap only. ",
                     "NC Map plots the median of the fill variable only."
                     ),
                   fluidRow(
                     column(width = 4,
                            selectizeInput("map_x",
                                           "X variable",
                                           choices = cat_vars[1],
                                           selected = "AGEP_Grp")
                            ),
                     column(width = 4,
                            selectizeInput("map_y",
                                           "Y variable",
                                           choices = cat_vars[1],
                                           selected = "VEH")
                            ),
                     column(width = 4,
                            selectizeInput("map_fill",
                                           "Color by",
                                           choices = num_vars[1],
                                           selected = "VALP")
                            )
                   ),
                   fluidRow(
                     column(width = 11, offset = 1,
                       # Plot: NC Map
                       withSpinner(
                         plotOutput("nc_map"))
                     )
                   ),
                   br(),
                   fluidRow(
                     column(width = 11, offset = 1,
                       # Plot: heatmap
                       withSpinner(
                         plotOutput("heatmap_plot"))
                     )
                   )
                   )
                 ),
              conditionalPanel("input.view_exploration.includes('Numeric Plots')",
                 card(
                   card_header(h3("Numeric Plots")),
                   p("Note: Density plot uses only the X and fill variables, as the ",
                     "Y axis represents the count of individuals in the subset"),
                   fluidRow(
                     column(width = 4,
                            selectizeInput("plot1_x",
                                           "X variable",
                                           choices = num_vars[1],
                                           selected = "INSP")
                     ),
                     column(width = 4,
                            selectizeInput("plot1_y",
                                           "Y variable",
                                           choices = num_vars[1],
                                           selected = "AGEP")
                     ),
                     column(width = 4,
                            selectizeInput("plot1_fill",
                                           "Color by",
                                           choices = cat_vars[1],
                                           selected = "VEH")
                     )
                   ),
                   br(),
                   fluidRow(
                     column(width = 11, offset = 1,
                       # Plot: kernel density plot
                       withSpinner(
                         plotOutput("density_plot"))
                     )
                   ),
                   br(),
                   fluidRow(
                     column(width = 11, offset = 1,
                       # Plot: scatter plot
                       withSpinner(
                         plotOutput("scatter_plot"))
                     )
                   )
                 )
                 ),
              conditionalPanel("input.view_exploration.includes('Categorical Plots')",
                 card(
                   card_header(h3("Categorical Plots")),
                   p("Note: Bar plot uses only the X and fill variables, as the ",
                     "Y axis represents the count of individuals in the subset"),
                   fluidRow(
                     column(width = 3,
                            selectizeInput("plot2_x",
                                           "X variable",
                                           choices = cat_vars[1],
                                           selected = "FS")
                     ),
                     column(width = 3,
                            selectizeInput("plot2_y",
                                           "Y variable",
                                           choices = num_vars[1],
                                           selected = "INSP")
                     ),
                     column(width = 3,
                            selectizeInput("plot2_fill",
                                           "Fill color",
                                           choices = cat_vars[1],
                                           selected = "ACCESSINET")
                     ),
                     column(width = 3,
                            selectizeInput("plot2_facet",
                                           "Facet by",
                                           choices = cat_vars[1],
                                           selected = "AGEP_Grp")
                     )
                   ),
                   br(),
                   fluidRow(
                     column(width = 11, offset = 1,
                       # Plot: Bar with facet
                       withSpinner(
                         plotOutput("bar_plot"))
                     )
                   ),
                   fluidRow(
                     column(width = 10, offset = 2,
                       uiOutput("facet_info")
                     )
                   ),
                   br(),
                   fluidRow(
                     column(width = 11, offset = 1,
                       # Plot: violin plot
                       withSpinner(
                         plotOutput("violin_plot"))
                     
                     )
                   )
                  )
              )
                )
        )
        )
      )      
    )


server <- function(input, output, session) {
  
  # ------------- Data Exploration -------------
  
  # update slider 1 values when num var 1 is changed
  observeEvent(input$num_var_1, {
    
    # get data column name from the descriptive input
    num_label <- input$num_var_1
    
    # if selected value is (none), set initial values
    if (num_label == "(none)") {
      slider_range <- tibble(min_val = 0, max_val = 100)
    } else {
      col_name <- data_dictionary_names |>
        filter(value == num_label) |>
        select(variable_name) |>
        as.character()
      
      slider_range <- census_subset() |>
        select(any_of(col_name)) |>
        drop_na(any_of(col_name)) |>
        summarize(across(everything(), 
                         list(min_val = min, max_val = max),
                         .names = "{.fn}")) 
    }
    
    updateSliderInput(session, "num_range_1",
                      min = slider_range$min_val,
                      max = slider_range$max_val,
                      value = slider_range)
  })
  
  
  # update slider 2 values when num var 2 is changed
  observeEvent(input$num_var_2, {
    
    # get data column name from the descriptive input
    num_label <- input$num_var_2
    
    # if selected value is (none), set initial values
    if (num_label == "(none)") {
      slider_range <- tibble(min_val = 0, max_val = 100)
    } else {
      col_name <- data_dictionary_names |>
        filter(value == num_label) |>
        select(variable_name) |>
        as.character()
      
      slider_range <- census_subset() |>
        select(any_of(col_name)) |>
        drop_na(any_of(col_name)) |>
        summarize(across(everything(), 
                         list(min_val = min, max_val = max),
                         .names = "{.fn}")) 
    }

    updateSliderInput(session, "num_range_2",
                      min = slider_range$min_val,
                      max = slider_range$max_val,
                      value = slider_range)
  })
  
  # subset data when action button is pressed
  census_subset <- eventReactive(input$run_subset, {
    
    # cat subset 1 (year_built)
    if(input$year_built == "All") {
      YRBLT_vals <- cat_sub_1[[2]]
    } else {
      YRBLT_vals <- input$year_built
    }

    # cat subset 2 (unit_type)
    if(input$unit_type == "All") {
      BLD_vals <- cat_sub_2[[2]]
    } else {
      BLD_vals <- input$unit_type
    }
    
    # num subset 1 (num_var_1, num_range_1)
    col_name_1 <- data_dictionary_names |>
      filter(value == input$num_var_1) |>
      select(variable_name) |>
      as.character()
    
    # num subset 2 (num_var_2, num_range_2)
    col_name_2 <- data_dictionary_names |>
      filter(value == input$num_var_2) |>
      select(variable_name) |>
      as.character()
    
    # subset census data, select only needed columns
    #   Note: referenced Dr. P's HW7 file for the numeric range filtering
    census_subset <- census |>
      filter(
        YRBLT_Grp %in% YRBLT_vals,
        BLD_Grp %in% BLD_vals
      ) %>%
      {if(col_name_1 == "INSP") filter(., INSP >= input$num_range_1[1] & 
                                          INSP <= input$num_range_1[2]) else .} %>%
      {if(col_name_1 == "AGEP") filter(., AGEP >= input$num_range_1[1] & 
                                          AGEP <= input$num_range_1[2]) else .} %>%
      {if(col_name_1 == "MRGP") filter(., MRGP >= input$num_range_1[1] & 
                                          MRGP <= input$num_range_1[2]) else .} %>%
      {if(col_name_1 == "VALP") filter(., VALP >= input$num_range_1[1] & 
                                          VALP <= input$num_range_1[2]) else .} %>%
      {if(col_name_1 == "HINCP") filter(., HINCP >= input$num_range_1[1] & 
                                           HINCP <= input$num_range_1[2]) else .} %>%
      {if(col_name_1 == "OCPIP") filter(., OCPIP >= input$num_range_1[1] & 
                                           OCPIP <= input$num_range_1[2]) else .} %>%
      {if(col_name_1 == "PINCP") filter(., PINCP >= input$num_range_1[1] & 
                                           PINCP <= input$num_range_1[2]) else .} %>%
      {if(col_name_2 == "INSP") filter(., INSP >= input$num_range_2[1] & 
                                         INSP <= input$num_range_2[2]) else .} %>%
      {if(col_name_2 == "AGEP") filter(., AGEP >= input$num_range_2[1] & 
                                         AGEP <= input$num_range_2[2]) else .} %>%
      {if(col_name_2 == "MRGP") filter(., MRGP >= input$num_range_2[1] & 
                                         MRGP <= input$num_range_2[2]) else .} %>%
      {if(col_name_2 == "VALP") filter(., VALP >= input$num_range_2[1] & 
                                         VALP <= input$num_range_2[2]) else .} %>%
      {if(col_name_2 == "HINCP") filter(., HINCP >= input$num_range_2[1] & 
                                          HINCP <= input$num_range_2[2]) else .} %>%
      {if(col_name_2 == "OCPIP") filter(., OCPIP >= input$num_range_2[1] & 
                                          OCPIP <= input$num_range_2[2]) else .} %>%
      {if(col_name_2 == "PINCP") filter(., PINCP >= input$num_range_2[1] & 
                                          PINCP <= input$num_range_2[2]) else .}

  },
  ignoreNULL = FALSE)
  
  #----- Summaries
  output$oneway_cont <- renderTable({
    
    # one-way contingency table
    cont_table <- census_subset() |>
      group_by(get(input$summ_cat_1)) |>
      filter(!is.na(get(input$summ_cat_1))) |>
      summarize(individuals = sum(PWGTP)) 
    
    # rename column so it provides the variable name
    names(cont_table)[1] <- input$summ_cat_1 
    
    cont_table
  })
  
  output$twoway_cont <- renderTable({
   
    # two-way contingency table
    cont_table <- census_subset() |>
      group_by(get(input$summ_cat_1), get(input$summ_cat_2)) |>
      filter(!is.na(get(input$summ_cat_1)) & !is.na(get(input$summ_cat_2))) |>
      summarize(individuals = sum(PWGTP)) |>
      pivot_wider(names_from = `get(input$summ_cat_2)`, values_from = individuals)
    
    # rename column so it provides the variable name
    names(cont_table)[1] <- input$summ_cat_1 
    
    cont_table
    
  })
  
  output$summary <- renderTable({
  
    # main stats
    summary_main <- census_subset() |>
      group_by(.data[[input$summ_cat_1]]) |>
      drop_na(.data[[input$summ_cat_1]], .data[[input$summ_num]]) |>
      summarize(mean = census_mean(.data[[input$summ_num]], PWGTP),
                median = census_median(.data[[input$summ_num]], PWGTP))
    
    # error summary
    summary_error <- census_subset() |>
      group_by(.data[[input$summ_cat_1]]) |>
      drop_na(.data[[input$summ_cat_1]], .data[[input$summ_num]]) |>
      do(census_error(., all_of(input$summ_num)))
    
    # final summary - above two should end up with the same grouping variable values in the same order, but going to do a full join just in case
    num_summary <- summary_main |>
      full_join(summary_error)
    
    num_summary
    
  })
  
  #----- Plots
  
  output$nc_map <- renderPlot({
  
    # ------- Plot Group 0 (Maps) 
    
    # aggregate census data
    census_aggregate <- census_subset() |>
      group_by(PUMA) |>
      drop_na(.data[[input$map_fill]]) |>
      summarize(median = census_median(.data[[input$map_fill]], PWGTP))
    
    # join to census data
    census_map <- pumas |>
      left_join(census_aggregate, join_by(PUMACE20 == PUMA))
    
    x_label <- data_dictionary_names |>
      filter(variable_name == input$map_fill) |>
      select(value)
    
    # plot data
    ggplot(census_map) +
      geom_sf(aes(fill = median)) +
      ggtitle(paste0("Median of ", x_label, "\n", " by PUMA")) +
      theme(plot.title = element_text(hjust = 0.5)) 
    
  })

  output$heatmap_plot <- renderPlot({
    
    # values for labels
    x_label <- data_dictionary_names |>
      filter(variable_name == input$map_x) |>
      select(value)
    
    y_label <- data_dictionary_names |>
      filter(variable_name == input$map_y) |>
      select(value)
    
    legend_label <- data_dictionary_names |>
      filter(variable_name == input$map_fill) |>
      select(value)
    
    # base object with global assignments
    g <- ggplot(data = census_subset() |> drop_na(.data[[input$map_x]], 
                                                  .data[[input$map_y]], 
                                                  .data[[input$map_fill]]), 
                aes(x = .data[[input$map_x]], y = .data[[input$map_y]], 
                    fill = .data[[input$map_fill]], weight = PWGTP))
    
    g + geom_tile() +
      ggtitle(paste0("Heatmap of ", x_label, "\n", " by ", y_label)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = x_label, y = y_label, fill = legend_label) 
    
  })
  
  # ------- Plot Group 1 (Num vs Num) 

  output$scatter_plot <- renderPlot({
    
    # values for labels
    x_label <- data_dictionary_names |>
      filter(variable_name == input$plot1_x) |>
      select(value)
    
    y_label <- data_dictionary_names |>
      filter(variable_name == input$plot1_y) |>
      select(value)
    
    legend_label <- data_dictionary_names |>
      filter(variable_name == input$plot1_fill) |>
      select(value)
    
    # take sample of the data (500 points) - note: referenced HW7 app.R file for help with setting up the sample size correctly 
    sample_size <- sample(1:nrow(census_subset()), 
                          size = 500,
                          replace = TRUE,
                          prob = census_subset()$PWGTP/sum(census_subset()$PWGTP))
    
    # sample for plotting
    census_sample <- census_subset()[sample_size, ]
    
    # base object with global assignments
    g <- ggplot(data = census_sample |> drop_na(.data[[input$plot1_x]], 
                                                .data[[input$plot1_y]],
                                                .data[[input$plot1_fill]]), 
                aes(x = .data[[input$plot1_x]], y = .data[[input$plot1_y]], 
                    color = .data[[input$plot1_fill]], weight = PWGTP))
    
    g + geom_point() +
      ggtitle(paste0("Scatter plot of ", x_label, "\n", " by ", y_label)) + 
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = x_label, y = y_label, color = legend_label) 
  })
  
  output$density_plot <- renderPlot({
    
    # values for labels
    x_label <- data_dictionary_names |>
      filter(variable_name == input$plot1_x) |>
      select(value)
    
    legend_label <- data_dictionary_names |>
      filter(variable_name == input$plot1_fill) |>
      select(value)
    
    # base object with global assignments
    g <- ggplot(data = census_subset() |> drop_na(.data[[input$plot1_x]], 
                                                  .data[[input$plot1_fill]]), 
                aes(x = .data[[input$plot1_x]], weight = PWGTP))
    
    # density plot
    g + geom_density(aes(fill = .data[[input$plot1_fill]]), kernel = "gaussian", alpha = 0.4) +
      ggtitle(paste0("Density plot of ", x_label)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = x_label, y = "Individuals") +
      scale_fill_discrete(legend_label) # legend crowds the screen, label too long
     
    
  })

  # ------- Plot Group 2 (Cat vs Num) 
  
  output$bar_plot <- renderPlot({
    
    # values for labels
    x_label <- data_dictionary_names |>
      filter(variable_name == input$plot2_x) |>
      select(value)
    
    legend_label <- data_dictionary_names |>
      filter(variable_name == input$plot2_fill) |>
      select(value)
    
    facet_label <- data_dictionary_names |>
      filter(variable_name == input$plot2_facet) |>
      select(value)
    
    g <- ggplot(data = census_facets() |> drop_na(.data[[input$plot2_x]], 
                                                .data[[input$plot2_fill]], 
                                                .data[[input$plot2_facet]]), 
                aes(x = .data[[input$plot2_x]], weight = PWGTP, fill = .data[[input$plot2_fill]]))
    
    # layers (remove the legend from the base layer, keep the legend for the fill)
    g + geom_bar() +
      ggtitle(paste0(x_label, "\n", " filled by ", legend_label, "\n",
                     "Faceted by ", facet_label)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = x_label, y = "Individuals") +
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      scale_fill_discrete(legend_label) +
      facet_wrap(vars(.data[[input$plot2_facet]]))
    
  })
  
  output$violin_plot <- renderPlot({
    
    # values for labels
    x_label <- data_dictionary_names |>
      filter(variable_name == input$plot2_x) |>
      select(value)
    
    y_label <- data_dictionary_names |>
      filter(variable_name == input$plot2_y) |>
      select(value)
    
    legend_label <- data_dictionary_names |>
      filter(variable_name == input$plot2_fill) |>
      select(value)
    
    # base object with global assignments
    g <- ggplot(data = census_subset() |> drop_na(.data[[input$plot2_x]], 
                                                  .data[[input$plot2_y]],
                                                  .data[[input$plot2_fill]]), 
                aes(y = .data[[input$plot2_y]], weight = PWGTP))
    
    g + geom_violin(aes(x = .data[[input$plot2_x]], fill = .data[[input$plot2_fill]])) +
      ggtitle(paste0("Violin plot of ", x_label, "\n", " by ", y_label)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = x_label, y = y_label) +
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      scale_fill_discrete(legend_label)
    
  })
  
  # create a reactive Values object for holding counts of facets in the original subset, and in the faceted subset. To be passed to the conditional message informing user about which facets are displayed
  facet_counts <- reactiveValues(original = 0, displayed = 0)
  
  # limit number of facets - generate data set with 4 or fewer levels
  # (currently for "bar_plot")
  census_facets <- reactive({
    
    # check how many facets would be created
    facets <- census_subset() |>
      group_by(.data[[input$plot2_facet]]) |>
      drop_na(.data[[input$plot2_facet]]) |>
      summarize(count = sum(PWGTP)) |>
      arrange(desc(count))
    
    # assign original subset to reactive Values object
    facet_counts$original <- length(facets[[1]])
    
    # if more than 4, isolate subsets to the 4 most common levels
    if (facet_counts$original > 4) {
      census_facets <- census_subset() |>
        filter(.data[[input$plot2_facet]] %in% facets[[1]][1:4])
    } else {
      census_facets <- census_subset()
    }
    
    # set how many facets will be displayed
    facet_counts$displayed <- census_facets |>
      distinct(get(input$plot2_facet)) |>
      summarize(count = n())
    
    census_facets
    
  })
  
  output$facet_info <- renderUI({
         
    em(HTML(paste0("Note: Plots are limited to a maximum of 4 facets.<br>",
                   "Your selection has omitted ", 
                   facet_counts$original - facet_counts$displayed, 
                   " facets.")))
    
  })
  
  # -------- Data Exploration --------------
  
  output$census_table <- DT::renderDataTable({
    
    # keep only relevant columns in census data
    keep_cols <- append(cat_vars$variable_name, num_vars$variable_name) |>
      append(c("PWGTP", "YRBLT"))

    # data table object for download tab
    census_display <- census_subset() |>
      select(any_of(keep_cols))
    
    # census_display <- census_subset() |>
    #   select(any_of(c("AGEP_Grp", "BLD_Grp", "TEN", "VEH", "YRBLT_Grp", "HHLDRHISP",
    #   "HHL", "MV", "SEX", "NATIVITY", "ACCESSINET", "FS", "TYPEHUGQ", "PWGTP", "YRBLT")))
    
  })
  
  # new subset created specifically for the csv download (the table created by DT::renderDataTable seems to be creating the file as html)
  census_download <- reactive({
    
    keep_cols <- append(cat_vars$variable_name, num_vars$variable_name) |>
      append(c("PWGTP", "YRBLT"))
    
    census_subset() |>
      select(any_of(keep_cols)) 
  })
  
  # download table when button pressed
  output$download_table <- downloadHandler(
    filename = "PUMS_subset.csv",
    content = function(file) {
      write.csv(census_download(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  #-------------Errors and Alerts
  
  # send alerts and/or update widgets when 
  observeEvent(
    c(input$num_var_1, input$num_var_2,
      input$summ_cat_1, input$summ_cat_2,
      input$map_x, input$map_y,
      input$plot1_x, input$plot1_y, 
      input$plot2_x, input$plot2_y, input$plot_fill), {
    
      # if numeric subset variables are the same (and not equal to "(none)", send
      # shiny alert, and reset 2nd variable to "(none)")
      if (input$num_var_1 == input$num_var_2 & input$num_var_1 != "(none)") {
        # create alert
        shinyalert(title = "Oops!",
                   text = "Please choose two different variables to subset",
                   type = "warning")
      
        # reset slider for 2nd variable
        updateSliderInput(session,
                          "num_var_2",
                          value = "(none)")
      }
    
      # two-way contingency table: prevent user from selecting the same 2 categorical variables
      if (input$summ_cat_1 == input$summ_cat_2) {
        # create alert
        shinyalert(title = "",
                   text = "Please choose two different variables for the contingency table",
                   type = "warning")
        
        # create updated list of choices, excluding the 1st cat variable selection
        valid_choices <- cat_vars[[1]]
        valid_choices <- valid_choices[-which(valid_choices == input$summ_cat_1)]
        
        # select a different value in the widget (not removing from the list, since that choice
        # would remain missing unless added back)
        updateSelectizeInput(session,
                             "summ_cat_2",
                             selected = valid_choices[1])
      }
    
        # heatmap: advise user to select different variables
        if (input$map_x == input$map_y) {
          # create alert
          shinyalert(title = "Heatmap look funny?",
                     text = "Choosing different variables for X and Y may be more informative!",
                     type = "info")
          
          # might need to change value...? test more...
        }
        
        # scatterplot: advise user to select different variables
        if (input$plot1_x == input$plot1_y) {
          # create alert
          shinyalert(title = "Scatterplot look funny?",
                     text = "Choosing different variables for X and Y may be more informative!",
                     type = "info")
          
          # might need to change value...? test more...
        }
        
        # Bar/Violin: advise user to select different variables
        if (input$plot2_x == input$plot2_fill |
            input$plot2_x == input$plot2_facet |
            input$plot2_fill == input$plot2_facet) {
          
          # create alert
          shinyalert(title = "Bar/Violin Plots look funny?",
                     text = "Choosing different variables for X, Fill, and Facet may be more informative!",
                     type = "info")
          
          # might need to change value...? test more...
        }
          

      
  })
  
  
}


shinyApp(ui = ui, server = server)
