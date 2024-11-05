# Helper functions for project 2

# dependent data sets 
data_dictionary_names <- readRDS("dd_names.rds")
data_dictionary_values <- readRDS("dd_values.rds")
census <- readRDS("census.rds")

# NC map PUMAS geographic table 
pumas <- tigris::pumas(state = "NC", 
                       year = "2023", 
                       progress_bar = FALSE)


#### Variable lists for widgets

# Categorical variables for summarizing/plotting
cat_vars <- data_dictionary_names |>
  filter(variable_name %in% c("AGEP_Grp", "BLD_Grp", "TEN", "VEH", "YRBLT_Grp", "HHLDRHISP",
                              "HHL", "MV", "SEX", "NATIVITY", "ACCESSINET", "FS", "TYPEHUGQ")) |>
  arrange(value)

# Test: add names to the cat_vars (assign value, i.e. the description as the row names to make referencing the plot variables easier)
# cat_vars_plots <- as.data.frame(cat_vars)
# rownames(cat_vars_plots) <- cat_vars_plots[[2]]

# Options for Categorical Subset 1
cat_sub_1 <- census |>
  filter(!is.na(YRBLT_Grp)) |>
  select(YRBLT_Grp) |>
  distinct() |>
  arrange(YRBLT_Grp) |>
  mutate(data_label = as.character(YRBLT_Grp))


# Options for Categorical Subset 2
cat_sub_2 <- census |>
  filter(!is.na(BLD_Grp)) |>
  select(BLD_Grp) |>
  distinct() |>
  arrange(BLD_Grp) |>
  mutate(data_label = as.character(BLD_Grp))


# Numeric variables for summarizing/plotting and subsetting
num_vars <- data_dictionary_names |>
  filter(variable_name %in% c("INSP", "AGEP", "MRGP", "VALP", "HINCP", "OCPIP", "PINCP")) |>
  arrange(value)
  

# Helper function for assigning correct data type, and reassigning raw values to descriptive values using the PUMS data dictionary

clean_census_columns <- function(data_column) {
    
  # get variable name for column that needs to be cleaned
  var_name <- colnames(data_column)
  
  # pull relevant key-value pairs from data dictionary
  column_dictionary <- data_dictionary_values |>
    filter(variable_name == var_name)
  
  # join data dictionary to get data type and descriptive values - actually I don't need to left join, but save this for now since this shows how to join via a variable rather than a string literal
  # data_column <- data_column |>
  #   left_join(column_dictionary, by = setNames("value", var_name))

  # recast columns that should be numeric
  if (column_dictionary["data_type"] |> distinct() == "N") {
    data_column[[var_name]] <- as.numeric(data_column[[var_name]])
  }
  
  # if it's a categorical variable, change to factors
  #    - exclude the columns where value and max_value are not the same, as these are ranges and we don't want to overwrite those values
  if (column_dictionary["data_type"] |> distinct() == "C" &
      all(column_dictionary$value == column_dictionary$value_max)) {
    data_column[var_name] <- factor(data_column[[var_name]],
                                    levels = column_dictionary$value,
                                    labels = column_dictionary$data_label)
  }
  
  return(data_column)
}


# calculate weighted mean from vectors
#  - var = variable that mean should be calculated on
#  - weight = the weight, so for person-level census this is PWGTP
census_mean <- function(var, weight) {
  mean <- sum(var * weight)/sum(weight)
  return(mean)
}


# calculate weighted median from vectors
census_median <- function(var, weight) {
  # total person/household weights
  count <- sum(weight)
  
  # combine into a tibble, sort ascending by the variable value
  values <- tibble(var = var, weight = weight) |>
    arrange(var)
  
  # create a new column providing the cumulative sum of the weights. This is for proper positioning of the midpoint value
  values["cum_wt"] <- cumsum(values$weight)
  
  # thinking of the cum_wt as a counter keeping track of how many data points are represented by each row of the tibble. 
  #   - Last data point of the row == cum_wt
  #   - First data point in the row == cum_wt - weight + 1
  # Midpoint is between the first and last data point of the row
  #   - If total count is odd, then we just have to pull the one value of var at the midpoint
  #   - If total count is even, then we have to pull both the data point at the midpoint, and also the next data point, and take the average
  
  # find the midpoint of the data
  if (count %% 2 == 1) {
    
    # if count is odd, just take the value at the midpoint
    midpoint <- (count + 1)/2
    median <- values[values$cum_wt >= midpoint & midpoint >= values$cum_wt - values$weight + 1, "var"]
  
  } else {
    
    # else, count is even, need to take two values and average them
    midpoint <- count/2
    
    # pull first number
    first_num <- values[values$cum_wt >= midpoint & midpoint >= values$cum_wt - values$weight + 1, "var"]
    
    # pull second number
    midpoint <- midpoint + 1
    second_num <- values[values$cum_wt >= midpoint & midpoint >= values$cum_wt - values$weight + 1, "var"]
    
    median <- (first_num + second_num) / 2
  }

  return(as.numeric(median))
}

# Error stats of the esimate (i.e. the mean): Margin of error, standard error 

census_error <- function(df, group_var){
  
  # get necessary vectors
  var <- df |> select({{group_var}})
  weight <- df |> select(PWGTP)
  rep_weights <- df |> select(PWGTP1:PWGTP80)
  
  # main estimate of the mean based on the weight
  estimate <- census_mean(var, weight)
  
  # same estimate on each of the 80 replicate weights
  estimate_r <- rep_weights |>
    summarize(across(PWGTP1:PWGTP80, \(x) census_mean(var, x)))
  
  # calculate the squared differences between the estimate and the replicated estimates
  squared_diffs <- estimate_r |>
    mutate(across(PWGTP1:PWGTP80, 
                  \(x) (estimate - x)^2,
                  .names = "sqdiff_{.col}")) 
  
  # take the sum, multiply by 4/80 to create vector of variances
  variance <- squared_diffs |>
    select(starts_with("sqdiff_")) |>
    apply(MARGIN = 1, FUN = \(x) sum(x)*(4/80))
  
  # add variance to a data frame
  error <- tibble(variance = variance)
  
  # add the other error stats
  error <- error |>
    mutate(std_err = round(sqrt(variance)),
           margin_of_error = round(sqrt(variance) * 1.645))
  
  # return error stats
  return(error)
  
}

