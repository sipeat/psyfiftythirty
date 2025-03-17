#Pre-Checks, Double Checks, & Descriptives--------------------------------------
#Purpose: To easily run pre-checks, Double Checks, & Descriptives, so that I can be efficient in coding Quant Projects
#Drew Sipe, 3/7/2025 & 3/16/2025

#Installing Packages------------------------------------------------------------
if (!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if (!require('moments')) install.packages('moments'); library(moments)
if (!require('patchwork')) install.packages('patchwork'); library(patchwork)
if (!require('data.table')) install.packages('data.table'); library(data.table)

#Establishing Functions---------------------------------------------------------
##Initial descriptives function-------------------------------------------------

library(moments) #allows for faster execution for descriptives on large datasets
library(patchwork)  # For combining plots

initial_desc <- function(df, columns, df_name = "dataset") {
  df <- as.data.table(df)  # Convert to data.table for efficiency
  results_list <- list()   # To store results for each column
  
  # Function to determine the scale of measurement
  get_possible_scale_measurement <- function(column_data) { #specifying column_data
    if (is.numeric(column_data)) { #making sure the provided data is in numeric form
      unique_vals <- length(unique(na.omit(column_data)))#gets the number of unique values in each column

      # If very few unique values, it's likely categorical or ordinal
      if (unique_vals <= 12) { #less than 12 we are likely dealing with some sort of likert scale
        if (all(column_data %% 1 == 0, na.rm = T)) {# if there are no decimals it is also likely a categorical variable
          return("Ordinal (Discrete Categories)")
        } else { #else there are decimals indicating interval level data
          return("Interval (Likely Scale Data)") 
        }
      } #all following variables have more than 12 unique values
      
      # Check if data has a T zero (Ratio Scale)
      min_val <- min(column_data, na.rm = T) # geting the minimum value
      has_negative <- any(column_data < 0, na.rm = T) #seeing if there are negative values, any values less than 0 
      
      if (min_val == 0 && !has_negative) { #if there are more than 12 unique values with a T 0, likely ratio
        return("Ratio (Continuous, T Zero)") 
      } else {
        return("Interval (Continuous, No T Zero)") #with negatives there is likely no T 0 
      }
      
    } else if (is.character(column_data) || is.factor(column_data)) { #if data is not in numeric form, list or likely character
      unique_vals <- length(unique(na.omit(column_data))) #getting the 3 of unique entries in the non numeric data
      if (unique_vals <= 12) { # if there are few unqiues, this is likely categorical or scale values in character format
        return("Nominal (Categorical, Limited Levels)")
      } else { #with many uniques, this is likely character data, with many categories
        return("Nominal (Categorical, Many Levels)")
      }
    } else { # if none of the above conditions are T, there should be further exploration of the raw data
      return("Needs Further Exploration")
    }
  }
  
  for (col in columns) { #for each column in the list of columns provided
    if (!col %in% names(df)) { #this handles misspellings or mistakes in provided columns, while continuing the rest of the column descrptives
      warning(paste("Column", col, "not found in the dataset. Skipping."))
      next #skips this column and moves to the next in the list if it exists
    }
    
    column_data <- df[[col]] #allows us to work with one column at a time,
    
    # Determine structure
    structure_type <- typeof(column_data) #gives us the structure of the data
    
    # Determine scale of measurement
    possible_scale_measurement <- get_possible_scale_measurement(column_data) #uses the above get possible scale measurement fucntion to determine a scale of measurement
    
    # Count NA and Infinites, allows us to determine if data needs to be looked at
    n_na <- sum(is.na(column_data)) #creates a count of NA's
    n_inf <- sum(is.infinite(column_data), na.rm = T) #creates a count of infinites in the data
    
    # Frequency table including NA and Infinites
    freq_table <- as.data.table(table(column_data, useNA = "always")) #gives us a frequency table as a data table
    setnames(freq_table, c("Value", "Frequency")) # provides column names for the data table
    
    # Compute Descriptive Statistics
    n_total <- length(column_data) #gives total # of rows
    n_complete <- n_total - n_na # gives # of rows complete by subtracting the na count
    completion_rate <- n_complete / n_total # gives a % for how often that column has entries
    
    if (is.numeric(column_data)) { #if the column is numeric we can compute many descriptives
      mean_val <- mean(column_data, na.rm = T) #gives p50 a common mct
      median_val <- median(column_data, na.rm = T) #gives median another measure of central tendency (mct)
      mode_val <- as.numeric(names(sort(table(column_data), decreasing = T)[1])) #provides the most common entry, our last, least used mct
      sd_val <- sd(column_data, na.rm = T) #quantifies the variation within the data
      skew_val <- skewness(column_data, na.rm = T) #gives us the skew, numerically showing which way the graph tails off
      kurtosis_val <- kurtosis(column_data, na.rm = T) #shows us how normal of a distribution we have
      percentiles <- quantile(column_data, probs = c(0, 0.05, 0.25, 0.50, 0.75, 0.95, 1), na.rm = T) #gives us every percentile that can likely give us a good picture of the entire data
      iqr_val <- IQR(column_data, na.rm = T) #takes p75-p25 to give us the interquartile range
      
      # Identify outliers
      min_p_outliers <- list(column_data[column_data <= percentiles[2] & !is.na(column_data)])# specifies p05, values near the minimum of the dataset, data below this is likely to contain outliers
      max_p_outliers <- list(column_data[column_data >= percentiles[6] & !is.na(column_data)])# specifies p95, values near the maximum of the dataset, data above this is likely to contain outliers
      
      # Store results in a list
      descriptives <- list( #creates a list to append into a df
        df_name = df_name, column = col, #specifying the column
        structure = structure_type, possible_scale_measurement = possible_scale_measurement,
        n_complete = n_complete, n_missing = n_na, completion_rate = round(completion_rate, 4),
        mean = round(mean_val, 4), median = median_val, mode = mode_val, sd = round(sd_val, 4),
        skew = round(skew_val, 4), kurtosis = round(kurtosis_val, 4),
        min = percentiles[1], p5 = percentiles[2], p25 = percentiles[3], p50 = percentiles[4], 
        p75 = percentiles[5], p95 = percentiles[6], max = percentiles[7], IQR = iqr_val,
        min_p_outliers = min_p_outliers, max_p_outliers = max_p_outliers
      )#above code organizes all descriptives we are interested in and rounds necessary values to the 4th decimal place
      
      # --- Generate Combined Plot ---
      
      # Histogram (100 bins) - FIXED using `!!sym(col)`
      hist_plot <- ggplot(df, aes(x = !!sym(col))) + #specifies the column we want
        geom_histogram(bins = 100, fill = "blue", alpha = 0.6) + #blue because why not, lots of bins to view variability more easily, alpha specifies opacity
        theme_minimal() + #making it look good
        ggtitle(paste("Histogram of", col))# labeling
      
      # Boxplot - FIXED using `!!sym(col)`
      box_plot <- ggplot(df, aes(y = !!sym(col))) + #specifies the column we want
        geom_boxplot(fill = "lightblue", alpha = 0.6) +#lightblue and opacity at 40%
        theme_minimal() + #making it look good
        ggtitle(paste("Boxplot of", col)) #labeling
      
      # Violin Plot - FIXED using `!!sym(col)`
      violin_plot <- ggplot(df, aes(x = "", y = !!sym(col))) +  # Adding x = "" for a single group #specifies the column we want
        geom_violin(fill = "purple", alpha = 0.6) +#purple, and opacity at 40%
        theme_minimal() + #making it look good
        ggtitle(paste("Violin Plot of", col)) + #labeling
        xlab("")  # Remove x axis label, not needed
      
      
      # Combine all three plots
      combined_plot <- (hist_plot / (box_plot | violin_plot)) + plot_annotation(title = paste("Exploratory Analysis of", col))
      #creates one overall plot to look at a all exploratory plots of a column in 1 plot 
      
      # Display the combined plot
      print(combined_plot)
      
    } else { #for non-numeric data we cannot create plots and there are not many possible descriptives
      mode_val <- names(sort(table(column_data), decreasing = T)[1]) #getting the only mct necessary
      descriptives <- list( #creating a list to append descriptives of the column to
        df_name = df_name, column = col, #specifying column
        structure = structure_type, possible_scale_measurement = possible_scale_measurement,
        n_complete = n_complete, n_missing = n_na, completion_rate = round(completion_rate, 4),
        mode = mode_val, min_p_outliers = NA, max_p_outliers = NA
      ) #specifies the necessary columns
    }
    
    # Save results to list
    results_list[[paste0(df_name, "_", col)]] <- descriptives
    
    # Print results
    cat("\n===========================================\n")
    cat("Column:", col, "\n")
    cat("Structure:", structure_type, "\n")
    cat("Scale of Measurement:", possible_scale_measurement, "\n")
    cat("NA's:", n_na, " | Infinites:", n_inf, "\n\n")
    
    cat("Frequency Table:\n")
    print(freq_table)
    
    cat("\nDescriptive Statistics:\n")
    for (stat in names(descriptives)[3:21]) {  # Exclude df_name, column, and outliers from print loop, they are kept in the df
      cat(sprintf("%-15s: %s\n", stat, descriptives[[stat]]))#printing initial descrptives as needed
    } 
    
    cat("===========================================\n")
  }
  
  # Convert results list to a data.table
  results_df <- rbindlist(lapply(results_list, as.data.table), fill = T) #combines descrptives from different rows
  
  # Dynamically name the output dataframe
  output_var_name <- paste0(df_name, "_initial_descriptives") #initializes a df
  
  # Assign the results to a dynamically named variable in the global environment
  assign(output_var_name, results_df, envir = .GlobalEnv) 
  
}

#####example syntax-------------------------------------------------------------
#initial_desc(raw_data_bfss, c("state", "fmonth", "genhlth","age","lsatisfy"), df_name= "bfss")

#That is what I am talking about!!!
##This initial_desc function gives us:
##na's, infinites, structure, scale, n, mean, median, mode, skek, kurtosis, percentiles, IQR, possible outliers
##In addition to stats, it gives us a histogram, boxplot, & violin plot to look at

##Creating Doublecheck Functions-------------------------------------------------
###Need to briefly create Doublecheck conditions
#dc1<-raw_data_bfss %>% mutate( adsleep_r= ifelse(adsleep >= 14, NA, adsleep))
#dc2<-raw_data_bfss %>% mutate( lsatisfy_r= case_when( lsatisfy %in% c(7,9,NA) ~ NA, 
#                                                     T ~ 5 - lsatisfy ))
#dc3<- raw_data_bfss %>% 
#  dplyr::mutate(add_sum = rowSums(across(c(adpleasr, addown, adsleep, adenergy, adthink)), na.rm = T), #listwise deletion
#                add_mean = rowMeans(across(c(adpleasr, addown, adsleep, adenergy, adthink)), na.rm = T))


#initial_desc(raw_data_bfss, c("lsatisfy"), df_name= "lsatisfy")

###dc_recode :Doublecheck Recoding (most mutates)-------------------------------
#this is if you are excluding values
#this only works for variables with two columns, ex lsatisfy & lsatisfy_r
dc_recode <- function(df, col, change, replace_with = NA) {
  
  # Select only the specified columns for recoding
  df <- df %>% select(all_of(col))
  
  df1 <- df[, 1, drop = FALSE]  # Select first column (original variable before recoding)
  colnames(df1) <- "recoded_column"  # Rename for standard comparison
  
  df2 <- df[, 2, drop = FALSE]  # Select second column (expected recoded variable)
  colnames(df2) <- "recoded_column"  # Rename for standard comparison
  
  # Convert both to character before anti-join to handle type changes
  df1_num <- df1 %>% mutate(recoded_column = as.numeric(recoded_column))
  df2_num <- df2 %>% mutate(recoded_column = as.numeric(recoded_column))
  
  df3 <- anti_join(df1_num, df2_num, by = "recoded_column")  # Finds rows that changed after recoding
  
  # Getting frequency tables before and after recoding
  frequent1 <- janitor::tabyl(df1$recoded_column, show_na = TRUE) # Creates frequency table for original values
  frequent2 <- janitor::tabyl(df2$recoded_column, show_na = TRUE) # Creates frequency table for recoded values
  
  # Applying recoding rule (supports both logical conditions & transformations like as.factor)
  if (grepl("x", change)) { 
    # If 'x' is found in 'change', treat it as a logical condition (e.g., "x > 10")
    df1 <- df1 %>% 
      mutate(recoded_column = ifelse(eval(parse(text = gsub("x", "recoded_column", change))), replace_with, recoded_column))
  } else {
    # If 'x' is NOT in 'change', treat it as a transformation (e.g., "as.factor(recoded_column)")
    df1 <- df1 %>% 
      mutate(recoded_column = eval(parse(text = paste0(change, "(recoded_column)"))))
  }
  
  # Getting numeric differences between original and recoded values (if numeric)
  if (is.numeric(df1$recoded_column) && is.numeric(df2$recoded_column)) {
    recoded_difference <- sum(df1$recoded_column - df2$recoded_column, na.rm = TRUE) # Sums up differences between columns
  } else {
    recoded_difference <- NA # Assigns NA if columns are not numeric
  }
  
  # Double-check logic: if no differences, return "success"; otherwise, suggest further review
  doublecheck <- ifelse(is.na(recoded_difference) || recoded_difference == 0, "Success!! :)", "Explore More")
  
  # Printing results
  cat("\n===========================================\n")
  cat("Column:", col, "\n") # Prints the name of the column being checked
  cat("Antijoin Results (Rows that Were Mutated):\n")
  print(df3)  # Prints only the rows that changed
  
  cat("\nFrequency Tables Pre & Post Recode:\n")
  print(frequent1) # Prints the original frequency table
  print(frequent2) # Prints the recoded frequency table
  
  cat("\nRecoded Sum Difference:", recoded_difference, "\n") # Prints the total sum difference
  print(doublecheck) # Prints the double-check validation message
  cat("\n===========================================\n")
  
}

#####example syntax-------------------------------------------------------------
#dc_recode(dc1, col= c("adsleep", "adsleep_r"), change= "x >= 14" )
#That is beautiful, the antijoin, frequency tables, & difference between rows post mutation!!


### dc_reverse: Reverse a numeric scale-------------------------------------------
dc_reverse <- function(df, col, reverse, change,replace_with = NA) {
  #getting our dc dataframes
  df <- df %>% select(all_of(col))
  
  df1 <- df[, 1, drop = F]  # Select first column (original)
  df2 <- df[, 2, drop = F]  # Select Second column (recoded)
  
  #creating anti_join, should display values that weren't reverse coded
  colnames(df1) <- "reversed_column"#getting the same column names
  colnames(df2) <- "reversed_column"#getting the same column names
  
  df3<- anti_join(df1, df2)
  #anti_join will match reverse coded and original rows, meaning only values not reverse coded should be displayed
  
  #Getting Frequency Tables
  frequent1 <- janitor::tabyl(df1$reversed_column, show_na = T)#original frequency
  frequent2 <- janitor::tabyl(df2$reversed_column, show_na = T) #recoded frequency
  
  
  #reverse coding original variable
  df1 <- df1 %>% 
    mutate(
      reversed_column = ifelse(
        eval(parse(text = gsub("x", "reversed_column", change))), # replaces "x" in the change condition with the actual column name
        replace_with, # assigns the specified replacement value if condition is met
        reversed_column # keeps the original value if condition is not met
      ),
      reversed_column = reverse - reversed_column # performs reverse coding by subtracting values from the given scale maximum
    )
  
  # getting numeric differences between original and reversed columns
  if (is.numeric(df1$reversed_column) && is.numeric(df2$reversed_column)) { 
    reversed_difference <- sum(df1$reversed_column - df2$reversed_column, na.rm = T) 
    # computes the sum of differences between reversed and expected reversed column, ignoring NA values
  } else { 
    reversed_difference <- NA # assigns NA if the columns are not numeric
  }
  
  # validation check: if sum difference is zero, the reversal was done correctly
  if (reversed_difference == 0) { 
    doublecheck <- "Success!! :)" # indicates that the reverse coding matches expectations
  } else { 
    doublecheck <- "Explore More" # suggests reviewing the process if differences exist
  }
  
  
  cat("\n===========================================\n")
  cat("Column:", col, "\n")
  cat("Antijoin Results (All Rows, No NA's):\n")
  print(df3)  # Only shows rows that changed
  
  cat("\nFrequency Tables Pre & Post Recode:\n")
  print(frequent1) #gives us orignal frequency table
  print(frequent2) #gives us mutated frequency table
  
  cat("\nReversed Sum Difference:", reversed_difference, "\n")
  print(doublecheck)
  cat("\n===========================================\n")
}
#####example syntax-------------------------------------------------------------
#dc_reverse(dc2, col= c("lsatisfy", "lsatisfy_r"), reverse = 5, change= "x >= 5" )

### dc_aggregate: Aggregate multiple columns--------------------------------------
dc_aggregate <- function(df, agg, cols, type = "sum", na = "remove") {
  df1 <- df[, 1] # selects column 1, which is the original column before aggregation
  df2 <- df[, 2] # selects column 2 and beyond, which are the columns to be aggregated
  
  # perform aggregation based on the specified type (sum or mean)
  df <- df %>%
    rowwise() %>% # ensures operations are applied row-wise
    mutate(!!sym(agg) := if (na == "remove") { # dynamically creates the new aggregated column
      if (type == "sum") sum(c_across(all_of(cols)), na.rm = T) # sums selected columns while removing na values
      else if (type == "mean") mean(c_across(all_of(cols)), na.rm = T) # calculates row-wise mean while removing na values
    } else { # case where na values are kept
      if (type == "sum") sum(c_across(all_of(cols))) # sums selected columns without removing na values
      else if (type == "mean") mean(c_across(all_of(cols))) # calculates row-wise mean without removing na values
    }) %>%
    ungroup() # removes row-wise grouping to maintain dataframe structure
  
  # getting numeric differences for double-checking accuracy
  original_values <- df %>% select(all_of(cols)) # selects only the columns that were aggregated
  
  # calculate expected aggregation manually for validation
  expected_agg <- if (type == "sum") rowSums(original_values, na.rm = (na == "remove")) # manually computes sum
  else if (type == "mean") rowMeans(original_values, na.rm = (na == "remove")) # manually computes mean
  
  # compute total difference between computed and expected aggregation
  agg_diff <- sum(abs(df[[agg]] - expected_agg), na.rm = T) # takes absolute differences and sums them
  
  # double-check logic to confirm aggregation accuracy
  doublecheck <- if (agg_diff == 0) "Success!! :)" else "Explore More" # prints success if no difference, otherwise suggests review
  
  # print results for user validation
  cat("\n===========================================\n") # separator for readability
  cat("Aggregated Column:", agg, "\n") # prints the name of the newly created aggregated column
  cat("Aggregation Type:", type, "\n") # prints whether sum or mean was used
  cat("NA Handling:", na, "\n") # prints whether na values were removed or kept
  cat("\nNumeric Difference Between Computed and Expected Aggregation:", agg_diff, "\n") # prints total difference
  print(doublecheck) # prints success message or error suggestion
  cat("\n===========================================\n") # closing separator for readability
}
#####example syntax-------------------------------------------------------------
#dc_aggregate(dc3, "add_sum", cols= c("adpleasr", "addown", "adsleep", "adenergy", "adthink"), type = "sum", na = "remove")

##Final Desc Function----------------------------------------------------------- 
#we want to establish a function similar to initial descriptives, but we don't want charts, final charts need to be highly customized

final_desc <- function(df, columns, df_name = "dataset", plot = "none") {
  df <- as.data.table(df)  # Convert to data.table for efficiency
  results_list <- list()   # To store results for each column
  
  # Function to determine the scale of measurement
  get_possible_scale_measurement <- function(column_data) { #specifying column_data
    if (is.numeric(column_data)) { #making sure the provided data is in numeric form
      unique_vals <- length(unique(na.omit(column_data)))#gets the number of unique values in each column
      
      # If very few unique values, it's likely categorical or ordinal
      if (unique_vals <= 12) { #less than 12 we are likely dealing with some sort of likert scale
        if (all(column_data %% 1 == 0, na.rm = T)) {# if there are no decimals it is also likely a categorical variable
          return("Ordinal (Discrete Categories)")
        } else { #else there are decimals indicating interval level data
          return("Interval (Likely Scale Data)") 
        }
      } #all following variables have more than 12 unique values
      
      # Check if data has a T zero (Ratio Scale)
      min_val <- min(column_data, na.rm = T) # geting the minimum value
      has_negative <- any(column_data < 0, na.rm = T) #seeing if there are negative values, any values less than 0 
      
      if (min_val == 0 && !has_negative) { #if there are more than 12 unique values with a T 0, likely ratio
        return("Ratio (Continuous, T Zero)") 
      } else {
        return("Interval (Continuous, No T Zero)") #with negatives there is likely no T 0 
      }
      
    } else if (is.character(column_data) || is.factor(column_data)) { #if data is not in numeric form, list or likely character
      unique_vals <- length(unique(na.omit(column_data))) #getting the 3 of unique entries in the non numeric data
      if (unique_vals <= 12) { # if there are few unqiues, this is likely categorical or scale values in character format
        return("Nominal (Categorical, Limited Levels)")
      } else { #with many uniques, this is likely character data, with many categories
        return("Nominal (Categorical, Many Levels)")
      }
    } else { # if none of the above conditions are T, there should be further exploration of the raw data
      return("Needs Further Exploration")
    }
  }
  
  for (col in columns) { #for each column in the list of columns provided
    if (!col %in% names(df)) { #this handles misspellings or mistakes in provided columns, while continuing the rest of the column descrptives
      warning(paste("Column", col, "not found in the dataset. Skipping."))
      next #skips this column and moves to the next in the list if it exists
    }
    
    column_data <- df[[col]] #allows us to work with one column at a time,
    
    # Determine structure
    structure_type <- typeof(column_data) #gives us the structure of the data
    
    # Determine scale of measurement
    possible_scale_measurement <- get_possible_scale_measurement(column_data) #uses the above get possible scale measurement fucntion to determine a scale of measurement
    
    # Count NA and Infinites, allows us to determine if data needs to be looked at
    n_na <- sum(is.na(column_data)) #creates a count of NA's
    n_inf <- sum(is.infinite(column_data), na.rm = T) #creates a count of infinites in the data
    
    # Frequency table including NA and Infinites
    freq_table <- as.data.table(table(column_data, useNA = "always")) #gives us a frequency table as a data table
    setnames(freq_table, c("Value", "Frequency")) # provides column names for the data table
    
    # Compute Descriptive Statistics
    n_total <- length(column_data) #gives total # of rows
    n_complete <- n_total - n_na # gives # of rows complete by subtracting the na count
    completion_rate <- n_complete / n_total # gives a % for how often that column has entries
    
    if (is.numeric(column_data)) { #if the column is numeric we can compute many descriptives
      mean_val <- mean(column_data, na.rm = T) #gives p50 a common mct
      median_val <- median(column_data, na.rm = T) #gives median another measure of central tendency (mct)
      mode_val <- as.numeric(names(sort(table(column_data), decreasing = T)[1])) #provides the most common entry, our last, least used mct
      sd_val <- sd(column_data, na.rm = T) #quantifies the variation within the data
      skew_val <- skewness(column_data, na.rm = T) #gives us the skew, numerically showing which way the graph tails off
      kurtosis_val <- kurtosis(column_data, na.rm = T) #shows us how normal of a distribution we have
      percentiles <- quantile(column_data, probs = c(0, 0.05, 0.25, 0.50, 0.75, 0.95, 1), na.rm = T) #gives us every percentile that can likely give us a good picture of the entire data
      iqr_val <- IQR(column_data, na.rm = T) #takes p75-p25 to give us the interquartile range
      
      # Identify outliers
      min_p_outliers <- list(column_data[column_data <= percentiles[2] & !is.na(column_data)])# specifies p05, values near the minimum of the dataset, data below this is likely to contain outliers
      max_p_outliers <- list(column_data[column_data >= percentiles[6] & !is.na(column_data)])# specifies p95, values near the maximum of the dataset, data above this is likely to contain outliers
      
      # Store results in a list
      descriptives <- list( #creates a list to append into a df
        df_name = df_name, column = col, #specifying the column
        structure = structure_type, possible_scale_measurement = possible_scale_measurement,
        n_complete = n_complete, n_missing = n_na, completion_rate = round(completion_rate, 4),
        mean = round(mean_val, 4), median = median_val, mode = mode_val, sd = round(sd_val, 4),
        skew = round(skew_val, 4), kurtosis = round(kurtosis_val, 4),
        min = percentiles[1], p5 = percentiles[2], p25 = percentiles[3], p50 = percentiles[4], 
        p75 = percentiles[5], p95 = percentiles[6], max = percentiles[7], IQR = iqr_val,
        min_p_outliers = min_p_outliers, max_p_outliers = max_p_outliers
      )#above code organizes all descriptives we are interested in and rounds necessary values to the 4th decimal place
      
      # Generate plots if specified
      if (plot != "none") { # checks if the user wants to generate a plot (must not be "none")
        
        # Histogram (allows both "hist" and "histogram" for flexibility)
        if (plot == "hist" || plot == "histogram") {  
          p <- ggplot(df, aes(x = .data[[col]])) +  # specifies which column to use as x-axis
            geom_histogram(bins = 30, fill = "blue", alpha = 0.6) +  # creates a histogram with 30 bins, blue color, and 60% opacity
            ggtitle(paste("Histogram of", col))  # sets the title dynamically using the column name
          
          # Boxplot  
        } else if (plot == "box") {  
          p <- ggplot(df, aes(y = .data[[col]])) +  # specifies which column to use as y-axis
            geom_boxplot(fill = "purple", alpha = 0.6) +  # creates a boxplot with purple fill and 60% opacity
            ggtitle(paste("Boxplot of", col))  # sets the title dynamically using the column name
          
          # Violin Plot  
        } else if (plot == "violin") {  
          p <- ggplot(df, aes(x = "", y = .data[[col]])) +  # specifies column for y-axis, empty x-axis (for single-variable violin plot)
            geom_violin(fill = "green", alpha = 0.6) +  # creates a violin plot with green fill and 60% opacity
            ggtitle(paste("Violin Plot of", col))  # sets the title dynamically using the column name
        }
        
        # Ensures a valid plot exists before modifying & printing
        if (exists("p")) {  
          p <- p + theme_minimal()  # applies a minimalistic theme for a clean look
          print(p)  # displays the plot
        }
      }
      
      
    } else { #for non-numeric data we cannot create plots and there are not many possible descriptives
      mode_val <- names(sort(table(column_data), decreasing = T)[1]) #getting the only mct necessary
      descriptives <- list( #creating a list to append descriptives of the column to
        df_name = df_name, column = col, #specifying column
        structure = structure_type, possible_scale_measurement = possible_scale_measurement,
        n_complete = n_complete, n_missing = n_na, completion_rate = round(completion_rate, 4),
        mode = mode_val, min_p_outliers = NA, max_p_outliers = NA
      ) #specifies the necessary columns
    }
    
    # Save results to list
    results_list[[paste0(df_name, "_", col)]] <- descriptives
    
    # Print results
    cat("\n===========================================\n")
    cat("Column:", col, "\n")
    cat("Structure:", structure_type, "\n")
    cat("Scale of Measurement:", possible_scale_measurement, "\n")
    cat("NA's:", n_na, " | Infinites:", n_inf, "\n\n")
    
    cat("Frequency Table:\n")
    print(freq_table)
    
    cat("\nDescriptive Statistics:\n")
    for (stat in names(descriptives)[3:21]) {  # Exclude df_name, column, and outliers from print loop
      cat(sprintf("%-15s: %s\n", stat, descriptives[[stat]]))
    }
    
    cat("===========================================\n")
  }
  
  # Convert results list to a data.table
  results_df <- rbindlist(lapply(results_list, as.data.table), fill = T)
  
  # Dynamically name the output dataframe
  output_var_name <- paste0(df_name, "_final_descriptives")
  
  # Assign the results to a dynamically named variable in the global environment
  assign(output_var_name, results_df, envir = .GlobalEnv)
}
#####example syntax-------------------------------------------------------------
#final_desc(raw_data_bfss, c("state", "fmonth", "genhlth","age","lsatisfy"), df_name= "bfss", plot = "none")


print("Welcome to psyfiftythirty functions :)")


             
             
             
             
             
             
