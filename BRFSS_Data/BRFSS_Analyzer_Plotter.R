library("readxl")
library("dplyr")
library("haven") #To export DTA
library(tidyr)
library(cdlTools) # for FIPS code
library(ggplot2) # I surrender. I'll use GGPLot. to hell with Base R.
library(gridExtra)

#### THIS SCRIPT WAS NOT SUPPOSED TO BE RUN IN FULL

### INSTEAD, YOU ARE ENCOURAGED TO USE THE FUNCTIONS TO CREATE YOUR OWN PLOT

### I'll make comments so you can use it easier.


## Read the data
## This should be compiled BRFSS data from '99 to '20
Data = read.csv(file = "./BRFSS_Data_Processed_Folder/Processed_BRFSS_1999_to_2020.csv", stringsAsFactors = FALSE)


## Function. Input: Data frame, and STRINGS representing a variable.
## Output: A dataset, which includes the year-state in which the said
## variable is available.
Data_Range_Checker = function(Input_Data, Var){
  temp = Input_Data %>% group_by(STATE_FIPS_CODE, YEAR_RESPONSE) %>%
    summarize(Temp1 = across({{ Var }}, sum, na.rm = TRUE)) %>%
    mutate(Temp1 = as.numeric(unlist(Temp1)))
  temp = temp[temp$Temp1 > 0, ]
  return(temp[, 1:2])
}


# This is a function that calculates sums and averages of data.
# Here's a list of inputs:
# Input_Data: The dataframe to be used. Usually just use Data, which was read from BRFSS_99_20
# Var1: String, variable name for the First Variable to be calculated
# Var2: String, variable name for Second Variable to be Calculated. Default NA. If NO input, then
# Only Var1 would be cauculated
# Var1 Condition, Var1 Denominator Values:
# This is used for calculation. Formula: For each state-year, calculate:
# (no. of entries fitting Var1_Condition) / (No of Entries fitting Var1_Denominator_Values)
# By "Fitting X", it means that the value of the entry is contained in X. Thus, These conditions can be a vector.
# Var1 Denominator Values is default to have NA.
# If there's any NA in Var1 Denominator Value, includeing NA in a vector or it was not inputted,
# The Denominator Value changes to c(1:6, 8) Since 
# The BRFSS Dataset usually have 7 as unsure and 9 as refused to answer.


# Var2 Condition, Var2 Denominator Values:
# Same. Default NA. NOT used if Var2 is also NA.
# Warning: Not specifying value of Var2 Condition with a proper Var2 input results in error.

# Num or Ratio: if input is "ratio", after calculating enumerator and denominator above,
# It calculates the ratio.
# If there's ANY input other than "ratio", then the ratio is NOT calculated

# Rename Columns: if TRUE, then Columns will be renamed starting with values you put in Var1 and Var2
# Otherwise, it will be left as Var1_Ratio, Var1_Fit_Condition_Count etc.

# Years begin, years end: specify the year range. Default is '99 to '20
Data_Condensor = function(Input_Data, Var1, Var2 = NA,
                          Var1_Condition, Var1_Denominator_Values = NA,
                          Var2_Condition = NA, Var2_Denominator_Values = NA,
                          Num_or_Ratio = "Ratio", Rename_Columns = FALSE,
                          year_begin = 1999, year_end = 2020){
  
  
  temp = Input_Data %>%
    filter(YEAR_RESPONSE >= year_begin & YEAR_RESPONSE <= year_end)
  ### Filter the data with year range
  
  year_List_1 = Data_Range_Checker(temp, Var1)
  ### This year_list_1 tells us which state-year these data would be available
  
  if(!is.na(Var2)){
    ### If var2 is not NA, meaning that there is an entry, also check the range
    ### for var2.
    year_List_2 = Data_Range_Checker(temp, Var2)
    
    ### Left-Join it with year range for Var1. 
    ### Keeps only common elements, so that year-state left here is available
    ### All the year.
    year_state_List = inner_join(year_List_1, year_List_2)
    
  }else{
    year_state_List = year_List_1
  }
  
  temp = inner_join(temp, year_state_List)
  ###Keer only state-year that have got available data
  
  if(TRUE %in% is.na(Var1_Denominator_Values)){
    # No entry is for Var1_Denominator_Values. Use Default Setting.
    # Contain everything that ain't not Don't Know, Unsure, or refused.
    # According to codebook, it shall be 1:9 without 7 and 9.
    Var1_Denominator_Values = c(1:6, 8)
  }
  
  # Sum up by state-year, drop those with too small a sample size.
  First_Var_Result = mutate(temp, 
                            Var1_Mask = ifelse(temp[, Var1] %in% Var1_Condition, 
                                               1, 0)) %>% 
    ### Gives 1 to a temporary variable if the Var1
    ### Fits in condition
    mutate(temp, Var1_Denominator_Count = 
             ifelse(temp[, Var1] %in% Var1_Denominator_Values, 1, 0)) %>%
    ### Give 1 to a temporary variable if Var1 fits in
    ### Denominator Value.
    group_by(STATE_FIPS_CODE, YEAR_RESPONSE) %>% 
    summarize(Var1_Fit_Condition_Sum = sum(Var1_Mask, na.rm = TRUE), 
              Var1_Total = sum(Var1_Denominator_Count, na.rm = TRUE)) %>%
    ### Sum the previous temporary variables by state-year 
    ### And write them in two variables for use later on.
    filter(Var1_Fit_Condition_Sum >= 100)
  
  if(!is.na(Var2)){
    # Var2 is default NA.
    # If anything was put onto Var2, then this chunk shall run, doing the same thing as above.
    
    if(TRUE %in% is.na(Var2_Denominator_Values)){
      Var2_Denominator_Values = c(1:6, 8)
    }
    Second_Var_Result = temp %>% 
      mutate(Var2_Mask = ifelse(temp[, Var2] %in% Var2_Condition, 1, 0)) %>% 
      mutate(temp, Var2_Denominator_Count = 
               ifelse(temp[, Var2] %in% Var2_Denominator_Values, 1, 0)) %>%
      group_by(STATE_FIPS_CODE, YEAR_RESPONSE) %>% 
      summarize(Var2_Fit_Condition_Sum = sum(Var2_Mask, na.rm = TRUE), 
                Var2_Total = sum(Var2_Denominator_Count, na.rm = TRUE)) %>%
      filter(Var2_Fit_Condition_Sum >= 100)
    
    ### Re-write temp dataframe as inner-join of the 
    ### result from calculating two variables
    temp = inner_join(First_Var_Result, Second_Var_Result)
  }else{
    ### No variable 2. Use only variable 1 result.
    temp = First_Var_Result
  }
  
  
  if(Num_or_Ratio == "Ratio"){
    # Find ratios for var1.
    temp = temp %>% mutate(Var1_Ratio = Var1_Fit_Condition_Sum / Var1_Total)
    # rename, if Rename_Columns was TRUE.
    if(Rename_Columns){
      temp = temp %>% 
        rename_at(vars(c("Var1_Fit_Condition_Sum", "Var1_Total", "Var1_Ratio")),
                  ~ c(paste(Var1, "_Fit_Condition_Count", sep = ""),
                      paste(Var1, "_Total", sep = ""),
                      paste(Var1, "_Ratio", sep = "")))
    }
    if(!is.na(Var2)){
      # Do the same thing for Var2, if it was calculated.
      temp = temp %>% 
        mutate(Var2_Ratio = Var2_Fit_Condition_Sum / Var2_Total)
      if(Rename_Columns){
        temp = temp %>% 
          rename_at(vars(c("Var2_Fit_Condition_Sum", 
                           "Var2_Total", "Var2_Ratio")),
                    ~ c(paste(Var2, "_Fit_Condition_Count", sep = ""),
                        paste(Var2, "_Total", sep = ""),
                        paste(Var2, "_Ratio", sep = "")))
      }
    }
  }
  else{
    ### No Ratio Calculated. Do Rename here.
    if(Rename_Columns){
      temp = temp %>% 
        rename_at(vars(c("Var1_Fit_Condition_Sum", "Var1_Total")), 
                  ~ c(paste(Var1, "_Fit_Condition_Count", sep = ""),
                      paste(Var1, "_Total", sep = "")))
    }
    if(!is.na(Var2)){
      temp = temp %>% mutate(Var2_Ratio = Var2_Fit_Condition_Sum / Var2_Total)
      if(Rename_Columns){
        temp = temp %>% 
          rename_at(vars(c("Var2_Fit_Condition_Sum", "Var2_Total")), 
                    ~ c(paste(Var2, "_Fit_Condition_Count", sep = ""),
                        paste(Var2, "_Total", sep = "")))
      }
    }
  }
  return(temp)
}


# For example, this line gives a dataset, of year-state level, on people who have a health insurance plan.

temp = Data_Condensor(Input_Data = Data, Var1 = "HEALTH_PLAN_STATUS", Var1_Condition = 1, Var2 = "DENTIST_VISIT_LAST", 
                      Var2_Condition = 1, Rename_Columns = TRUE)


# This function plots scatterplot of two ratios included in Input_Data
# It automatically find columns with "_Ratio" at the last
# X-axis is the first variable ended with _Ratio, Y-axis the second.

Ratio_Scatter_Plotter_1 = function(Input_Data, reverse_xy_axis = FALSE){
  ### Retrieve the list of variables needed for plotting.
  List_of_Plotting_Variables = 
    colnames(Input_Data)[which(grepl("_Ratio", colnames(Input_Data)))]
  
  ### Record the variable names
  Var1 = List_of_Plotting_Variables[1]
  Var2 = List_of_Plotting_Variables[2]
  
  if(reverse_xy_axis){
    ### Reverse X-Y axis if needed
    Var1 = List_of_Plotting_Variables[2]
    Var2 = List_of_Plotting_Variables[1]
  }
  
  ### Plotting.
  ggplot(data = Input_Data,
         aes_string(x = Var1, 
                    y = Var2,
                    color = "YEAR_RESPONSE")) +
    geom_point() +
    ggtitle("Comparing two ratios within BRFSS Dataset 
            \n Averaged Over Years for All States") + 
    geom_smooth(method='lm')
  ### Make a regression at the end.
  
}

# This is also a plot, but
# It plot calculates the Averages based on year or state.
# By putting "return_average_of" as "year", the plot shows the averages over years of all states,
# It then plot a scatterplot with averages as in previous function.
# Putting "state" returns averages over state
# Putting "both" returns both plot, side by side.
# Same_Scale is useful ONLY WHEN you put return_average_of = "both".
# By putting TRUE in Same_Scale, you set same scale for side-by-side plots. Default is TRUE
# xlim and ylim were used to fix the scale. Default is .2 to 1.



Ratio_Scatter_Plotter_2 = function(Input_Data, 
                                   return_average_of = "year", 
                                   reverse_xy_axis = FALSE,
                                   same_scale = TRUE, 
                                   xlim = c(.2, 1), ylim = c(.2, 1),
                                   vertical_or_horizontal_arrange = "horizontal"){
  
  List_of_Plotting_Variables = 
    colnames(Input_Data)[which(grepl("_Ratio", colnames(Input_Data)))]
  ## Find the list of variables to be plotted
  
  Var1 = List_of_Plotting_Variables[1]
  Var2 = List_of_Plotting_Variables[2]
  
  if(reverse_xy_axis){
    Var1 = List_of_Plotting_Variables[2]
    Var2 = List_of_Plotting_Variables[1]
  }
  ### Designate the variables for X and Y axis respectively,
  ### Which are first and second variables ended in "_Ratio".
  ### Reversed if reverse_xy_axis == TRUE.
  
  ### Get the year range of data.
  ### Since input should be data cleaned by Data_Condensor,
  ### Every year should see data fully available.
  Year_Range = unique(Input_Data$YEAR_RESPONSE)
  
  # Do averages
  if(return_average_of == "state"){
    # For state, find average over years.
    
    Input_Data =  Input_Data %>% group_by(STATE_FIPS_CODE) %>%
      summarize("Var1_Ave_Over_Years" = across({{ Var1 }}, mean), 
                "Var2_Ave_Over_Years" = across({{ Var2 }}, mean)) %>%
      ### Find mean over years, for each state.
      
      mutate(Var1_Ave_Over_Years = as.numeric(unlist(Var1_Ave_Over_Years)), 
             Var2_Ave_Over_Years = as.numeric(unlist(Var2_Ave_Over_Years))) %>%
      ### Use numeric to prevent formatting problems
      
      rename_at(vars(c("Var1_Ave_Over_Years", "Var2_Ave_Over_Years")),
                ~ c(paste(Var1, "_Ave_Over_Years", sep = ""),
                    paste(Var2, "_Ave_Over_Years", sep = ""))) %>%
      ### Rename the variables with character Var1 and Var2 carries
      ### Prevent confusion later on
      
      mutate(State_Abb = fips(STATE_FIPS_CODE, to =  "Abbreviation")) %>%
      ### Change State FIPS code to abbreviations.
      
      filter(STATE_FIPS_CODE <= 56) 
    # No more Puerto Rico, Virgin Islands, and Guam
    
    ### Conduct the plotting.
    ggplot(data = Input_Data,
           aes_string(x = paste(Var1, "_Ave_Over_Years", sep = ""), 
                      y = paste(Var2, "_Ave_Over_Years", sep = "") )) +
      geom_point() +
      geom_text(label = Input_Data$State_Abb,
                nudge_x = -.002, nudge_y = .001) +
      ggtitle("Comparing two ratios within BRFSS Dataset 
              \n Averaged Over Years for All States") + 
      geom_smooth(method='lm')
    
  }else if(return_average_of == "year"){
    # Average by year, over states.
    Input_Data =  Input_Data %>% 
      filter(STATE_FIPS_CODE <= 56) %>% 
      # No more Puerto Rico, Virgin Islands, and Guam
      
      group_by(YEAR_RESPONSE) %>%
      summarize("Var1_Ave_Over_States" = across({{ Var1 }}, mean), 
                "Var2_Ave_Over_States" = across({{ Var2 }}, mean)) %>%
      ## Make Average over states for each year
      
      mutate(Var1_Ave_Over_States = as.numeric(unlist(Var1_Ave_Over_States)), 
             Var2_Ave_Over_States = as.numeric(unlist(Var2_Ave_Over_States))) %>%
      
      rename_at(vars(c("Var1_Ave_Over_States", "Var2_Ave_Over_States")),
                ~ c(paste(Var1, "_Ave_Over_States", sep = ""),
                    paste(Var2, "_Ave_Over_States", sep = "")))
    
    ## Plot with a regression line
    ggplot(data = Input_Data,
           aes_string(x = paste(Var1, "_Ave_Over_States", sep = ""), 
                      y = paste(Var2, "_Ave_Over_States", sep = "") )) +
      geom_point() +
      geom_text(label = Input_Data$YEAR_RESPONSE,
                nudge_x = -.002, nudge_y = .001) +
      ggtitle("Comparing two ratios within BRFSS Dataset \n Averaged Over State for All Years") + 
      geom_smooth(method='lm')
  }else if(return_average_of == "both"){
    # Plot both stuff above side-by-side.
    
    Part1 = Input_Data
    Part2 = Input_Data
    
    # Average over States. Generally the same as above.
    Part1 = Part1 %>% group_by(STATE_FIPS_CODE) %>%
      summarize("Var1_Ave_Over_Years" = across({{ Var1 }}, mean), 
                "Var2_Ave_Over_Years" = across({{ Var2 }}, mean)) %>%
      mutate(Var1_Ave_Over_Years = as.numeric(unlist(Var1_Ave_Over_Years)), 
             Var2_Ave_Over_Years = as.numeric(unlist(Var2_Ave_Over_Years))) %>%
      rename_at(vars(c("Var1_Ave_Over_Years", "Var2_Ave_Over_Years")),
                ~ c(paste(Var1, "_Ave_Over_Years", sep = ""),
                    paste(Var2, "_Ave_Over_Years", sep = ""))) %>%
      mutate(State_Abb = fips(STATE_FIPS_CODE, to =  "Abbreviation")) %>%
      filter(STATE_FIPS_CODE <= 56) # No more Puerto Rico, Virgin Islands, and Guam
    
    # Average over years. Also generally the same as above
    Part2 = Part2 %>% 
      filter(STATE_FIPS_CODE <= 56) %>% 
      # No more Puerto Rico, Virgin Islands, and Guam
      
      group_by(YEAR_RESPONSE) %>%
      summarize("Var1_Ave_Over_States" = across({{ Var1 }}, mean), 
                "Var2_Ave_Over_States" = across({{ Var2 }}, mean)) %>%
      mutate(Var1_Ave_Over_States = as.numeric(unlist(Var1_Ave_Over_States)), 
             Var2_Ave_Over_States = as.numeric(unlist(Var2_Ave_Over_States))) %>%
      rename_at(vars(c("Var1_Ave_Over_States", "Var2_Ave_Over_States")), 
                ~ c(paste(Var1, "_Ave_Over_States", sep = ""),
                    paste(Var2, "_Ave_Over_States", sep = "")))
    
    Part1_Plot = ggplot(data = Part1,
                        aes_string(x = paste(Var1, "_Ave_Over_Years", sep = ""), 
                                   y = paste(Var2, "_Ave_Over_Years", sep = "") )) +
      geom_point() +
      geom_text(label = Part1$State_Abb,
                nudge_x = -.002, nudge_y = .001) +
      ggtitle("Comparing two ratios within BRFSS Dataset 
              \n Averaged Over Years for All States") + 
      geom_smooth(method='lm')
    
    
    Part2_Plot = ggplot(data = Part2,
                        aes_string(x = paste(Var1, "_Ave_Over_States", sep = ""), 
                                   y = paste(Var2, "_Ave_Over_States", sep = "") )) +
      geom_point() +
      geom_text(label = Part2$YEAR_RESPONSE,
                nudge_x = -.002, nudge_y = .001) +
      ggtitle("Comparing two ratios within BRFSS Dataset \n Averaged Over State for All Years") + 
      geom_smooth(method='lm')
    
    if(same_scale){
      ### If using same scales, rearrange the limits to same scales
      Part1_Plot = Part1_Plot + xlim(xlim[1], xlim[2]) + ylim(ylim[1], ylim[2])
      Part2_Plot = Part2_Plot + xlim(xlim[1], xlim[2]) + ylim(ylim[1], ylim[2])
    }
    if(vertical_or_horizontal_arrange == "horizontal"){
      grid.arrange(Part1_Plot, Part2_Plot, ncol = 2)
      
    }else if(vertical_or_horizontal_arrange == "vertical"){
      grid.arrange(Part1_Plot, Part2_Plot, nrow = 2)
    }
  }
}

## These two are list of variables you can use for exploratory analysis.
## See codebook for more specific values used for Conditions.

XVar_List = c("ANNUAL_INCOME_HSHLD", "HEALTH_PLAN_STATUS", "MED_COST", "CHECKUP_MOST_RECENT")
YVar_List = c("DENTIST_VISIT_LAST", "NO_TEETH_RMVD", "DENTIS_CLEAN_LAST")

# Example usage

Ratio_Scatter_Plotter_2( Data_Condensor(Input_Data = Data,
                                        Var1 = XVar_List[1],
                                        Var2 = YVar_List[2],
                                        Var1_Condition = c(7:8),
                                        Var1_Denominator_Values = c(1:8),
                                        Var2_Condition = 8, 
                                        Var2_Denominator_Values = c(1:6, 8),
                                        Rename_Columns = TRUE), return_average_of = "both",
                         xlim = c(.28, .6),
                         ylim = c(.32, .58))

