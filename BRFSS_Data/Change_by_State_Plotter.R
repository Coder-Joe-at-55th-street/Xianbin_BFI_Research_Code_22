library("readxl")
library("dplyr")
library("haven") #To export DTA
library(tidyr)
library(cdlTools) # for FIPS code
library(ggplot2) # I surrender. I'll use GGPLot. to hell with Base R.
library(gridExtra)

### This file were supposed to create some plots. Same content as
### change_by_state_graphic_reproduction.RMD.

#### THIS SCRIPT WAS NOT SUPPOSED TO BE RUN IN FULL

### INSTEAD, YOU ARE ENCOURAGED TO USE THE FUNCTIONS TO CREATE YOUR OWN PLOT

### I'll make comments so you can use it easier.


## Read the data
## This should be compiled BRFSS data from '99 to '20
Data = read.csv(file = "./BRFSS_Data_Processed_Folder/Processed_BRFSS_1999_to_2020.csv", stringsAsFactors = FALSE)


# Specifics: In this dataset, only the specific subdivision of occupations, such as dentist, general, family practitioners, were recorded
BLS_Data_Specifics = read.csv(file = "./BRFSS_Data/Wages_By_State.csv", 
                              stringsAsFactors = FALSE)

# General: In this dataset, general division of occupations, such as dentist, all, and physicians and surgeons, all, were recorded as well.
BLS_Data_General = read.csv(file = "./BRFSS_Data/Employment_By_State.csv", 
                            stringsAsFactors = FALSE)

###
### Function. Input some data and a variable name
### Then find its average or sum over two periods, state-by-state
### Useful for plotting later on.

By_Year_Operation = function(Input_Data, Var_Name, 
                             Year_Var_Name, 
                             State_Var_Name, Sum_or_Average = "Average",
                             first_period = c(1999, 2004), 
                             second_period = c(2014, 2019)){
  
  temp1 = Input_Data %>%
    filter(get(Year_Var_Name) >= first_period[1] & 
             get(Year_Var_Name) <= first_period[2])
  
  temp2 = Input_Data %>%
    filter(get(Year_Var_Name) >= second_period[1] & 
             get(Year_Var_Name) <= second_period[2])
  ### Filter the data into two parts,
  ### Each only taking from the year designated by 
  ### first_period and second_period
  
  if(Sum_or_Average == "Average"){
    Var_Name_1 = paste(Var_Name, "_Average_",
                       first_period[1], "_", first_period[2],
                       sep = "")
    ### Designate variable names for the first period.
    ### Format: Original_Var_Name_Average_begin_year_end_year.
    
    Var_Name_2 = paste(Var_Name, "_Average_",
                       second_period[1], "_", second_period[2],
                       sep = "")
    
    temp1 = temp1 %>% 
      group_by_at(State_Var_Name) %>%
      summarise_at(Var_Name, mean, na.rm = TRUE) %>%
      rename_at(Var_Name, 
                ~Var_Name_1)
    
    ### For each state, find the mean of the variable
    ### Designated by input Var_Name. This dataset,
    ### the temp1, shall consist only entries from the first period,
    ### Thus directly summarising would be of no problem.
    ### The variable would be renamed for later use.
    
    temp2 = temp2 %>% 
      group_by_at(State_Var_Name) %>%
      summarise_at(Var_Name, mean, na.rm = TRUE) %>%
      rename_at(Var_Name, 
                ~Var_Name_2)
    
    ### Same for second part (for second period)
    
  }else if(Sum_or_Average == "Sum"){
    Var_Name_1 = paste(Var_Name, "_Sum_",
                       first_period[1], "_", first_period[2],
                       sep = "")
    
    Var_Name_2 = paste(Var_Name, "_Sum_",
                       second_period[1], "_", second_period[2],
                       sep = "")
    
    temp1 = temp1 %>% 
      group_by_at(State_Var_Name) %>%
      summarise_at(Var_Name, sum, na.rm = TRUE) %>%
      rename_at(Var_Name, 
                ~Var_Name_1)
    
    temp2 = temp2 %>% 
      group_by_at(State_Var_Name) %>%
      summarise_at(Var_Name, sum, na.rm = TRUE) %>%
      rename_at(Var_Name, 
                ~Var_Name_2)
    
    ### Doing the same thing except changing Average to Sum
  }
  
  temp = left_join(temp1, temp2, by = State_Var_Name)
  ## Merge the dataset, by State_Var_Name, the name 
  ## for the variable which shows which state the entry represents.
  
  temp = as.data.frame(temp)
  
  temp$Diff_Ratio = (temp[, Var_Name_2] - temp[, Var_Name_1]) /
    temp[, Var_Name_1]
  ### Find the relative difference.
  ### Formula: (Result of second period - Result first period)/Res. 2nd period
  
  
  temp$State_FIPS_Code = fips(temp[, State_Var_Name], to = "fips")
  ### Convert whatever state_var you used into FIPS code, for easier plotting.
  
  
  return(temp)
  
}

#### BRFSS Data processing, copied from BRFSS_Analyzer_Plotter.R

Data_Range_Checker = function(Input_Data, Var){
  temp = Input_Data %>% group_by(STATE_FIPS_CODE, YEAR_RESPONSE) %>%
    summarize(Temp1 = across({{ Var }}, sum, na.rm = TRUE)) %>%
    mutate(Temp1 = as.numeric(unlist(Temp1)))
  temp = temp[temp$Temp1 > 0, ]
  return(temp[, 1:2])
}

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


#### Plotting Function

State_Change_Plotter = function(Input_Data, Change_Var_Name,
                                Var_Name_1,
                                Var_Name_2, State_Var_Name,
                                Title_Name, label_size = 5,
                                label_x_shift = 0,
                                label_y_shift = -.015){
  
  Input_Data = as.data.frame(Input_Data) ## To prevent problems
  
  if(!("Diff_Ratio" %in% colnames(Input_Data))){
    temp$Diff_Ratio = (temp[, Var_Name_2] - temp[, Var_Name_1]) /
      temp[, Var_Name_1]
    ### Find the ratio of difference between two variables to be plotted
    ### And compared, if not already done earlier.
  }
  if(!("State_FIPS_Code" %in% colnames(Input_Data))){
    temp$State_FIPS_Code = fips(Input_Data[, State_Var_Name],
                                to = "fips")
    ### Find the FIPS code if not done already.
  }
  
  ggplot(data = Input_Data,
         aes(x = reorder(State_FIPS_Code, -get(Var_Name_1))) ) +
    ## The base of the plot.
    ## the reorder plot shall re-order State_FIPS_Code value at Decreasing
    ## order of the values in Var_Name_1. The get() function was used
    ## because Var_Name began as a string and cannot be read directly 
    ## by aes.
    
    geom_point(aes_string(y = Var_Name_1), col = "cyan") +
    ### Get the Y-coordinate of some of the scatterplot.
    ## Namely, the ones represented by Var_Name_1, colorded Cyan.
    ## Here, there were no get() function because we are using
    ## aes_string, which can have string input for its parameters,
    ## should such string refers to one of the variable names.
    
    
    geom_point(aes_string(y = Var_Name_2), col = "red") +
    ## Another point, for values in Var_Name_2
    ## The X-coordinate were not defined here so should just inherit from
    ## the first ggplot function.
    
    labs(y = Change_Var_Name,
         x = "Reordered State FIPS code, Sorted by Ave. Ratio in period one",
         color = "Inc/Dec") +
    ### X and Y axis name, and label for the color of the segment.
    
    geom_segment(aes(x = reorder(State_FIPS_Code, -get(Var_Name_1)), 
                     xend = reorder(State_FIPS_Code, -get(Var_Name_1)),
                     y = get(Var_Name_1), yend = get(Var_Name_2),
                     col = Diff_Ratio)) +
    ### Okay so here we have the segmented plot.
    ### The segement connects (x,y) and (xend, yend)
    ### X-coordinate just inherit from previously, using same formula.
    ### X-end should be same as x, as written above.
    ### y-coordinates and yend should be values of Var_Name_1
    ### and Var_Name_2 respectively. Those are also coordinates
    ### of the two plots we made earlier.
    ### The color depends on diff_ratio.
    
    geom_text(label = Input_Data[, State_Var_Name], 
              aes(x = reorder(State_FIPS_Code, -get(Var_Name_1)), 
                  y = get(Var_Name_1)),
              nudge_x = label_x_shift, 
              nudge_y = label_y_shift,
              size = label_size) +
    ### Label the text with coordinates for points showing values of Var_Name_1
    ### nudge the label as given in input
    ### label shall be the state_var_name
    
    ggtitle(Title_Name)
  ### Add the title
}


### Example: BLS Data, 
### change of dental hygienist per 1,000 people from averages of '99 to '04 to average of '14 to '19


BLS_Data_General_For_Plotting = 
  BLS_Data_General[BLS_Data_General$occ_code == 2021, ]
### 29-2021 is the BLS code for dental hygienists.
### This includes only dental hygienist in the dataset

BLS_Data_General_For_Plotting$Dtl_Hyg_per_1K_Pop = 
  BLS_Data_General_For_Plotting$tot_employment /
  BLS_Data_General_For_Plotting$Est_Population * 1000
### Find the dental hygienist per 1,000 people data, for each state-year
### Named as Dtl_Hyg_per_1K_Pop

BLS_Data_General_For_Plotting = 
  By_Year_Operation(Input_Data = BLS_Data_General_For_Plotting,
                    Var_Name = "Dtl_Hyg_per_1K_Pop",
                    Year_Var_Name = "year",
                    State_Var_Name = "st")
### Process the data and find averages over two period
### of variable Dtl_Hyg_per_1K_Pop.
### Used default 1999 to 2004 average and 2014 to 2019 average.

State_Change_Plotter(Input_Data = BLS_Data_General_For_Plotting,
                     Change_Var_Name = "Dental Hygienist per 1,000 people",
                     Var_Name_1 = "Dtl_Hyg_per_1K_Pop_Average_1999_2004",
                     Var_Name_2 = "Dtl_Hyg_per_1K_Pop_Average_2014_2019",
                     State_Var_Name = "st", 
                     Title_Name = "Dental Hygienist per 1,000 people \n
                     Change in 5-year average",
                     label_size = 2)
### Plotting, with self-designated title, Y-label, etc.


#### Example: BRFSS Data, portion of people with health plan

BRFSS_Example_1 = Data_Condensor(
  Input_Data = Data, Var1 = "HEALTH_PLAN_STATUS", 
  Var1_Condition = 1, Rename_Columns = TRUE)

BRFSS_Example_1$st = fips(BRFSS_Example_1$STATE_FIPS_CODE, to = "abbreviation")

BRFSS_Example_1 = By_Year_Operation(Input_Data = BRFSS_Example_1,
                                    Var_Name = "HEALTH_PLAN_STATUS_Ratio",
                                    Year_Var_Name = "YEAR_RESPONSE",
                                    State_Var_Name = "st")

State_Change_Plotter(Input_Data = BRFSS_Example_1,
                     Change_Var_Name = "Portion of People with Health Plan",
                     Var_Name_1 = colnames(BRFSS_Example_1)[2],
                     Var_Name_2 = colnames(BRFSS_Example_1)[3],
                     Title_Name = "Change of Portion of People with health plan, 
                     \n Average of '99 to '04 and of '14 to '19",
                     State_Var_Name = "st", label_size = 2)


#### Another function: Scatterplot for two periods.
####  dental hygienist and dentist per 1,000 people in two periods.

BLS_temp_1 = BLS_Data_General[BLS_Data_General$occ_code == 1020, ]

BLS_temp_1$Dentist_per_1K_Pop = 
  BLS_temp_1$tot_employment / BLS_temp_1$Est_Population * 1000
### Find the dentist per 1,000 people data, for each state-year
### Named as Dentist_per_1K_Pop

BLS_temp_1 = 
  By_Year_Operation(Input_Data = BLS_temp_1,
                    Var_Name = "Dentist_per_1K_Pop",
                    Year_Var_Name = "year",
                    State_Var_Name = "st")

BLS_temp_2 = BLS_Data_General[BLS_Data_General$occ_code == 2021, ]

BLS_temp_2$Dtl_Hyg_per_1K_Pop = 
  BLS_temp_2$tot_employment / BLS_temp_2$Est_Population * 1000
### Find the dental hygienist per 1,000 people data, for each state-year
### Named as Dtl_Hyg_per_1K_Pop

BLS_temp_2 = 
  By_Year_Operation(Input_Data = BLS_temp_2,
                    Var_Name = "Dtl_Hyg_per_1K_Pop",
                    Year_Var_Name = "year",
                    State_Var_Name = "st")

BLS_Data_General_For_Plotting = left_join(BLS_temp_1, BLS_temp_2,
                                          by = "st")



## Plot them.
A = ggplot(data = BLS_Data_General_For_Plotting,
           aes(x = Dentist_per_1K_Pop_Average_1999_2004, 
               y = Dtl_Hyg_per_1K_Pop_Average_1999_2004)) +
  ### Here, you can actually change to whatever dataset with two variables
  ### Just like any ggplot plot.
  
  geom_point() +
  ### To plot scatterplot.
  
  labs(y = "Dental Hygienist per Dentist",
       x = "Number of Dentist per 1,000 people") +
  ### Add labels for X and Y axis
  
  geom_text(label = BLS_Data_General_For_Plotting$st, 
            aes(x = Dentist_per_1K_Pop_Average_1999_2004, 
                y = Dtl_Hyg_per_1K_Pop_Average_1999_2004),
            nudge_x = 0, nudge_y = .1) +
  ### Add labels to each data point.
  ### The label = argument will be shown at places designated by x-y coordinates
  ### in aes, and the text shown is what was take in label = argument.
  ### The label should be a column from the dataset which were used for plotting 
  ### The X-Y coordinates were designated in aes(), using the same as 
  ### What the plot began with, though you can change it.
  
  xlim(0, .6) + ylim(0, 1.3) +
  ### It is advised to fix the X and Y limit if 
  ### you want to have sensible comparison
  
  geom_smooth(method = "lm") +
  ### Add a regression line.
  ggtitle("Dental Hygienist and Dentist per 1,000 people, \n
          Averaged '99 to '04")

B = ggplot(data = BLS_Data_General_For_Plotting,
           aes(x = Dentist_per_1K_Pop_Average_2014_2019, 
               y = Dtl_Hyg_per_1K_Pop_Average_2014_2019)) +
  geom_point() +
  labs(y = "Dental Hygienist per Dentist",
       x = "Number of Dentist per 1,000 people") +
  geom_text(label = BLS_Data_General_For_Plotting$st, 
            aes(x = Dentist_per_1K_Pop_Average_2014_2019, 
                y = Dtl_Hyg_per_1K_Pop_Average_2014_2019),
            nudge_x = 0, nudge_y = .1) +
  xlim(0, .6) + ylim(0, 1.3) +
  geom_smooth(method = "lm") +
  ggtitle("Dental Hygienist and Dentist per 1,000 people, \n
          Averaged '14 to '19")

grid.arrange(A, B, ncol = 2)
