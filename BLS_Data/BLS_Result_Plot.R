library("readxl")
library("dplyr")
library("haven") #To export DTA
library("tidyr")

#result = read.csv("Wages_By_MSA.csv", stringsAsFactors = FALSE)

# In order to run this file, make sure Employment_By_State.csv are in the same folder.
# It should be an output of Data_Compiler_State.R

Employment_Data = read.csv("Employment_By_State.csv")


#####
# Part 1: job/population ratio extractor


Ratio_Extractor = function(First_Occupation, Second_Occupation, Input_Data,  
                           Begin_Year_1 = 2009, End_Year_1 = 2011,
                           Begin_Year_2 = 2014, End_Year_2 = 2016,
                           plot = 1, give_processed_data = 0){
  # This function processes data and plot them
  # Input: dataframe, just use "Employment Data"
  # Output: a plot and/or a processed data
  # Formula: For each occupation, define:
  # Ave_job_ratio = sum(employment by year) / sum(population by year), summing for all year in a period
  # job_ratio_growth = Ave_job_ratio in period 2 / Ave_job_ratio in period 1
  # This function then plots the result for both occupations, for each state, as scatterplot.
  
  temp = Input_Data
  temp = temp[temp$job_type == First_Occupation | temp$job_type == Second_Occupation, ]
  
  # Sorry but no statehood this time. You said DC? Never heard of it.
  temp = temp[temp$st != "PR", ]
  
  # Categorize by job type
  temp = temp %>% mutate(year_period = case_when(
    year >= Begin_Year_1 & year <= End_Year_1 ~ 1, #First Period
    year >= Begin_Year_2 & year <= End_Year_2 ~ 2, #Second Period
    TRUE ~ -1
  )) %>% filter(year_period != -1) -> temp
  
  # I have checked: If there's 0 in any year's employment, it means that original data had an NA.
  # We shall drop it.
  temp = temp[!(is.na(temp$Est_Population) | temp$tot_employment == 0), ]
  
  
  temp = temp[, c("st", "job_type", "occ_code", "tot_employment", "year", "Est_Population", "year_period")]
  
  # Sum 
  temp = temp %>% group_by(st, job_type, year_period) %>%
    summarize(ave_ratio = sum(tot_employment) / sum(Est_Population)) %>%
    unique()
  
  temp = temp %>% pivot_wider(names_from = year_period, values_from = ave_ratio,
                              names_prefix = "year_period_") %>%
    select(st, job_type, year_period_1, year_period_2) %>%
    mutate(job_ratio_growth = year_period_2 / year_period_1) %>%
    select(st, job_type, job_ratio_growth) %>%
    pivot_wider(names_from = job_type, values_from = job_ratio_growth,
                names_prefix = "Job Ratio Growth, ")
  
  temp = as.data.frame(temp)
  
  temp = temp[, c("st", paste("Job Ratio Growth, ", First_Occupation, sep = ""),
                  paste("Job Ratio Growth, ", Second_Occupation, sep = ""))]
  # In this case we don't need to worry about ordering or messing up variables.
    
  if(plot == 1){
    plot(x = temp[, 2], y = temp[, 3],
         xlab = colnames(temp)[2],
         ylab = colnames(temp)[3])
         # xlab = paste("Occupation: ", First_Occupation, sep = ""),
         # ylab = paste("Occupation: ", Second_Occupation, sep = ""))
         # Fix Scale)
    title(main = "Growth in Averaged Job to Population Ratio",
          sub = paste("Period 1:", Begin_Year_1, "to", End_Year_1, 
                      ", Period 2:", Begin_Year_2, "to", End_Year_2, "."))
    text(temp[, 2], temp[, 3], temp[, 1], pos = 2, col = "red")
    abline(h = 1)
    abline(v = 1)
  }
  if(give_processed_data == 1){
    colnames(temp) = c("st", paste("job_ratio_growth_", First_Occupation, sep = ""),
                       paste("job_ratio_growth_", Second_Occupation, sep = ""))
    return(temp)
  }
}

Ratio_Extractor_by_Code = function(First_Occupation_Code, Second_Occupation_Code, Input_Data,  
                                   Begin_Year_1 = 2009, End_Year_1 = 2011,
                                   Begin_Year_2 = 2014, End_Year_2 = 2016,
                                   plot = 1, give_processed_data = 0){
  # This function basically does the same thing as above (Ratio_Extractor),
  # But it uses occupation code instead of occupation titles
  return(
    Ratio_Extractor(
      First_Occupation = Input_Data[Input_Data$occ_code == First_Occupation_Code, ][1, ]$job_type,
      Second_Occupation = Input_Data[Input_Data$occ_code == Second_Occupation_Code, ][1, ]$job_type,
      Input_Data = Input_Data, Begin_Year_1 = Begin_Year_1, End_Year_1 = End_Year_1,
      Begin_Year_2 = Begin_Year_2, End_Year_2 = End_Year_2,
      plot = plot, give_processed_data = give_processed_data
    )
  )
}


#####
# Part 2: job/population and wage extractor.

Ratio_Wage_Extractor = function(Occupation_for_Ratio, Occupation_for_Wage = Occupation_for_Ratio, Input_Data,  
                           Begin_Year_Ratio = 2003, End_Year_Ratio = 2018,
                           Begin_Year_Wage = 2003, End_Year_Wage = 2018,
                           plot = 1, give_processed_data = 0){
  
  # This function plots yearly earning (in thousands of dollars) over average number of jobs
  # per 1,000 populations.
  # Input: dataframe, just use "Employment Data"
  # Output: a plot and/or a processed data
  # Formula: For each occupation, define:
  # Ave_job_ratio = sum(employment by year) / sum(population by year), summing for all year in a period
  # Ave_annual_wage = mean(wages in all year)
  # This function then plots the result, for each state, as scatterplot.
  # Note that you can have different occupations
  
  temp = Input_Data
  #temp = temp[temp$job_type == Occupation_for_ratio | temp$job_type == Occupation_for_ratio, ]
  
  #Compute the ratio part
  temp = temp[temp$st != "PR", ]
  
  data_for_ratio = temp %>% filter(job_type == Occupation_for_Ratio) %>% #Done with Occupation
    filter(year >= Begin_Year_Ratio & year <= End_Year_Ratio) %>% #Done with Year Range
    filter(!is.na(Est_Population)) %>% filter(tot_employment > 0)
  
  #Group them and sum 'em up
  data_for_ratio = data_for_ratio %>% group_by(st) %>%
    summarize(ave_job_ratio = sum(tot_employment) / sum(Est_Population)) %>%
    mutate(ave_job_ratio = ave_job_ratio * 1000) %>%
    unique()
  #data_for_ratio$ave_job_ratio = data_for_ratio$tot_jobs / data_for_ratio$tot_pop
  
  #Compute mean wage.
  data_for_wage = temp %>% filter(job_type == Occupation_for_Wage) %>% #Done with Occupation
    filter(year >= Begin_Year_Wage & year <= End_Year_Wage) %>% #Done with Year Range
    filter(!is.na(Est_Population)) %>% filter(tot_employment > 0)
  
  data_for_wage = data_for_wage %>% group_by(st) %>%
    summarize(ave_annual_wage = mean(annual_mean_income, na.rm = TRUE)) %>%
    mutate(ave_annual_wage = ave_annual_wage / 1000) %>%
    unique()
  
  temp = merge(data_for_ratio, data_for_wage, by = "st")
  
  if(plot == 1){
    plot(x = temp$ave_job_ratio, y = temp$ave_annual_wage,
         xlab = paste("Averaged job per 1,000 People, ", Occupation_for_Ratio, sep = ""),
         ylab = paste("Averaged yearly earning of ", Occupation_for_Wage, ", in 1,000 dollars", sep = ""))
    # Fix Scale)
    title(main = "Yearly Earning (in thousands of dollars) over Job per 1,000 people",
          sub = paste("Period for X-axis:", Begin_Year_Ratio, "to", End_Year_Ratio, 
                      ", Period for Y-axis:", Begin_Year_Wage, "to", End_Year_Wage, "."))
    text(temp[, 2], temp[, 3], temp[, 1], pos = 2, col = "red")
  }
  if(give_processed_data == 1){
    return(temp)
  }
}

Ratio_Wage_Extractor_by_Code = function(Occupation_for_Ratio_Code, 
                                        Occupation_for_Wage_Code = Occupation_for_Ratio_Code,
                                        Input_Data,  
                                        Begin_Year_Ratio = 2003, End_Year_Ratio = 2018,
                                        Begin_Year_Wage = 2003, End_Year_Wage = 2018,
                                        plot = 1, give_processed_data = 0){
  return(
    Ratio_Wage_Extractor(
      Occupation_for_Ratio = Input_Data[Input_Data$occ_code == Occupation_for_Ratio_Code, ][1, ]$job_type,
      Occupation_for_Wage = Input_Data[Input_Data$occ_code == Occupation_for_Wage_Code, ][1, ]$job_type,
      Input_Data = Input_Data, 
      Begin_Year_Ratio = Begin_Year_Ratio, End_Year_Ratio = End_Year_Ratio,
      Begin_Year_Wage = Begin_Year_Wage, End_Year_Wage = End_Year_Wage,
      plot = plot, give_processed_data = give_processed_data
    )
  )
}




#### Part three: How to plot

# Here's a guide to use these functions:

# Ratio_Extractor = function(First_Occupation: First Occupation to be included, 
#                       appear in X-axis of the plot and second column of returned dataframe

#                            Second_Occupation: First Occupation to be included, 
#                       appear in Y-axis of the plot and third column of returned dataframe

#                           Input_Data: Data Input. Just use Employment_Data as we read it earlier

#                            Begin_Year_1, End_Year_1: 
#             Beginning and end year for First period for calculating growth in job/pop ratio. Default 2009-2011

#                            Begin_Year_2, End_Year_2: 
#             Beginning and end year for Second period for calculating growth in job/pop ratio. Default 2014-2016

#                            plot: Shall it plot the result? 1 for yes, other for no. Default 1
#                           give_processed_data : Shall it return processed data? 1 for yes. Default 0.
  
  
# Ratio_Wage_Extractor(Occupation_for_Ratio: Occupation to calculate average job/1,000 people Ratio 
#                 Occupation_for_Wage: Occupation to calculate average wage. Default: Same as Occupation_for_Ratio
#                 Input_Data: Data Input. Just use Employment_Data as we read it earlier
#                 Begin_Year_Ratio, End_Year_Ratio: Year range for calculating ave. job/1,000 people. Default 2003-2018
#                 Begin_Year_Wage, End_Year_Wage: Year range for calculating ave. wage. Default 2003-2018
#                 plot: Shall it plot the result? 1 for yes, other for no. Default 1
#                 give_processed_data : Shall it return processed data? 1 for yes. Default 0.

# For "by_code" version of the above functions, all input are same.
# Just change First Two Input (Occupations) to their code, as numeric, with correspondence as below:

# 2021: Dental Hygienists
# 1020: Dentists, All
# 1021: Dentists, General (Not available before 2004)
# 1141: Registered Nurses
# 1060: Physicians and Surgeons, All 
# 1062: Physicians and Surgeons, Family Practitioner.
# To check for corresponding code, see:
# unique(Employment_Data[, 3:4])


####### Part 4:
# Plot what is required.

# Firstly, let's try dentist, all over doctor, all
Ratio_Extractor_by_Code(First_Occupation_Code = 1060, Second_Occupation_Code = 1020, Input_Data = Employment_Data)

# Secondly, let's try dentist, general over physicians, family practitioners
Ratio_Extractor_by_Code(First_Occupation_Code = 1062, Second_Occupation_Code = 1021, Input_Data = Employment_Data)

# Finally, let's try dentists, income over jobs per 1,000 people.
Ratio_Wage_Extractor_by_Code(Occupation_for_Ratio_Code = 1020, Input_Data = Employment_Data)


