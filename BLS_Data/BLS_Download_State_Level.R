library("readxl")
library("dplyr")
library("haven") #To export DTA
library(tidyr)

# This code downloads Bureau of Labor Statistics' Data on all occupations
# Back to year 2000 and compiles them into single dataset containing only data for
# dentists, physicians, dental hygienists, and nurses.
### If you have used this script before and have all BLS data downloaded 
# with name OESM_year_State.zip, then run Part 1 and then begin from
# LINE 99.

# If you have already unzipped all BLS data,
# Run Part 1
# Then Run from Line 115 (Part 3)


####
# Part 1: Population Data.

# See readme.txt in Population_Data_Folder for their sources.
# Regardless you downloaded BLS data, you still must run Part 1.

if(!dir.exists("Population_Data_Folder")){
  dir.create("Population_Data_Folder")
}

download.file(url = "http://eadiv.state.wy.us/pop/ST91-99est.xls",
              destfile = "./Population_Data_Folder/Pop_Estimate_1990_2000.xls", mode = "wb")

download.file(url = "https://www2.census.gov/programs-surveys/popest/tables/2000-2010/intercensal/state/st-est00int-01.xls",
              destfile = "./Population_Data_Folder/Pop_Estimate_2000_2010.xls", mode = "wb")

download.file(url = "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/national/totals/nst-est2020.csv",
              destfile = "./Population_Data_Folder/Pop_Estimate_2010_2020.csv", mode = "wb")

download.file(url = "https://www2.census.gov/programs-surveys/popest/tables/2020-2021/state/totals/NST-EST2021-POP.xlsx",
              destfile = "./Population_Data_Folder/Pop_Estimate_2020_2021.xlsx", mode = "wb")

## This chunk gives '97 to '99
Pop_Data = read_xls("./Population_Data_Folder/Pop_Estimate_1990_2000.xls")
Pop_Data = Pop_Data[8:58, c(3, (ncol(Pop_Data) - 3):(ncol(Pop_Data) - 1))]
colnames(Pop_Data) = c("State", 1997:1999)
Pop_Data = Pop_Data %>% mutate_at(as.character(1997:1999), as.numeric)



#This chunk gives up to 2009 estimation
Pop_Data2 = read_xls("./Population_Data_Folder/Pop_Estimate_2000_2010.xls")
Pop_Data2 = Pop_Data2[3:61, -c(2, 13)] #Lines above are useless
Pop_Data2[1,1] = "State" 
Pop_Data2[1, 12] = "2010"
colnames(Pop_Data2) <- Pop_Data2[1, ] #

Pop_Data2 = Pop_Data2[c(7:57, 59), c("State", 2000:2009)] 
#Data now only have names of state and yearly population estimate

Pop_Data2$'2000' = as.numeric(Pop_Data2$'2000')
Pop_Data2$State = gsub('\\.', '', Pop_Data2$State)
#Clear the dots in name of the state.
Pop_Data = inner_join(Pop_Data, Pop_Data2, by = "State")


# This chunk give up to 2019 estimation
Pop_Data2 = read.csv("./Population_Data_Folder/Pop_Estimate_2010_2020.csv", stringsAsFactors = FALSE)
Pop_Data2 = Pop_Data2[6:nrow(Pop_Data2), 5:ncol(Pop_Data2)]
Pop_Data2 = Pop_Data2[, -c(2, 3, ncol(Pop_Data2) - 2, ncol(Pop_Data2) - 1)]
colnames(Pop_Data2) <- c("State", 2010:2019)

# Merge two datasets
Pop_Data = inner_join(Pop_Data, Pop_Data2, by = "State")


# This chunk goes to 2021
Pop_Data2 = read_xlsx("./Population_Data_Folder/Pop_Estimate_2020_2021.xlsx")
Pop_Data2 = Pop_Data2[9:61, -2]
Pop_Data2 = Pop_Data2[-52, ]
colnames(Pop_Data2) = c("State", 2020,2021)
Pop_Data2$`2020` = as.numeric(Pop_Data2$`2020`)
Pop_Data2$State = gsub('\\.', '', Pop_Data2$State)

Pop_Data = inner_join(Pop_Data, Pop_Data2, by = "State")

#Put the variables: 2000 to 2021 into column: year. So that we have 
# Only three columns: state, year, est_population (which is census bureau's estimation of population by the year.)
Pop_Data = pivot_longer(Pop_Data, cols = 2:ncol(Pop_Data), names_to = "year", values_to = "Est_Population")
Pop_Data$year = as.numeric(Pop_Data$year)

# State names to abbreviations.
Pop_Data$st = c(state.abb, "PR", "DC")[match(Pop_Data$State, c(state.name, "Puerto Rico", "District of Columbia"))]

# Part 1 Done


######
# Part 2: Download and unzip the BLS data.



if(!dir.exists("BLS_Data_Unzip_Folder")){
  dir.create("BLS_Data_Unzip_Folder")
}


download.file(url = paste("https://www.bls.gov/oes/special.requests/oes", 99, "st.zip", sep = ""),
              destfile = paste("./BLS_Data_Unzip_Folder/OESM_", 1999, "_State.zip", sep = ""))

for(i in 0:2){
  download.file(url = paste("https://www.bls.gov/oes/special.requests/oes0", i, "st.zip", sep = ""),
                destfile = paste("./BLS_Data_Unzip_Folder/OESM_", i + 2000, "_State.zip", sep = ""))
}

for(i in 3:9){
  download.file(url = paste("https://www.bls.gov/oes/special.requests/oesm0", i, "st.zip", sep = ""),
                destfile = paste("./BLS_Data_Unzip_Folder/OESM_", i + 2000, "_State.zip", sep = ""))
}


# Per Requested, I changed the end time to year 2018
for(i in 10:18){
  download.file(url = paste("https://www.bls.gov/oes/special.requests/oesm", i, "st.zip", sep = ""),
                destfile = paste("./BLS_Data_Unzip_Folder/OESM_", i + 2000, "_State.zip", sep = ""))
  # download.file(url = paste("https://www.bls.gov/oes/special.requests/oesm", i, "ma.zip", sep = ""),
  #               destfile = paste("./BLS_Data_Unzip_Folder/OESM_", i + 2000, "_MSA.zip", sep = ""))
  
  print(i)
}


#### IMPORTANT:
## If you already downloaded BLS data, but not yet unzipped it, run from this line

for(i in (-1):18){
  unzip(zipfile = paste("./BLS_Data_Unzip_Folder/OESM_", i+2000, "_State.zip", sep = ""),
        exdir = "./BLS_Data_Unzip_Folder")
  # unzip(zipfile = paste("./BLS_Data_Unzip_Folder/OESM_", i+2000, "_MSA.zip", sep = ""), 
  #       exdir = "./BLS_Data_Unzip_Folder")
  #print(i + 2000)
}


######
# Part 3: process the BLS Data
# If you have already unzipped all BLS data, run from here.


OCC_CODE_Changer_Pre_18 = function(temp){
  #Used for year of or before 2018
  #Input: Dataframe, extracted from unzipped raw data
  # Output: Dataframe, but containing only occupation specified below
  
  
  # Keep only these occupations.
  Intermediate_Data = temp[temp$occ_code == "29-1021" | #Dentist, General
                             temp$occ_code == "29-1020" | #All Dentist (Used before '09)
                             temp$occ_code == "29-2021" | #Dental Hygienists
                             temp$occ_code == "29-1062" | #Family and General Practitioners.
                             temp$occ_code == "29-1069" | #Physician and Surgeons, all other.
                             temp$occ_code == "29-1111" | #Registered Nurse 
                             temp$occ_code == "29-1141", ] #Registered Nurse after 2012. 
  
  # A few notes:
  # code for nurse is 1111 before 2012 and 1141 in and after 2012
  # We can keep all because 1141 was not used before 2012 and 1111 not used after 2012
  # Dentists are not divided into subgroups before 2003. 
  # Physician and Surgeons, all other does not exist before 2003
  
  #Source: https://www.bls.gov/soc/2000/soc-structure-2000.pdf 
  
  # Since we are using data up to year 2018, we don't need to format occupational code as they are in 2021.

  # Change the code of nurse to post-2012 code for simplicity.
  
  Intermediate_Data %>%
    mutate(occ_code = recode(occ_code, "29-1111" = "29-1141", #for Nurse
                             ) ) -> Intermediate_Data
  
  #Change code for nurses to what it should be after 2012 (Code becomes 29-1141)
  
  return(Intermediate_Data)
}

State_Processor = function(Input_Year, Input_Data){
  # This function processes the inputed data and keep only needed columns.
  # Input: Dataframe, read from excel (with Table_Reader function), and year of the data.
  # Output: Dataframe, containing only the variables specified below and "year", which 
  # records the year of the entry.
  
  return(select(OCC_CODE_Changer_Pre_18(Input_Data), 
           #Only this list of variables are kept.
           # See data_descriptor_wages_by_state.xlsx for specific description.
           area, st, occ_code, occ_title, tot_emp, h_mean, a_mean, h_median, a_median) %>%
           # Year of the entry, specified by Input_Year
           mutate(year = Input_Year))
}




Table_Reader = function(year){
  # This function reads the downloaded and unzipped data for later processing.
  # Input: Year of the data to be read
  # Output: Dataframe
  # I used this function instead of use directly read_xls because the format of the 
  # Unzipped files varies greatly year by year.
  
  if(year == 1999){
    temp =  read_xls(paste("./BLS_Data_Unzip_Folder/state_", year, "_dl.xls", sep = ""))
    temp[42, 5] = "occ_title"
    colnames(temp) = temp[42, ]
    temp = temp[-c(1:42), ]
  } else if(year == 2000){
    temp =  read_xls(paste("./BLS_Data_Unzip_Folder/state_", year, "_dl.xls", sep = ""))
    temp[41, 5] = "occ_title"
    colnames(temp) = temp[41, ]
    temp = temp[-c(1:41), ]
  } else if(year <= 2002){
    temp =  read_xls(paste("./BLS_Data_Unzip_Folder/state_", year, "_dl.xls", sep = ""))
  } else if(2002 <= year & year <= 2007){
    temp = read_xls(paste("./BLS_Data_Unzip_Folder/state_may", year, "_dl.xls", sep = ""))
  } else if(year == 2008){
    temp = read_xls("./BLS_Data_Unzip_Folder/state__M2008_dl.xls")
  } else if(year == 2009){
    temp = read_xls("./BLS_Data_Unzip_Folder/state_dl.xls")
  }else if(year <= 2013){
    temp = read_xls(paste("./BLS_Data_Unzip_Folder/state_M", year, "_dl.xls", sep = ""))
  }else{
    temp = read_xlsx(paste("./BLS_Data_Unzip_Folder/oesm", year - 2000, "st/state_M", year, "_dl.xlsx", sep = ""))
  }
  
  #Get all variable names to lower.
  # The variable names are same, but sometimes they are all capitalized.
  colnames(temp) = tolower(colnames(temp))
  return(temp)
}



Total_Occupation_Counter = function(Input_Year, Input_Data){
  # This function sums up number of occupations for a given year.
  # Input: year, dataframe of BLS (from Table_Reader)
  # Output: dataframe, with number of occupationa and mean annual salary for each included occupation
  # For each state at that year.
  
  temp = Input_Data
  
  # Include only medical industry
  temp = temp[grepl("29-", temp$occ_code), ]
  temp$occ_code = as.numeric(gsub("29-", "", temp$occ_code))
  temp = select(temp, 
                # List of Variables needed
                st, occ_code, occ_title, tot_emp, a_mean)
  
  # Turn data to numeric, and fit 0 in NA.
  temp$tot_emp = as.numeric(temp$tot_emp)
  temp$tot_emp = ifelse(is.na(temp$tot_emp), 0, temp$tot_emp)
  temp$a_mean = as.numeric(temp$a_mean)
  
  # Identify occupations based on occupation code
  temp %>% mutate(occ_group = case_when(
    occ_code == 1021 ~ "Dentists, General", #All kind of dentists added together
    occ_code == 1062 ~ "Physicians and Surgeons, Family Practitioner", #All Physicians and Surgeons
    TRUE ~ "Other"
  )) %>% filter(occ_group != "Other") -> temp1
  
  # Add up number in occupations in these two specific category.
  temp1 = temp1 %>% group_by(st, occ_group) %>%
    summarize(occ_code = occ_code, emp_of_catg = sum(tot_emp), total_wage_of_catg = a_mean)
  
  # Add up general type (dentist, physicians)
  temp %>% mutate(occ_group = case_when(
    occ_code >= 1020 & occ_code < 1030 ~ "Dentists, All", #All kind of dentists added together
    occ_code >= 1060 & occ_code < 1070 ~ "Physicians and Surgeons, All", #All Physicians and Surgeons
    occ_code == 2021 ~ "Dental Hygienists", #Dental Hygienists
    occ_code == 1111 | occ_code == 1141 ~ "Nurses", #Nurses
    TRUE ~ "Other"
  )) %>% mutate(occ_code = case_when(
    #Change the code for easier identification later on
    occ_code >= 1020 & occ_code < 1030 ~ 1020, #All kind of dentists added together, code to 1020
    occ_code >= 1060 & occ_code < 1070 ~ 1060, #All Physicians and Surgeons, code to 1060
    occ_code == 2021 ~ 2021, #Dental Hygienists, no need to change code
    occ_code == 1111 | occ_code == 1141 ~ 1141, #Nursesm, change code to 1141.
    TRUE ~ 114514
  )) %>% filter(occ_group != "Other") -> temp #Same condition, so we should have cleaned data here.
  
  temp$tot_income = temp$tot_emp * temp$a_mean
  
  # Sum up occupation and income of each occupation.
  temp = temp %>% group_by(st, occ_group) %>%
    summarize(occ_code = occ_code, emp_of_catg = sum(tot_emp), total_wage_of_catg = sum(tot_income)) %>%
    unique()
  
  temp$total_wage_of_catg = temp$total_wage_of_catg / temp$emp_of_catg
  
  #Combine two datasets.
  temp = rbind(temp, temp1)
  
  colnames(temp) = c("st", "job_type", "occ_code", "tot_employment", "annual_mean_income")
  
  temp$year = Input_Year #Set the year.
  return(temp)
}


result = Table_Reader(1999)
Employment_Count_Result = Total_Occupation_Counter(Input_Year = 1999, Input_Data = result)
result = State_Processor(Input_Year = 1999, Input_Data = result)

for(i in 2000:2018){
  temp = Table_Reader(i)
  Employment_Count_Addon = Total_Occupation_Counter(Input_Year = i, Input_Data = temp)
  temp = State_Processor(Input_Year = i, Input_Data = temp)
  
  Employment_Count_Result = rbind(Employment_Count_Result, Employment_Count_Addon)
  result = rbind(result, temp)
  print(i)
}

for(i in 5:10){
  result[, i] = as.numeric(unlist(result[, i]))
}

result$occ_code = as.numeric(gsub("29-", "", result$occ_code))

result$occ_title = tolower(gsub("\\*", "", result$occ_title))


result = inner_join(result, Pop_Data, by = c("st", "year"))
#result = subset(result, select = -c(area, State))



write.csv(result, file = "Wages_By_State.csv")

# Add Labels
attr(result$area, "label") <- "State FIPS Code"
attr(result$st, "label") <- "State Abbreviation"
attr(result$occ_code, "label") <- "Occupation Code, in BLS 2010 Standard Occupational Classification System."
attr(result$occ_title, "label") <- "Occupation Title, in BLS 2010 Standard Occupational Classification System."
attr(result$tot_emp, "label") <- "Total Employment of the occupation, in the state recorded in st variable, of year recorded in year variable"
attr(result$h_mean, "label") <- "Mean Hourly Wage"
attr(result$a_mean, "label") <- "Mean Annual Wage"
attr(result$h_median, "label") <- "Median Hourly Wage"
attr(result$a_median, "label") <- "Median Annual Wage"
attr(result$year, "label") <- "Year of the entry"
attr(result$Est_Population, "label") <- "Estimated Population."
attr(result$State, "label") <- "State Name"



write_dta(result, "Wages_By_State.dta")



# Now, let's deal with employment data
Employment_Count_Result = inner_join(Employment_Count_Result, Pop_Data, by = c("st", "year"))

# Employment_Count_Result = Employment_Count_Result[, -ncol(Employment_Count_Result)]
# colnames(Employment_Count_Result)[ncol(Employment_Count_Result)] = "Est_Population"
# 
write.csv(Employment_Count_Result, file = "Employment_By_State.csv")
# 
attr(Employment_Count_Result$st, "label") <- "State Abbreviation"
attr(Employment_Count_Result$job_type, "label") <- "Occupation Title, in BLS 2010 Standard Occupational Classification System."
attr(Employment_Count_Result$occ_code, "label") <- "Occupation Code, in BLS 2010 Standard Occupational Classification System."
attr(Employment_Count_Result$tot_employment, "label") <- "Total Employment of the occupation, in the state recorded in st variable, of year recorded in year variable"
attr(Employment_Count_Result$annual_mean_income, "label") <- "Mean Annual Wage"
attr(Employment_Count_Result$year, "label") <- "Year of the entry"
attr(Employment_Count_Result$Est_Population, "label") <- "Estimated Population."
attr(Employment_Count_Result$State, "label") <- "State Name."

# 

write_dta(Employment_Count_Result, "Employment_By_State.dta")

#Use the following line if you don't need these stuff afterwards.
#unlink("BLS_Data_Unzip_Folder", recursive = TRUE)





