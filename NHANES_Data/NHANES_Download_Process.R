library("readxl")
library("dplyr")
library("haven") #To export DTA
library(tidyr)

library(openxlsx)

# This script downloads and cleans the National Health and Nutrition Examination Survey (NHANES) data.
# Run with internet connection or downloaded file from previous execution of this script.
# Output: CSV and DTA file on compiled and cleaned NHANES data, with availability form.


######
# Part 1: Download those stuff.
## Jump to Line 92 if you have already downloaded these stuff.

# Create directory if you don't have it
if(!dir.exists("NHANES_Data_Unzip_Folder")){
  dir.create("NHANES_Data_Unzip_Folder")
}

# Download 1999 data
year = 1999
download.file(url = paste("https://wwwn.cdc.gov/Nchs/Nhanes/", year, "-", year + 1, "/DEMO.XPT", sep = ""),
              destfile = paste("./NHANES_Data_Unzip_Folder/DEMO_", year, ".XPT", sep = ""), mode = "wb")
download.file(url = paste("https://wwwn.cdc.gov/Nchs/Nhanes/", year, "-", year + 1, "/OHXDENT.XPT", sep = ""),
              destfile = paste("./NHANES_Data_Unzip_Folder/ORAL_HEALTH_DENTITION_", year, ".XPT", sep = ""), mode = "wb")


download.file(url = paste("https://wwwn.cdc.gov/Nchs/Nhanes/", year, "-", year + 1, "/OHXREF.XPT", sep = ""),
              destfile = paste("./NHANES_Data_Unzip_Folder/ORAL_HEALTH_RECOMMENDATION_", year, ".XPT", sep = ""), mode = "wb")


download.file(url = paste("https://wwwn.cdc.gov/Nchs/Nhanes/", year, "-", year + 1, "/HIQ.XPT", sep = ""),
              destfile = paste("./NHANES_Data_Unzip_Folder/HEALTH_INSURANCE_", year, ".XPT", sep = ""), mode = "wb")

# Download all years after
for(year in seq(from = 2001, to = 2017, by = 2)){
  download.file(url = paste("https://wwwn.cdc.gov/Nchs/Nhanes/", year, "-", year + 1, 
                            "/HIQ_", LETTERS[(year-1997)/2], ".XPT", sep = ""),
                destfile = paste("./NHANES_Data_Unzip_Folder/HEALTH_INSURANCE_", year, ".XPT", sep = ""), mode = "wb")
  # health Insurance Data
  # Put here because other data have different link before and after 2007.
}

for(year in seq(from = 2001, to = 2007, by = 2)){
  download.file(url = paste("https://wwwn.cdc.gov/Nchs/Nhanes/", year, "-", year + 1, 
                            "/DEMO_", LETTERS[(year-1997)/2], ".XPT", sep = ""),
                destfile = paste("./NHANES_Data_Unzip_Folder/DEMO_", year, ".XPT", sep = ""), mode = "wb")
  # Demographic Data
  if(year <= 2003){
    # They divided Periodontal to upper and lower
    download.file(url = paste("https://wwwn.cdc.gov/Nchs/Nhanes/", year, "-", year + 1, 
                              "/OHXDEN_", LETTERS[(year - 1997)/2], ".XPT", sep = ""),
                  destfile = paste("./NHANES_Data_Unzip_Folder/ORAL_HEALTH_DENTITION_", year, ".XPT", sep = ""), mode = "wb")
    # Dentition Data
    
    download.file(url = paste("https://wwwn.cdc.gov/Nchs/Nhanes/", year, "-", year + 1, 
                              "/OHXREF_", LETTERS[(year - 1997)/2], ".XPT", sep = ""),
                  destfile = paste("./NHANES_Data_Unzip_Folder/ORAL_HEALTH_RECOMMENDATION_", year, ".XPT", sep = ""), mode = "wb")
    #Recommendation of Care
  }else{
    download.file(url = paste("https://wwwn.cdc.gov/Nchs/Nhanes/", year, "-", year + 1, 
                              "/OHX_", LETTERS[(year - 1997)/2], ".XPT", sep = ""),
                  destfile = paste("./NHANES_Data_Unzip_Folder/ORAL_HEALTH_DENTITION_", year, ".XPT", sep = ""), mode = "wb")
    # Dentition Data
  }
  print(paste("Year", year, "Download Finished!"))
}



for(year in seq(from = 2009, to = 2017, by = 2)){
  download.file(url = paste("https://wwwn.cdc.gov/Nchs/Nhanes/", year, "-", year + 1, 
                            "/DEMO_", LETTERS[(year - 1997)/2], ".XPT", sep = ""),
                destfile = paste("./NHANES_Data_Unzip_Folder/DEMO_", year, ".XPT", sep = ""), mode = "wb")
  # Demographic Data
  
  download.file(url = paste("https://wwwn.cdc.gov/Nchs/Nhanes/", year, "-", year + 1, 
                            "/OHXDEN_", LETTERS[(year - 1997)/2], ".XPT", sep = ""),
                destfile = paste("./NHANES_Data_Unzip_Folder/ORAL_HEALTH_DENTITION_", year, ".XPT", sep = ""), mode = "wb")
  # Dentition Data
  
  download.file(url = paste("https://wwwn.cdc.gov/Nchs/Nhanes/", year, "-", year + 1, 
                            "/OHXREF_", LETTERS[(year - 1997)/2], ".XPT", sep = ""),
                destfile = paste("./NHANES_Data_Unzip_Folder/ORAL_HEALTH_RECOMMENDATION_", year, ".XPT", sep = ""), mode = "wb")
  #Recommendation of Care
  print(paste("Year", year, "Download Finished!"))
}



#### 
# Part2: Prepare the column names to be used

### List of demographic variables to be used, 1999:

## SEQN (Sequence Number)
## RIDRETH (Race)
## DMDCITZN (Is US Citizen?)
## DMDEDUC2 (Education level, adult 20+)
## DMDEDUC3 (Education Level, children)
## DMDHHSIZ (Household Size)
## INDHHINC (Household Income)

## RIAGENDR (Gender)
## RIDAGEYR (Age, by year)

DEMO_List_Pre_07 = c("SEQN", "RIDRETH1", "DMDCITZN", "DMDEDUC2", "DMDEDUC3", "DMDHHSIZ", "INDHHINC", "RIAGENDR", "RIDAGEYR")
DEMO_List_Post_07 = c("SEQN", "RIDRETH1", "DMDCITZN", "DMDEDUC2", "DMDEDUC3", "DMDHHSIZ", "INDHHIN2", "RIAGENDR", "RIDAGEYR")



### For Dentition part

## SEQN: Sequence Number
## OHAEXSTS, Overall Dental Health
## OHXIMP: Have an implant?
## OHXEDEN - Edentulous: yes / no
## OHXRCAR - Root Caries: yes / no
## OHXRRES - Root restorations: yes / no
## OHX: from 01 to 32, and "TC, CTC, STC, SE, DI"

## '99 to '05:
## Denlist: OHX 01 to 32 TC
Den_List_Pre_05 = c("SEQN", "OHAEXSTS", "OHXIMP", "OHXEDEN", "OHXRCAR", "OHXRRES")

## 05-07: Den_List shall be OHX 01 to 32 HTC
Den_List_05_07 = c("SEQN", "OHAEXSTS", "OHXDECAY", "OHXREST", "OHXSEAL")

## Denlist: OHX 01 to 32 TC
Den_List_09 = c("SEQN", "OHDEXSTS", "OHXDECAY", "OHXREST", "OHXSEAL")

# Change in variables included
Den_List_11_13 = c("SEQN", "OHDEXSTS", "OHXIMP")

##By '15, they added 
## OHXIMP - Dental Implant: yes / no? 
## OHXRCAR - Root caries
## OHXRCARO - Other non-carious root lesion
## OHXRRES - Root caries restoration
## OHXRRESO - Other non-carious root restoration

Den_List_Post_15 = c("SEQN", "OHDEXSTS", "OHXIMP", "OHXRCAR", "OHXRCARO", "OHXRRES", "OHXRRESO")


# This function was used to generate column names that 
# recorded the condition of each of 32 teeth of respondent.
Dentition_Tooth_Col_Name_Generator = function(Year){
  
  Den_List = rep("Variable_Mask", 32)
  if(Year >= 2005 & Year <= 2007){
    Suffix = "HTC"
  }else{
    Suffix = "TC"
  }
  if(Year == 2001){
    Prefix = "OHD"
  }else{
    Prefix = "OHX"
  }
  
  for(i in 1:32 ){
    if(i < 10){
      Den_List[i] = paste(Prefix, "0", i, Suffix, sep = "")
    }else{
      Den_List[i] = paste(Prefix, i, Suffix, sep = "")
    }
  }

  return(Den_List)
}

# This function shall produce a list of columns in dentition data.
Dentition_Data_Selector = function(Year){
  Den_List = Dentition_Tooth_Col_Name_Generator(Year)
  if(Year < 2005){
    Den_List = c(Den_List_Pre_05, Den_List)
  }else if(Year < 2009){
    Den_List = c(Den_List_05_07, Den_List)
  }else if(Year == 2009){
    Den_List = c(Den_List_09, Den_List)
  }else if(Year < 2015){
    Den_List = c(Den_List_11_13, Den_List)
  }else{
    Den_List = c(Den_List_Post_15, Den_List)
  }
  return(Den_List)
}



#### Recommendation of Care Stuff
## OHAREC: Overall Recommendation of Care
## OHQ160 - Past 30 days / painful tooth?
## OHQ170 - How many days / painful tooth?
## OHAROCDT - Untreated Caries / Restorative needs
## These are for '99 to '03

Recom_of_Care_99_01 = c("SEQN", "OHAREC", "OHQ160", "OHQ170", "OHAROCDT")

Recom_of_Care_03 = c("SEQN", "OHAREC", "OHAROCDT")


## Following for post-09
## OHAREC - Overall recommendation for care
## OHAROCDT - Decayed teeth
## OHAROCGP - Gum disease/problem
## OHAROCOH - Oral hygiene
## OHARNF - No significant findings
Recom_of_Care_Post_09 = c("SEQN", "OHAREC", "OHAROCDT", "OHAROCGP", "OHAROCOH", "OHARNF")


##### Health Insurance Stuff
## Variables to be used for health insurance
## HID010 - Covered by health insurance
## HID030A - Covered by private insurance
## HID030B - Covered by Medicare
## HID030C - Covered by Medicaid/CHIP
## HID030D - Covered by other government insurance
## HID040 - Dental coverage included
## HIQ210 - Time when no insurance in past year?
Health_Insurance_99_03 = c("SEQN", "HID010", "HID030A", "HID030B", "HID030C", "HID030D", "HID040", "HIQ210")


## HIQ011 - Covered by health insurance (Original HID010)
## HIQ031A - Covered by private insurance (Original HID030A)
## HIQ031B - Covered by Medicare (Original HID030B)
## HIQ031D - Covered by Medicaid
## HIQ031E - Covered by SCHIP (These two added together shall be MedicAid and CHIP)
## HIQ031 C, and  F through I: Added for "Covered by other government insurance"
## HIQ210 - Time when no insurance in past year?

Health_Insurance_09_and_17 = c("SEQN", "HIQ011", "HIQ031A", "HIQ031B", "HIQ031C", "HIQ031D", "HIQ031E", "HIQ031F", "HIQ031H",
                             "HIQ031I", "HIQ210")

Health_Insurance_Post_05 = c("SEQN", "HIQ011", "HIQ031A", "HIQ031B", "HIQ031C", "HIQ031D", "HIQ031E", "HIQ031F", "HIQ031G", 
                             "HIQ031H", "HIQ031I", "HIQ210")

### Final list of variables to be used

Final_Var_List = union(c(DEMO_List_Pre_07, Den_List_Pre_05), c(Den_List_Post_15, Recom_of_Care_99_01, Recom_of_Care_Post_09))
Final_Var_List = union(Final_Var_List, Den_List_09)
Final_Var_List = c(Final_Var_List, "Year", "OHX_Decayed_Teeth", 
                   paste("Num_Teeth_of_Condition_", 1:4, sep = ""),
                   "Num_Teeth_of_Condition_Other", "Num_Teeth_Total",
                   "HID010", "HID030A", "HID030B", "HID_Have_MedicAid", "HID_Have_CHIP",
                   "HID_Have_MedicAid_or_CHIP", "HID_Have_Other_Government_Insurance",
                   "HID040", "HIQ210")

Final_Var_List = Final_Var_List[-which(Final_Var_List == "OHDEXSTS")]

# See codebook for more specific information.


##### Okay, let's actually do these stuff

Dentition_Data_Sum = function(Input_Dentition_Data){
  # Input: Dentition Data
  # Output: dataset, with columns Num_Teeth_of_Condition 1:4 telling
  # Number of teeth in 4 conditions respectively.
  Input_Backup = Input_Dentition_Data
  
  cols_need_change = Dentition_Tooth_Col_Name_Generator(2009)
  
  for(Condition_Value in 1:4){
    Input_Backup[, paste("Num_Teeth_of_Condition_", Condition_Value, sep = "")] = NA
    Input_Backup[, cols_need_change] = Input_Dentition_Data[, cols_need_change]
    
    Input_Backup[, cols_need_change] <-
      lapply(Input_Backup[, cols_need_change], function(x){
        ifelse(x == Condition_Value, 1, 0)
      })
    
    Input_Backup[, paste("Num_Teeth_of_Condition_", Condition_Value, sep = "")] = 
      rowSums(Input_Backup[, cols_need_change], na.rm = TRUE)
  }
  
  Input_Backup[, "Num_Teeth_of_Condition_Other"] = 32 - rowSums(Input_Backup[, paste("Num_Teeth_of_Condition_", 1:4, sep = "")])
  Input_Backup$Num_Teeth_Total = 32
  Input_Backup = Input_Backup %>% select(-cols_need_change)
  
  return(Input_Backup)
}



Yearly_Extractor = function(Year){
  ## First, extract the Demographic data and change the title
  temp_Demo = read_xpt(file = paste("./NHANES_Data_Unzip_Folder/DEMO_", Year, ".XPT", sep = ""))
  if(Year < 2007){
    temp_Demo = temp_Demo[, DEMO_List_Pre_07]
  }else{
    temp_Demo = temp_Demo[, DEMO_List_Post_07]
    temp_Demo = temp_Demo %>% rename_at(vars(c("INDHHIN2")), ~c("INDHHINC"))
  }
  
  ## Secondly, let's grind the Dentition Data
  temp_Dentition = read_xpt(file = paste("./NHANES_Data_Unzip_Folder/ORAL_HEALTH_DENTITION_", Year, ".XPT", sep = ""))
  temp_Dentition = temp_Dentition[, Dentition_Data_Selector(Year)]
  if(Year >= 2009){
    temp_Dentition = temp_Dentition %>% rename_at(vars("OHDEXSTS"), ~c("OHAEXSTS"))
  }
  if(Year %in% c(2001, 2005, 2007)){
    temp_Dentition = temp_Dentition %>% rename_at(vars(Dentition_Tooth_Col_Name_Generator(Year)), 
                                                  ~c(Dentition_Tooth_Col_Name_Generator(2009)))
  }

  temp_Dentition = Dentition_Data_Sum(temp_Dentition)
  temp = left_join(temp_Demo, temp_Dentition, by = "SEQN")
  
  ## Thirdly, let us provide the Recommendation of Care Data
  if(!(Year %in% c(2004:2008))){
    # Only in these times are Recommendation Data Available
    temp_Recom_of_Care = read_xpt(file = paste("./NHANES_Data_Unzip_Folder/ORAL_HEALTH_RECOMMENDATION_", Year, ".XPT", sep = ""))
    if(Year < 2003){
      temp_Recom_of_Care = temp_Recom_of_Care[, Recom_of_Care_99_01]
    }else if(Year == 2003){
      temp_Recom_of_Care = temp_Recom_of_Care[, Recom_of_Care_03]
    }else if(Year >= 2009){
      temp_Recom_of_Care = temp_Recom_of_Care[, Recom_of_Care_Post_09 ]
      temp_Recom_of_Care = temp_Recom_of_Care %>% rename_at(vars(c("OHAROCDT")), ~c("OHX_Decayed_Teeth"))
      # temp_Recom_of_Care = temp_Recom_of_Care %>% rename_at(vars(c("OHAROCGP")), ~c("OHX_Gum_Disease"))
    }
    temp = left_join(temp, temp_Recom_of_Care, by = "SEQN")
  }

  
  ## Finally, let us deal with the health insurance data
  temp_health_insurance = read_xpt(file = paste("./NHANES_Data_Unzip_Folder/HEALTH_INSURANCE_", Year, ".XPT", sep = ""))
  
  if(Year < 2005){
    temp_health_insurance = temp_health_insurance[, Health_Insurance_99_03] %>%
      rename_at(vars("HID030C", "HID030D"), 
                ~c("HID_Have_MedicAid_or_CHIP", "HID_Have_Other_Government_Insurance"))
  }else if(Year == 2009 | Year == 2017){
    # Rename some variables to pre-09 standard
    temp_health_insurance = temp_health_insurance[, Health_Insurance_09_and_17] %>%
      rename_at(vars("HIQ011", "HIQ031A", "HIQ031B", "HIQ031D", "HIQ031E"),
                ~c("HID010", "HID030A", "HID030B", "HID_Have_MedicAid", "HID_Have_CHIP"))
    
    # This is a list of columns that needs to be changed.
    # Previously, they were done as 1 yes 0 no.
    # Now, they use larger numbers, but still a yes/no question. Having a number that is <= 25 means that it is a "yes",
    # Thus should be changed to 1 in accordance with earlier part.
    cols_need_change = c("HID030A", "HID030B", "HID_Have_MedicAid", "HID_Have_CHIP", "HIQ031C",
                         "HIQ031F", "HIQ031H", "HIQ031I")
    temp_health_insurance[, cols_need_change] <-
      lapply(temp_health_insurance[, cols_need_change], function(x){
        ifelse(x <= 25, 1, x)
      })
    
    temp_health_insurance = temp_health_insurance %>% #Calculate people with Both MedicAid and CHIP
      mutate(HID_Have_MedicAid_or_CHIP = rowSums(cbind(HID_Have_MedicAid, HID_Have_CHIP), na.rm = TRUE)) %>%
      mutate(HID_Have_MedicAid_or_CHIP = ifelse(HID_Have_MedicAid_or_CHIP >= 1 & HID_Have_MedicAid_or_CHIP <= 2, 
                                                1, HID_Have_MedicAid_or_CHIP)) %>% # Sum up other government insurance
      mutate(HID_Have_Other_Government_Insurance = rowSums(cbind(HIQ031C, HIQ031F, HIQ031H, HIQ031I), na.rm = TRUE)) %>%
      mutate(HID_Have_Other_Government_Insurance = ifelse(HID_Have_Other_Government_Insurance >= 1 & HID_Have_Other_Government_Insurance <= 4, 
                                                1, HID_Have_Other_Government_Insurance)) %>%
      select(-c("HIQ031C", "HIQ031F", "HIQ031H", "HIQ031I")) #Drop useless sub-categories.
    
    
  }else {
    # Same as above, except having an additional government insurance
    temp_health_insurance = temp_health_insurance[, Health_Insurance_Post_05] %>%
      rename_at(vars("HIQ011", "HIQ031A", "HIQ031B", "HIQ031D", "HIQ031E"),
                ~c("HID010", "HID030A", "HID030B", "HID_Have_MedicAid", "HID_Have_CHIP"))
    
    cols_need_change = c("HID030A", "HID030B", "HID_Have_MedicAid", "HID_Have_CHIP", "HIQ031C",
                         "HIQ031F", "HIQ031G", "HIQ031H", "HIQ031I")
    temp_health_insurance[, cols_need_change] <-
      lapply(temp_health_insurance[, cols_need_change], function(x){
        ifelse(x <= 25, 1, x)
      })
    
    temp_health_insurance = temp_health_insurance %>%
      mutate(HID_Have_MedicAid_or_CHIP = rowSums(cbind(HID_Have_MedicAid, HID_Have_CHIP), na.rm = TRUE)) %>%
      mutate(HID_Have_MedicAid_or_CHIP = ifelse(HID_Have_MedicAid_or_CHIP >= 1 & HID_Have_MedicAid_or_CHIP <= 2, 
                                                1, HID_Have_MedicAid_or_CHIP)) %>%
      mutate(HID_Have_Other_Government_Insurance = rowSums(cbind(HIQ031C, HIQ031F, HIQ031G, HIQ031H, HIQ031I), na.rm = TRUE)) %>%
      mutate(HID_Have_Other_Government_Insurance = ifelse(HID_Have_Other_Government_Insurance >= 1 & HID_Have_Other_Government_Insurance <= 4, 
                                                          1, HID_Have_Other_Government_Insurance)) %>%
      select(-c("HIQ031C", "HIQ031F", "HIQ031G", "HIQ031H", "HIQ031I"))
    
  }
  # Combine all these stuff
  temp = left_join(temp, temp_health_insurance, by = "SEQN")

  # Record the year. Change any variable that did NOT show as -1.
  temp$Year = Year
  temp[setdiff(Final_Var_List, colnames(temp))] = -1
  return(temp)
}


## Create a data frame to record availability
Availability_Data = matrix("X", nrow = length(seq(from = 1999, to = 2017, by = 2)), ncol = length(Final_Var_List))
# Record the year
Availability_Data = as.data.frame(Availability_Data)
colnames(Availability_Data) = Final_Var_List
Availability_Data$Year = seq(from = 1999, to = 2017, by = 2)

result = Yearly_Extractor(1999)

#Calculate availability based on the -1 we given in previous function.
Availability_Data = Availability_Data[, colnames(result)]
Availability_Data[which(Availability_Data$Year == 1999), which(result[1, ] == -1)] <- NA

for(i in seq(2001, 2017, by = 2)){
  result_this_year = Yearly_Extractor(i)
  
  Availability_Data = Availability_Data[, colnames(result_this_year)]
  Availability_Data[which(Availability_Data$Year == i), which(result_this_year[1, ] == -1)] <- NA
  
  result = rbind(result, result_this_year)
  print(paste("Year", i, "Done!"))
}

# re-arrange the columns
Availability_Data = Availability_Data[, colnames(result)]

# Rename the variables so they are easier read.
result = result %>% 
  rename_at(vars(Final_Var_List[1:26]),
            ~c("Sequence_Num", "Race_and_Ethnicity", "Is_US_Citizen",
               "Level_Education_Adult", "Level_Education_Age_15_to_19", "Household_Size", 
               "Household_Income", "Gender", "Age_in_Year", "Dental_Exam_Completion_Status", "Have_Implant",
               "Have_Edentulous", "Have_Root_Caries", "Have_Root_Restorations", "Have_Other_Non_Carious_Root_Lension",
               "Have_Root_Caries_Restoration", "Overall_Recommendation_of_Care", "Have_Painful_Tooth_Past_30_Days",
               "Num_Days_with_Painful_Tooth", "Have_Untreated_Caries_Restoration_Needs", "Have_Gum_Disease", "Have_Oral_Hygiene",
               "No_Significant_Findings", "Have_Dental_Decay", "Have_Dental_Restoration", "Have_Dental_Sealant")) %>%
  rename_at(vars("HID010", "HID030A", "HID030B", "HIQ210", "HID040"), 
            ~c("Is_Covered_by_Health_Insurance", "Is_Covered_by_Private_Insurance", "Is_Covered_by_Medicare", 
               "Have_time_no_coverage_past_year", "Have_Dental_Insurance")) %>% 
  rename_at(vars(paste("Num_Teeth_of_Condition_", 1:4, sep = "")),
            ~c("Num_Teeth_Primary_Present", "Num_Teeth_Permanent_Present", 
              "Num_Teeth_Implant_Present", "Num_Teeth_Not_Present"))


Availability_Data = Availability_Data %>% 
  rename_at(vars(Final_Var_List[1:26]),
            ~c("Sequence_Num", "Race_and_Ethnicity", "Is_US_Citizen",
               "Level_Education_Adult", "Level_Education_Age_15_to_19", "Household_Size", 
               "Household_Income", "Gender", "Age_in_Year", "Dental_Exam_Completion_Status", "Have_Implant",
               "Have_Edentulous", "Have_Root_Caries", "Have_Root_Restorations", "Have_Other_Non_Carious_Root_Lension",
               "Have_Root_Caries_Restoration", "Overall_Recommendation_of_Care", "Have_Painful_Tooth_Past_30_Days",
               "Num_Days_with_Painful_Tooth", "Have_Untreated_Caries_Restoration_Needs", "Have_Gum_Disease", "Have_Oral_Hygiene",
               "No_Significant_Findings", "Have_Dental_Decay", "Have_Dental_Restoration", "Have_Dental_Sealant")) %>%
  rename_at(vars("HID010", "HID030A", "HID030B", "HIQ210", "HID040"), 
            ~c("Is_Covered_by_Health_Insurance", "Is_Covered_by_Private_Insurance", "Is_Covered_by_Medicare", 
               "Have_time_no_coverage_past_year", "Have_Dental_Insurance")) %>% 
  rename_at(vars(paste("Num_Teeth_of_Condition_", 1:4, sep = "")),
            ~c("Num_Teeth_Primary_Present", "Num_Teeth_Permanent_Present", 
               "Num_Teeth_Implant_Present", "Num_Teeth_Not_Present"))

# Write them down
write.csv(result, "NHANES_Result.csv")

# Transpose the Availability data for easier reading.
Availability_Data = as.data.frame(t(Availability_Data))
colnames(Availability_Data) = Availability_Data["Year", ]
Availability_Data$Var_Name = rownames(Availability_Data)


write.xlsx(Availability_Data, "NHANES_Availability.xlsx")

result = result %>% 
  rename_at(vars(c("Have_Untreated_Caries_Restoration_Needs", "HID_Have_Other_Government_Insurance", "Have_Other_Non_Carious_Root_Lension")),
            ~c("Untreated_Caries_Rest_Needs", "Have_Other_Gov_Insurance", "Other_N_Carious_Root_Lension"))


write_dta(result, "NHANES_Result.dta")



### Example for plotting: Yearly people w/ Good Oral Health
library(ggplot2)

# Process it for plotting
temp_for_plotting =
  # First, give 1 to anything where Overall_Recommendation_of_Care was recorded and available.
  mutate(result, Var1_Mask = ifelse(Overall_Recommendation_of_Care %in% c(1:4), 1, 0)) %>%
  #Second, here we give 1 to anythign where OVerall Recommendation of Care have value 4
  # Which means continuing routine care.
    mutate(Var1_Mask_2 = ifelse(Overall_Recommendation_of_Care == 4, 1, 0)) %>% 
    filter(Var1_Mask == 1) %>%
    group_by(Year) %>%
  # Sum each year's available response and those who have 1 in Var1_Mask_2
    summarize(Var1_Fit_Condition_Sum = sum(Var1_Mask_2, na.rm = TRUE), 
              Var1_Total = sum(Var1_Mask, na.rm = TRUE)) %>%
  # Find a ratio.
    mutate(Pctg_No_Need_Urgent_Dental_Care = Var1_Fit_Condition_Sum / Var1_Total)
  
#Plotting.
ggplot(data = temp_for_plotting,
         aes(x = Year, y = Pctg_No_Need_Urgent_Dental_Care)) +
    geom_line() +
    geom_text(label = temp_for_plotting$Pctg_No_Need_Urgent_Dental_Care, 
              aes(x = Year, y = Pctg_No_Need_Urgent_Dental_Care),
              nudge_x = 0, nudge_y = .03) +
    labs(y = "Year",
         x = "Pctg of respondent") +
    ggtitle("Percentage of People who are asked to continue their routine care, in favor of more urgent ones.")



