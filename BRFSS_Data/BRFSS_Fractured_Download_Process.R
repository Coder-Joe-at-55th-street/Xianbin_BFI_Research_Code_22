library("readxl")
library("dplyr")
library("haven") #To export DTA
library(tidyr)

#### HOW TO USE THIS SCRIPT:

### MAKE SURE YOU HAVE INTERNET, if you are downloading,
### MAKE SURE YOU HAVE AT LEAST ENOUGH DISK SPACE.
### STANDARD: < 100 M for a year, 2GB Max Merged Data Size.
### MAKE SURE YOU HAVE ENOUGH TIME: take about 2-3 minutes each year.
### MAKE SURE R CAN ACCESS INTERNET, READ/MODIFY/UNZIP/WRITE FILES.

### NOW, CHANGE PARAMETERS BELOW.
### RUN ALL.
### WAIT AND MAKE YOUSELF A CUP OF TEA
### CHECK FOR RESULT:
### 

# Currently Only support downloading from 1999 to 2020.
Download_Year_Start = 2010 #Which year to begin with? Default 1999
Download_Year_End = 2011 #Which year to end in? Default 2020
Keep_Download = 0 #Shall you keep your downloaded raw data, given that you don't have it before? 
#1 for yes, any other value for no. Default 0

Keep_Unzipped = 0 #Shall you keep what was unzipped from data, given that you don't have it before? 
#1 for yes, any other value for no. Default 0

Keep_Yearly = 0 #Do you want yearly data to be left? 1 yes, any other for no. Default 0
Keep_Merged = 1 #Do you want merged data to be left? 1 yes, any other for no. Default 0
CSV_OR_DTA = 3 #Do you want CSV file, DTA file, or both?
# 1 for DTA, 2 for CSV, 3 for both.

# This script automatically check for past download (by name BRFSS_data_year.zip)
# Make sure you have a directory with name "BRFSS_Data_Unzip_Folder" and dump all downloaded file there.

# IF YOU JUST WANT GOT DATA DOWNLOADED WITHOUT VIEWING CODE,
# JUMP TO LINE 330 OR CLICK "RUN" ON UPPER-RIGHT CORNER OF R-STUDIO AND MAKE YOURSELF A CUP OF TEA.


### Infrastructures: function to identify file name and download data
## # Input: year. Output: file name of the year's data after unzipping. Usefule for multiple cases.




# Input: Year
# Output: file name of the year's data.
Get_Unzipped_File_Name = function(year){
  
  if(year == 2002 | year == 2003){
    return(paste("./BRFSS_Data_Unzip_Folder/cdbrfs0", year - 2000, ".xpt", sep = ""))
  }else if(year <= 2010){
    return(paste("./BRFSS_Data_Unzip_Folder/CDBRFS", substr(as.character(year), 3, 4), ".XPT", sep = ""))
  }else{
    return(paste("./BRFSS_Data_Unzip_Folder/LLCP", year, ".XPT", sep = ""))
  }
}

## Input: Year
## Output: None. Had the year's BRFSS data downloaded to ./BRFSS_Data_Unzip_Folder/BRFSS_data_year.zip.
Downloader = function(year){
  if(year == 1999){
    download.file(url = "https://www.cdc.gov/brfss/annual_data/1999/files/CDBRFS99XPT.zip",
                  destfile = "./BRFSS_Data_Unzip_Folder/BRFSS_data_1999.zip", mode = "wb")
  }else if(year <= 2009 & year >= 2000){
    download.file(url = paste("https://www.cdc.gov/brfss/annual_data/", year, "/files/CDBRFS0", year - 2000, "XPT.zip", sep = ""),
                  destfile = paste("./BRFSS_Data_Unzip_Folder/BRFSS_data_", year, ".zip", sep = ""), mode = "wb")
  }else if(year == 2010){
    download.file(url = "https://www.cdc.gov/brfss/annual_data/2010/files/CDBRFS10XPT.zip",
                  destfile = "./BRFSS_Data_Unzip_Folder/BRFSS_data_2010.zip", mode = "wb")
  }else{
    download.file(url = paste("https://www.cdc.gov/brfss/annual_data/", year, "/files/LLCP", year, "XPT.zip", sep = ""),
                  destfile = paste("./BRFSS_Data_Unzip_Folder/BRFSS_data_", year, ".zip", sep = ""), mode = "wb")
  }
  print(paste("download for year", year, "done!"))
}

## Input: Year
## Output: List of variables names, to be extracted from raw data.
Var_List_Assembler = function(year){
  # Based on year, we shall give a list of variables to be used for data extraction.
  # Input: year
  # output: a list of characters corresponding to variable names
  # Sources: Yearly Codebook.
  # This part is extremely long because the data had variable name changes almost every year.
  
  # This is first part of variable list to be used.
  # This is mainly demographic and basic health condition
  Base_All = c( #Demographic First
    "_STATE", "IYEAR", "SEQNO", 
    "AGE", "RACE2", "EDUCA", 
    "EMPLOY", "INCOME2", "CTYCODE",
    "SEX", "_MSACODE",
    # Health Care Access and health condition
    "GENHLTH", "HLTHPLAN", "MEDCOST", "CHECKUP")
  
  # This is second part of the variable.
  # There includes basics in dental conditions, and sometimes insurance/dentist visit information.
  Dent_All = c("LASTDEN2", "RMVTEETH", "DENCLEAN")
  Dent_Add = c("DENTLINS", "REASDENT") #Only used in '01 and '02
  
  if(year == 1999){
    # For 1999. Change the name for "RACE" and add "HISPANIC" to calculate race/ethnicity category
    Base_All[which(Base_All == "RACE2")] <- "ORACE"
    Var_List = c(Base_All, "HISPANIC")
    Var_List = c(Var_List, Dent_All)
  }else if(year == 2000){
    # For 2000. Same as above, but more variables in dentistry
    Base_All[which(Base_All == "RACE2")] <- "ORACE"
    Var_List = c(Base_All, "HISPANIC")
    Var_List = c(Var_List, Dent_All, Dent_Add)
  }else if (year == 2001){
    Var_List = c(Base_All, Dent_All, Dent_Add)
  }else if (year == 2002){
    # Change one variable name for 2002
    Base_All[which(Base_All == "MEDCOST")] <- "MEDCARE"
    Base_All = c(Base_All, "MEDREAS")
    Var_List = c(Base_All, Dent_All)
  }else if (year == 2003 | year == 2004){
    # MSA Code no longer available in 2003 and 2004.
    # No "Checkup" (last time of routine checkup) for 2003 and 2004
    Base_All = Base_All[-which(Base_All == "_MSACODE")]
    Var_List = c(Base_All[-which(Base_All == "CHECKUP")], Dent_All)
  }else if (year == 2005){
    # Change of variable names. 
    Dent_All[which(Dent_All == "LASTDEN2")] <- "LASTDEN3"
    Var_List = c(Base_All, "MSCODE", Dent_All)
  }else if (year == 2006){
    Dent_All[which(Dent_All == "LASTDEN2")] <- "LASTDEN3"
    Dent_All[which(Dent_All == "RMVTEETH")] <- "RMVTETH3"
    Var_List = c(Base_All[-which(Base_All == "_MSACODE")], "MSCODE", Dent_All)
  }else{
    # From 2006 to 2018.
    
    base_pre_2011 = c(
      "_STATE", "IYEAR", "SEQNO", 
      "AGE", "RACE2", "EDUCA", "EMPLOY", "INCOME2", "CTYCODE", "SEX", "MSCODE",
      "GENHLTH", "HLTHPLAN", "MEDCOST", "CHECKUP1")
    
    base_2011_and_2012 = c(
      "_STATE", "IYEAR", "SEQNO", 
      "AGE", "RACE2", "EDUCA", "EMPLOY", "INCOME2", "CTYCODE1", "SEX", "MSCODE",
      "GENHLTH", "HLTHPLN1", "MEDCOST", "CHECKUP1")
    
    base_post_2012 = c(
      "_STATE", "IYEAR", "SEQNO", 
      "_AGEG5YR", "_RACE", "EDUCA", "EMPLOY1", "INCOME2", "SEX", "MSCODE",
      "GENHLTH", "HLTHPLN1", "MEDCOST", "CHECKUP1")
    
    base_2018 = c(
      "_STATE", "IYEAR", "SEQNO",
      "_AGEG5YR", "_RACE", "EDUCA", "EMPLOY1", "INCOME2", "SEX1", "MSCODE",
      "GENHLTH", "HLTHPLN1", "MEDCOST", "CHECKUP1")
    
    base_post_2018 = c( #Demographic First
      "_STATE", "IYEAR", "SEQNO",
      "_AGEG5YR", "_RACE", "EDUCA", "EMPLOY1", "INCOME2", "SEXVAR", "MSCODE",
      "GENHLTH", "HLTHPLN1", "MEDCOST", "CHECKUP1")
    
    
    
    dental_pre_10 = c("LASTDEN3", "RMVTETH3", "DENCLEAN")
    dental_2010_to_2018 = c("LASTDEN3", "RMVTETH3")
    dental_post_18 = c("LASTDEN4", "RMVTETH4")
    if(year < 2011){
      Var_List = base_pre_2011
    }else if(year < 2013){
      Var_List = base_2011_and_2012
    }else if(year < 2018){
      Var_List = base_post_2012
    }else if(year == 2018){
      Var_List = base_2018
    }else{
      Var_List = base_post_2018
    }
    if(year %% 2 == 0){
      #Dental Data available only in even years.
      if(year <= 2010){
        Var_List = c(Var_List, dental_pre_10)
      }else if(year < 2018){
        Var_List = c(Var_List, dental_2010_to_2018)
      }else{
        Var_List = c(Var_List, dental_post_18)
      }
    }
  }
  return(Var_List)
  
}


## This is gonna appear as the final list of variables in dataset output.

Final_List_of_Variables = c( 
  "STATE_FIPS_CODE", #Peviously _STATE
  "YEAR_RESPONSE", #Previously IYEAR
  "SEQ_NO", #Previously SEQNO
  "AGE", #Previously AGE
  "AGE_5_YR_GAP", #Previously "_AGEG5YR"
  "RACE", #Previously Various Race
  "EDUCA",
  "EMPLOY_STATUS", #Previously various employment stuff
  "ANNUAL_INCOME_HSHLD", #Annual Household Income
  "COUNTY_CODE", #FIPS County Code
  "SEX", #Sex
  "MSCODE", #MS Code
  "MSA_CODE", #MSA Code of entry
  "GEN_HEALTH", #General Health 
  "HEALTH_PLAN_STATUS", #Health Plan Status
  "MED_COST", #Failed to see doc due to cost,
  "CHECKUP_MOST_RECENT", #Most Recent Checkup
  "DENTIST_VISIT_LAST", #Last visit to dentist's practice
  "NO_TEETH_RMVD", #Number of teeth removed due to gum disease etc.
  "DENTIS_CLEAN_LAST", #Last time had teeth cleaned
  "DENTLINS", # Dental Insurance coverage. Only in '00 and '01
  "REASDENT" #why didn't visit dental clinic last year?", only in '00 and '01
)


# This is 2-D list of possible names that are actually same variables.
# They will be renamed into corresponding names in Final_List_of_Variables so it can be used later.
# Corresponding it: all names included in Names_to_be_used[i] 
# shall thus be changed to Final_List_of_Variables[i]

Names_to_be_used = list(
  list("_STATE"),
  list("IYEAR"),
  list("SEQNO"),
  list("AGE"),
  list("_AGEG5YR"),
  list("_RACE", "RACE2"), 
  list("EDUCA"),
  list("EMPLOY", "EMPLOY1"),
  list("INCOME2"),
  list("CTYCODE"),
  list("SEX", "SEX1", "SEXVAR"),
  list("MSCODE"),
  list("_MSACODE"),
  list("GENHLTH"),
  list("HLTHPLAN", "HLTHPLN1"),
  list("MEDCOST"),
  list("CHECKUP", "CHECKUP1"),
  list("LASTDEN2", "LASTDEN3", "LASTDEN4"),
  list("RMVTEETH", "RMVTETH3", "RMVTETH4"),
  list("DENCLEAN"),
  list("DENTLINS"),
  list("REASDENT")
)

# List of Labels used for the variables
Labels_for_Final_Variables = c(
  "FIPS Code for Each State",
  "Year of the entry",
  "BRFSS Sequence Number",
  "Respondent Age",
  "Respondent Age in 5 Year Gap. See BRFSS codebook any year for specific numbers' meaning. -1 for not available in year.",
  "Race of Respondent.  See BRFSS codebook post 2014 for specific numbers' meaning. -1 for not available in year.", 
  "Level of Education. See BRFSS codebook any year for specific numbers' meaning. -1 for not available in year.",
  "Employment Status.  See BRFSS codebook any year for specific numbers' meaning. -1 for not available in year.",
  "Annual Household Income. See BRFSS codebook any year for specific numbers' meaning. -1 for not available in year.", 
  "FIPS County Code",
  "Sex(gender) of respondent. See BRFSS codebook 2019 for specific numbers' meaning. -1 for not available in year.",
  "Code indicating if is in MSA or not.  See BRFSS codebook any year (2012 recommended) for specific numbers' meaning. -1 for not available in year.",
  "FIPS Code for MSA of the respondent",
  "Self Evaluation on General Health.  See BRFSS codebook any year for specific numbers' meaning. -1 for not available in year.",
  "Availability of Health Plan.  See BRFSS codebook any year for specific numbers' meaning. -1 for not available in year.",
  "Have the respondent failed to see a doctor due to COST issue in past year?  See BRFSS codebook any year for specific numbers' meaning. -1 for not available in year.",
  "Most Recent Checkup.  See BRFSS codebook any year for specific numbers' meaning. -1 for not available in year.",
  "Last Visit to dentists' practice.",
  "Number of teeth removed due to gum disease or decay",
  "Last time have teeth cleaned by dentists or dental hygienists",
  "Coverage of Dental Insurance. Available only in '01 and '00",
  "Reason why the respondent didn't visit a dentists' practice last year? Available only in '00 and '01"
)


# This function fixs issues in data and calculate variables.
data_processor = function(input_data, year, additional_variables){
  # This function changes the name to proper ones, and add columns for later rbind./
  # Firstly, if year is 2002, change MEDCOST
  temp = input_data
  # for(i in 1:ncol(temp)){
  #   temp[, i] = as.numeric(unlist(temp[, i]))
  # }
  temp = sapply(temp, as.numeric)
  temp = as.data.frame(temp)
  
  # First, let's deal MEDCOST (have you, in past year, failed to receive medical care due to cost concerns?)
  if(year == 2002){
    # Idea: Calculate MEDCOST as:
    # 1 if answered 1 for both.
    # 2: Answered 2 for Q1, or answered 1 in Q1 and 2-10 in Q2.
    # 7 if 7 in Q1, or 1 in Q1 and 77 in Q2
    # 9 if 9 in Q1 or 1 in Q1 and 99 in Q2
    # Blank for the rest.
    temp = temp %>% mutate(MEDCOST = NA) %>% 
      mutate( MEDCOST = case_when(
        (MEDCARE == 1 & MEDREAS == 1) ~ 1,
        (MEDCARE == 2 | (MEDCARE == 1 & MEDREAS >= 2 & MEDREAS <= 10)) ~ 2,
        (MEDCARE == 7 | (MEDCARE == 1 & MEDREAS == 77)) ~ 7,
        (MEDCARE == 9 | (MEDCARE == 1 & MEDREAS == 99)) ~ 9))
    
    temp = temp[ , -which(names(temp) %in% c("MEDCARE", "MEDREAS"))]
    # This shall fix the issue about MEDCOST in 2002
  }
  #Secondly, seperate age to 5-year gaps
  if(!("_AGEG5YR" %in% colnames(temp)) & ("AGE" %in% colnames(temp))){
    temp = temp %>% 
      mutate('_AGEG5YR' = case_when(
        (AGE == 18| AGE == 19) ~ 1,
        AGE >= 80 ~ 13,
        (AGE == 7 | AGE == 9) ~ 14,
        TRUE ~ AGE %/% 5 - 3
      ))
    # This shall fix age issue
  }
  # Finally, deal with Race/Ethnicity stuff
  if(year <= 2000){
    temp = temp %>% mutate(
      RACE2 = case_when(
        (HISPANIC == 2 & ORACE == 1) ~ 1, #Non Hispanic White
        (HISPANIC == 2 & ORACE == 2) ~ 2, #Non Hispanic African
        (HISPANIC == 2 & ORACE == 4) ~ 3, #Non Hispanic American Indian
        (HISPANIC == 2 & ORACE == 3) ~ 4, #Non Hispanic Asian and Pacific Islander
        # They mixed Asian with Pacific Islander....I'll put 'em in Asian.
        # 5 was supposed to be Pacific Islander but....they dropped it.
        (HISPANIC == 2 & ORACE == 5) ~ 6, #Non Hispanic Other
        #There was a 7 Multicultural after 2013, but I cannot find correspond.
        (HISPANIC == 1) ~ 8, #Hispanic
        TRUE ~ 9
        # No need to change them in code below: they were in 2020 standard.
      )
    )
    temp = temp[ , -which(names(temp) %in% c("ORACE", "HISPANIC"))]
  }else if(year <= 2012){
    temp = temp %>% mutate(
      RACE2 = case_when(
        RACE2 == 3 ~ 4, #Non Hispanic Asian became coded as 4 in and after '13
        RACE2 == 5 ~ 3, #Non Hispanic American Indian became coded as 3 in and after '13
        RACE2 == 4 ~ 5 #Non Hispanic Native Hawaiian and Pacific Islander became coded as 5 in and after '13
        # Imagine being a Hispanic "Native" Hawaiian.
      ))
  }
  return(temp)
}

# This function changes name of variables and re-arrange column orders for easier later 
# It also fills -1 in columns NOT included in the year's data.
name_changer_gap_filler = function(input_data){
  temp = input_data
  for(i in 1:length(Final_List_of_Variables)){
    for(j in 1:length( Names_to_be_used[[i]] )){
      names(temp)[names(temp) == as.character(Names_to_be_used[[i]] [[j]])] <- as.character(Final_List_of_Variables[[i]])
    }
  }
  temp[setdiff(Final_List_of_Variables, colnames(temp))] = -1
  temp = temp[, Final_List_of_Variables]
  return(temp)
}



######
### FINALLY, ALL THINGS ARE DONE. WE CAN BEGIN WORKING.
###

#Create a directory to put these data in.
if(!dir.exists("BRFSS_Data_Unzip_Folder")){
  dir.create("BRFSS_Data_Unzip_Folder")
}

### For recording purose
Have_Unzipped_File = 1
Have_Downloaded_File = 1

if(Keep_Yearly == 1 | Keep_Merged == 1){
  if(!dir.exists("BRFSS_Data_Processed_Folder")){
    dir.create("BRFSS_Data_Processed_Folder")
  }
}

if(!file.exists(Get_Unzipped_File_Name( Download_Year_Start ) ) ){
  Have_Unzipped_File = 0
  
  # Well, let's see if you downloaded the file then.
  if(! file.exists( paste("./BRFSS_Data_Unzip_Folder/BRFSS_data_", Download_Year_Start, ".zip", sep = "") ) ){
    # Ooof! You gotta have a painful time downloading it.
    Have_Downloaded_File = 0
    Downloader(Download_Year_Start)
  }
  # Let's unzip it/
  unzip(zipfile = paste("./BRFSS_Data_Unzip_Folder/BRFSS_data_", Download_Year_Start, ".zip", sep = ""),
        exdir = "./BRFSS_Data_Unzip_Folder")
}


result = read_xpt(file = Get_Unzipped_File_Name(Download_Year_Start))[, Var_List_Assembler(Download_Year_Start)]
result = data_processor(input_data = result, year = Download_Year_Start)
result = name_changer_gap_filler(result)

for(i in 1:ncol(result)){
  attr(result[, which(colnames(result) == as.character(Final_List_of_Variables[[i]]) )], "label") <- Labels_for_Final_Variables[i]
}

# Delete your files if you hate having it

if(Have_Unzipped_File == 0 & Keep_Unzipped != 1){
  unlink(Get_Unzipped_File_Name( Download_Year_Start ))
}
if(Have_Downloaded_File == 0 & Keep_Download != 1){
  unlink( paste("./BRFSS_Data_Unzip_Folder/BRFSS_data_", Download_Year_Start, ".zip", sep = "") )
}

if(Keep_Yearly == 1){
  # If you want to keep yearly result, we shall write it down
  if(CSV_OR_DTA == 2 | CSV_OR_DTA == 3){
    # So you want CSV file.
    write.csv(result, file = paste("./BRFSS_Data_Processed_Folder/Processed_BRFSS_", Download_Year_Start, ".csv", sep = ""))
  }
  
  if(CSV_OR_DTA == 1 | CSV_OR_DTA == 3){
    # So you want DTA file.
    write_dta(data = result, path = paste("./BRFSS_Data_Processed_Folder/Processed_BRFSS_", Download_Year_Start, ".dta", sep = ""))
  }
}




# Do the download etc. Year by year
for(year in (Download_Year_Start + 1):Download_Year_End){
  if((Download_Year_Start + 1) > Download_Year_End){
    break
  }
  ### For recording purose
  Have_Unzipped_File = 1
  Have_Downloaded_File = 1
  
  if(!file.exists(Get_Unzipped_File_Name( year ) ) ){
    Have_Unzipped_File = 0
    
    # Well, let's see if you downloaded the file then.
    if(! file.exists( paste("./BRFSS_Data_Unzip_Folder/BRFSS_data_", year, ".zip", sep = "") ) ){
      # Ooof! You gotta have a painful time downloading it.
      Have_Downloaded_File = 0
      Downloader(year)
    }
    # Let's unzip it/
    unzip(zipfile = paste("./BRFSS_Data_Unzip_Folder/BRFSS_data_", year, ".zip", sep = ""),
          exdir = "./BRFSS_Data_Unzip_Folder")
  }
  
  
  new_result = read_xpt(file = Get_Unzipped_File_Name(year))[, Var_List_Assembler(year)]
  new_result = data_processor(input_data = new_result, year = year)
  new_result = name_changer_gap_filler(new_result)
  if(Keep_Merged == 1){
    # If you need merged file, merge with older ones
    result = rbind(result, new_result)
  }
  
  # Delete your files if you hate having it
  
  if(Have_Unzipped_File == 0 & Keep_Unzipped != 1){
    unlink(Get_Unzipped_File_Name( year ))
  }
  if(Have_Downloaded_File == 0 & Keep_Download != 1){
    unlink( paste("./BRFSS_Data_Unzip_Folder/BRFSS_data_", year, ".zip", sep = "") )
  }
  
  for(i in 1:ncol(result)){
    attr(new_result[, which(colnames(new_result) == as.character(Final_List_of_Variables[[i]]) )], "label") <- Labels_for_Final_Variables[i]
  }
  
  if(Keep_Yearly == 1){
    # Keep processed yearly file if you need it.
    if(CSV_OR_DTA == 2 | CSV_OR_DTA == 3){
      # So you want CSV file.
      write.csv(new_result, file = paste("./BRFSS_Data_Processed_Folder/Processed_BRFSS_", year, ".csv", sep = ""))
    }
    
    if(CSV_OR_DTA == 1 | CSV_OR_DTA == 3){
      # So you want DTA file.
      write_dta(data = new_result, path = paste("./BRFSS_Data_Processed_Folder/Processed_BRFSS_", year, ".dta", sep = ""))
    }
  }
  print(paste("Year", year, "Data downloading and processing done!"))
}

if(Keep_Merged == 1){
  # Write down merged file, if you need it.
  for(i in 1:ncol(result)){
    attr(result[, which(colnames(result) == as.character(Final_List_of_Variables[[i]]) )], "label") <- Labels_for_Final_Variables[i]
  }
  
  if(CSV_OR_DTA == 2 | CSV_OR_DTA == 3){
    # So you want CSV file.
    write.csv(result, file = paste("./BRFSS_Data_Processed_Folder/Processed_BRFSS_", Download_Year_Start, "_to_", Download_Year_End, ".csv", sep = ""))
  }
  
  if(CSV_OR_DTA == 1 | CSV_OR_DTA == 3){
    # So you want DTA file.
    write_dta(data = result, path = paste("./BRFSS_Data_Processed_Folder/Processed_BRFSS_", Download_Year_Start, "_to_", Download_Year_End, ".dta", sep = ""))
  }
  print("Merged Dataset Output Complete. Please check BRFSS_Data_Processed_Folder for it.")
}

