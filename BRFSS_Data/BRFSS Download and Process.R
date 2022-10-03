library("readxl")
library("dplyr")
library("haven") #To export DTA
library(tidyr)

#####
# This script reads the data from BRFSS and compile them into several files
# The script might take over 30 minutes to download, run, and compile the stuff.
# The total size required can go up to 5 Gigabyte.


### WARNING: THIS SCRIPT IS FOR TESTING PURPOSE ONLY.
## DO NOT ATTEMPT TO RUN IT IN FULL....
## Feel free to grab some functions though.


######
# Part 1: Download data


#Create a directory to put these data in.
if(!dir.exists("BRFSS_Data_Unzip_Folder")){
  dir.create("BRFSS_Data_Unzip_Folder")
}


### Warning: following code might take up to 5 GB of space and 30 minutes of time.

# Download 1999 data
download.file(url = "https://www.cdc.gov/brfss/annual_data/1999/files/CDBRFS99XPT.zip",
              destfile = "./BRFSS_Data_Unzip_Folder/BRFSS_data_1999.zip", mode = "wb")

# Download from 2000 to 2020
for(year in 2000:2020){
  if(year <= 2009 & year >= 2000){
    download.file(url = paste("https://www.cdc.gov/brfss/annual_data/", year, "/files/CDBRFS0", year - 2000, "XPT.zip", sep = ""),
                  destfile = paste("./BRFSS_Data_Unzip_Folder/BRFSS_data_", year, ".zip", sep = ""), mode = "wb")
  }else if(year == 2010){
    download.file(url = "https://www.cdc.gov/brfss/annual_data/2010/files/CDBRFS10XPT.zip",
                  destfile = "./BRFSS_Data_Unzip_Folder/BRFSS_data_2010.zip", mode = "wb")
  } else{
    download.file(url = paste("https://www.cdc.gov/brfss/annual_data/", year, "/files/LLCP", year, "XPT.zip", sep = ""),
                  destfile = paste("./BRFSS_Data_Unzip_Folder/BRFSS_data_", year, ".zip", sep = ""), mode = "wb")
  }
  print(paste("download for year", year, "done!"))
}


#### Unzip the file
for(year in 1999:2020){
  unzip(zipfile = paste("./BRFSS_Data_Unzip_Folder/BRFSS_data_", year, ".zip", sep = ""),
        exdir = "./BRFSS_Data_Unzip_Folder")
}


file_Reader = function(year){
  # This function reads the data based on the year given.
  # Input: year
  # Output: dataset, untreated.
  # Warning: output can be huge, > 100,000 lines in size and 200 columns.
  
  if(year == 2002 | year == 2003){
    temp = read_xpt(file = paste("./BRFSS_Data_Unzip_Folder/cdbrfs0", year - 2000, ".xpt", sep = ""))
  }else if(year <= 2010){
    temp = read_xpt(file = paste("./BRFSS_Data_Unzip_Folder/CDBRFS", substr(as.character(year), 3, 4), ".XPT", sep = ""))
  }else{
    temp = read_xpt(file = paste("./BRFSS_Data_Unzip_Folder/LLCP", year, ".XPT", sep = ""))
  }
  return(temp)
}

# For 1999 data, we shall include: state (_STATE), Year of Interview (IYEAR), Interviewer ID (INTVID),
# Sequence number (SEQNO), Number of Adult (NUMADULT), Has any health plan? (HLTHPLAN), 
# Have MediCare? (MEDICAR2), what type of cover? TYPCOVR,
# Could not afford seeing doctor in past 12 months? (MEDCOST), Time since last checkup? (CHECKUP?)
# Last time visited dental clinic? (LASTDEN2)
# Number of Permanant Teeth removed? (RMVTEETH)
# Last time teeth cleaned by dentist? (DENCLEAN)
# Reported Age (AGE)
# Reported Race (ORACE)
# Education Level (EDUCA)
# Employment Status (EMPLOY)
# Income Level (INCOME2)
# County Code (CTYCODE)
# Sex/Gender (SEX)
# Is there a list of doctors for the plan? (DOCTLIST)
# Self report satisfaction of health care? (RATECARE)
# FIPS Metropolitan Code (_MSACODE)

# For year 2000 data, these variables shall be included:
# States (_STATE), year of interview (IYEAR),  Interviewer ID (INTVID),
# Sequence number (SEQNO), Number of Adult (NUMADULT), 
# Self-Evalauted General Health (GENHLTH)
# Have any health plan? (HLTHPLAN)
# Have MediCare? (MEDICAR2), 
# If not Medicare, what kind of coverage? (TYPCOVR1)
# Could not afford seeing doctor in past 12 months? (MEDCOST), Time since last checkup? (CHECKUP)
# Last time visited dental clinic? (LASTDEN2)
# Number of Permanant Teeth removed? (RMVTEETH)
# Last time teeth cleaned by dentist? (DENCLEAN)
# Reason didn't go to see a dentist? (REASDENT)
# Do you have dental insurance plans? Include prepaid and MedicAid (DENTLINS) 
# Reported Age (AGE)
# Reported Race (ORACE)
# Education Level (EDUCA)
# Employment Status (EMPLOY)
# Income Level (INCOME2)
# County Code (CTYCODE)
# Sex/Gender (SEX)
# Is there a list of doctors for the plan? (DOCTLIST)
# Self report satisfaction of health care? (RATECARE)
# FIPS Metropolitan Code (_MSACODE)


# In 2001: ORACE replaced by ORACE2 (Self-select for race)
# Questions about if they have medicare, and type of providers if have 
# insurance other than medicare, was skipped in 2001

# In 2002: MEDCOST was not available, but swapped to:
# Was there a time in the past 12 months when you needed medical care, but could not get it? (MEDCARE)
# If yes, why? (MEDRES)
# REASDENT (Why didn't see dentist) and "DENTLINS" (have insurance or not) was not included in 2002

# In 2003:"CHECKUP" no longer included
# Dental Condition: Same as 2002

#In 2004: Still no "CHECKUP"

# 2005: "CHECKUP" returned. Still only three variables for dental (LASTDEN2, RMVTEETH, DENCLEAN)
# Name of LASTDEN2 changed to LASTDEN3

# 2006: Still have CHECKUP
# Name of LASTDEN2 changed to LASTDEN3
# RMVTEETH changed to RMVTETH3 (Number of teeth removed due to tooth decay or gum disease)

# 2007: Name of CHECKUP changed to CHECKUP1
# No dental health condition for 2007

# 2008: Use name CHECKUP1
# Used name LASTDEN3, RMVTETH3, DENCLEAN

# 2009: Dental Health Not Included

# 2010: Same as '08

# 2011: Name for Health Plan Coverage (HLTHPLAN) changed to HLTHPLN1. Used CHECKUP1 for checkup.
# No dental data

# 2012: Same as '11. No longer have DENCLEAN

# 2013: EMPLOY changed to EMPLOY1. No more county. Only 5 year age.

# 2014: Same as '11. No longer have DENCLEAN

# 2015: Same as before. No Dental Data

# 2016: Same as '12. No longer have DENCLEAN. Only RMVTETH3 and LASTDEN3

# 2017: Same. No dental data

# 2018: Same. LASTDEN3 name changed to LASTDEN4. RMVTETH3 changed to RMVTETH4

# 2019: Sams as '18. No dental data

# 2020: Sams as '18. Have dental data.



## The following function intakes a year and output a list of variables to be used.
Var_List_Assembler = function(year){
  # Based on year, we shall give a list of variables to be used for data extraction.
  # This is gross. They changed one code every year and we are d*mned.
  # Input: year
  # output: a list of characters corresponding to variable names
  # Sources: Yearly Codebook.
  
  # This should work for 2001 etc.
  Base_All = c( #Demographic First
    "_STATE", "IYEAR", "SEQNO", 
    "AGE", "RACE2", "EDUCA", 
    "EMPLOY", "INCOME2", "CTYCODE",
    "SEX", "_MSACODE",
    # Health Care Access and health condition
    "GENHLTH", "HLTHPLAN", "MEDCOST", "CHECKUP")
  
  Dent_All = c("LASTDEN2", "RMVTEETH", "DENCLEAN")
  Dent_Add = c("DENTLINS", "REASDENT") #Only used in '01 and '02
  
  if(year == 1999){
    Base_All[which(Base_All == "RACE2")] <- "ORACE"
    Var_List = c(Base_All, "HISPANIC")
    Var_List = c(Var_List, Dent_All)
  }else if(year == 2000){
    Base_All[which(Base_All == "RACE2")] <- "ORACE"
    Var_List = c(Base_All, "HISPANIC")
    Var_List = c(Var_List, Dent_All, Dent_Add)
  }else if (year == 2001){
    Var_List = c(Base_All, Dent_All, Dent_Add)
  }else if (year == 2002){
    Base_All[which(Base_All == "MEDCOST")] <- "MEDCARE"
    Base_All = c(Base_All, "MEDREAS")
    Var_List = c(Base_All, Dent_All)
  }else if (year == 2003 | year == 2004){
    Base_All = Base_All[-which(Base_All == "_MSACODE")]
    Var_List = c(Base_All[-which(Base_All == "CHECKUP")], Dent_All)
  }else if (year == 2005){
    Dent_All[which(Dent_All == "LASTDEN2")] <- "LASTDEN3"
    Var_List = c(Base_All, "MSCODE", Dent_All)
  }else if (year == 2006){
    Dent_All[which(Dent_All == "LASTDEN2")] <- "LASTDEN3"
    Dent_All[which(Dent_All == "RMVTEETH")] <- "RMVTETH3"
    Var_List = c(Base_All[-which(Base_All == "_MSACODE")], "MSCODE", Dent_All)
  }else{
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


## WARNING: THE FOLLOWING FUCNTION IS FOR TESTING PURPOSE ONLY
## This function intakes a year and output a list of variables to be used.
## The variables here were compiled manually.
## All variables here has been tested: they are all contained in the corresponding year's dataset.
## This part spans 150 lines so it's advised to keep it closed in R Studio.
Var_List_Assembler_Backup = function(year){
  
  if(year == 1999){
    Var_List = c("_STATE", "IYEAR", "SEQNO", 
                 "AGE", "ORACE", "HISPANIC",
                 "EDUCA", "EMPLOY", "INCOME2", 
                 "CTYCODE", "SEX", "_MSACODE", 
                 
                 # "MEDICAR2", "TYPCOVR1", "TYPCOVR2", This was not included later on so I deleted it.
                 
                 "GENHLTH", "HLTHPLAN", "MEDCOST", "CHECKUP", 
                 "LASTDEN2", "RMVTEETH", "DENCLEAN")
  }else if (year == 2000){
    Var_List = c( #Demographic First
      "_STATE", "IYEAR", "SEQNO", 
      "AGE", "ORACE", "HISPANIC", "EDUCA", "EMPLOY", "INCOME2", "CTYCODE", "SEX", "_MSACODE",
      # Health Care Access and health condition
      "GENHLTH", "HLTHPLAN", "MEDCOST", "CHECKUP", 
      
      # Next line for dental related
      "LASTDEN2", "RMVTEETH", "DENCLEAN",
      # Additional: Only in year 2000 and 2001
      "DENTLINS", "REASDENT"
    )
  }else if (year == 2001){
    Var_List = c( #Demographic First
      "_STATE", "IYEAR", "SEQNO", 
      "AGE", "RACE2", "EDUCA", "EMPLOY", "INCOME2", "CTYCODE", "SEX", "_MSACODE",
      # Health Care Access and health condition
      "GENHLTH", "HLTHPLAN", "MEDCOST", "CHECKUP", 
      
      # Next line for dental related
      "LASTDEN2", "RMVTEETH", "DENCLEAN",
      "DENTLINS", "REASDENT"
    )
  }else if (year == 2002){
    Var_List = c( #Demographic First
      "_STATE", "IYEAR", "SEQNO", 
      "AGE", "RACE2", "EDUCA", "EMPLOY", "INCOME2", "CTYCODE", "SEX", "_MSACODE",
      # Health Care Access and health condition
      "GENHLTH", "HLTHPLAN", "MEDCARE", "MEDREAS", "CHECKUP", 
      # MEDCARE and MEDRES replaced the old MEC
      # Next line for dental related
      "LASTDEN2", "RMVTEETH", "DENCLEAN"
    )
  }else if (year == 2003 | year == 2004){
    Var_List = c( #Demographic First
      "_STATE", "IYEAR", "SEQNO", 
      "AGE", "RACE2", "EDUCA", "EMPLOY", "INCOME2", "CTYCODE", "SEX", 
      # Health Care Access and health condition
      # 2003 and 2004 data had no MSA.
      
      "GENHLTH", "HLTHPLAN", "MEDCOST", 
      # No more "Checkup" category.
      # Next line for dental related
      "LASTDEN2", "RMVTEETH", "DENCLEAN"
    )
  }else if (year == 2005){
    Var_List = c( #Demographic First
      "_STATE", "IYEAR", "SEQNO", 
      "AGE", "RACE2", "EDUCA", "EMPLOY", "INCOME2", "CTYCODE", "SEX", "MSCODE", "_MSACODE",
      # Health Care Access and health condition
      
      "GENHLTH", "HLTHPLAN", "MEDCOST", "CHECKUP",
      # No more "Checkup" category.
      # Next line for dental related
      "LASTDEN3", "RMVTEETH", "DENCLEAN"
    )
  }else if (year == 2006){
    Var_List = c( #Demographic First
      "_STATE", "IYEAR", "SEQNO", 
      "AGE", "RACE2", "EDUCA", "EMPLOY", "INCOME2", "CTYCODE", "SEX", "MSCODE",
      # Health Care Access and health condition
      # Only have MS code, indicating if it's in an MSA
      
      "GENHLTH", "HLTHPLAN", "MEDCOST", "CHECKUP",
      # No more "Checkup" category.
      # Next line for dental related
      "LASTDEN3", "RMVTETH3", "DENCLEAN"
    )
  }else{
    base_pre_2011 = c( #Demographic First
      "_STATE", "IYEAR", "SEQNO", 
      "AGE", "RACE2", "EDUCA", "EMPLOY", "INCOME2", "CTYCODE", "SEX", "MSCODE",
      # Health Care Access and health condition
      # Only have MS code, indicating if it's in an MSA
      
      "GENHLTH", "HLTHPLAN", "MEDCOST", "CHECKUP1")
      # No more "Checkup" category.
      # Next line for dental related)
    base_2011_and_2012 = c( #Demographic First
      "_STATE", "IYEAR", "SEQNO", 
      "AGE", "RACE2", "EDUCA", "EMPLOY", "INCOME2", "CTYCODE1", "SEX", "MSCODE",
      # Health Care Access and health condition
      # Only have MS code, indicating if it's in an MSA
      
      "GENHLTH", "HLTHPLN1", "MEDCOST", "CHECKUP1")
    # No more "Checkup" category.
    # Next line for dental related)
    
    base_post_2012 = c( #Demographic First
      "_STATE", "IYEAR", "SEQNO", 
      "_AGEG5YR", "_RACE", "EDUCA", "EMPLOY1", "INCOME2", "SEX", "MSCODE",
      # Health Care Access and health condition
      # Only have MS code, indicating if it's in an MSA
      
      "GENHLTH", "HLTHPLN1", "MEDCOST", "CHECKUP1")
    # 
    base_2018 = c( #Demographic First
      "_STATE", "IYEAR", "SEQNO",
      "_AGEG5YR", "_RACE", "EDUCA", "EMPLOY1", "INCOME2", "SEX1", "MSCODE",
      # Health Care Access and health condition
      # Only have MS code, indicating if it's in an MSA
      # CTYCODE is all blank

      "GENHLTH", "HLTHPLN1", "MEDCOST", "CHECKUP1")
    base_post_2018 = c( #Demographic First
      "_STATE", "IYEAR", "SEQNO",
      "_AGEG5YR", "_RACE", "EDUCA", "EMPLOY1", "INCOME2", "SEXVAR", "MSCODE",
      # Health Care Access and health condition
      # Only have MS code, indicating if it's in an MSA
      # CTYCODE is all blank
      
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


### This is just for checking if variable names are correct.
for(i in 1999:2020){
  print(setdiff(Var_List_Assembler_Backup(i), Var_List_Assembler(i)))
  print(setdiff(Var_List_Assembler(i), Var_List_Assembler_Backup(i)))
}

####
# We shall standardize all respondent to the following:
# _State: FIPS code for state
# IYEAR: Year
# SEQNO: Sequence Number for Identification.
# AGE: Age. NOT used after 2012

# _AGEG5YR: Age, by gap of five years. Used after 2012 at following scheme:
# 1 for 18-24, then add 1 every 5 years (25-29, 30-34, 35-39...12 for 75-79, 13 for 80 - 99, 14 for other)
# Same across all years
# Used because age is NOT available after 2012.

# _RACE: categorized race, used after 2013
# The variable was NOT included in 1999 and 2000
# It was included as RACE2 between 2001 and 2012
# WARNING: THEY USED A DIFFERENT SYSTEM
# Between 2001 and 2012, we have:
# 1,2 for white and black Non-Hispanic, 3 for Asian, 4 for Pacific Islander, 5 for American Indian,
# 6 for other, 7 for multicultural, 8 for Hispanic, 9 for unsure

# But by 2013 we have:
# 1,2 for white and black, 3 for AMERICAN INDIAN, 4 for Asian, 5 for pacific islander, other same.
# We shall use post-2012 standard.
# I think they are just against Asians.

# EDUCA: Level of Education. 1 to 6 for no education, primary school, some high school (grade 9-11),
# High School Graduate, Some College/Technical Schools, College Graduate. 9 for other.
# Surprisingly same for all data.

# EMPLOY: Named Named EMPLOY1 from 2013 onwards.
# Formula: 1 Employed for wages, 2 self-employed, 3 out of work for 1 year or more, 4 out of work less than 1 year
# 5 homemaker 6 student 7 retired, 8 unable to work, 9 not responding.
# Same for all year

# INCOME2: Annual Household Income
# 1 to 8 for <$10K, 10-15K, 15-20K, 20-25K, 25-25K, 25-50K, 50-75K, >75K, 
# 77 NOT SURE, 99 REFUSED. Code didn't change over time.

# CTYCODE: FIPS code for county. NOT included in some years. kept as is.
# _MSACODE: FIPS code for MSA area of respondent. NOT included after '05.  Kept as is.

# SEX: Always 1 for male, 2 for female. 
# From 2016, they added 9: refused.
# From 2018, they added 7: Don't know/not sure
# From 2019, it was divided based on if respondent are on land line or cell. 
# Anyways I used SEXVAR: 1 for male, 2 for female

# MSCODE: if the respondent is in an MSA. NOT used in or before year 20
# 1 In center city, 2 Outside Center city but in county containing the center city,
# 3 in Suburban County, 4 No center city in MSA, 5 NOT in MSA, 6 Non applicable (in Puerto Rico, Virgin Island, Guam)
# Code stayed the same. Category "4" no longer appeared after 2014.

# GENHLTH: Self Evaluated General Health.
# The 2020 Standard is: 1 Excellent, 2 Very Good, 3 Good, 4 Fair, 5 Poor, 7 Don't Know/Unsure, 9 Refused, Blank (No Response/Not asked)

# HLTHPLAN/HLTHPLN1 (Second name used in and after 2011):
# Always, 1 yes (have health plan), 2 no, 7 don't know/not sure, 9 refused to answer, blank not asked.

# MEDCOST: Failed to see a doctor because of cost?
# Always, 1 yes, 2 no, 7 don't know/not sure, 9 refused to answer, blank not asked.

# It was not shown in 2002 but instead they used: "MEDCARE", "MEDREAS",
# It shall thus be cauculated as:
# MEDCARE: Was there a time in the past 12 months when you needed medical care, but could not get it?
# 1 for yes. Go to next question: MEDREAS. 7 don't know, 9 unsure
# MEDREAS: What is the main reason you did not get medical care? 
# 1 for cost. 77 don't know/unsure, 99 refused. blank: no answer
# Idea: Calculate MEDCOST as:
# 1 if answered 1 for both.
# 2: Answered 2 for Q1, or answered 1 in Q1 and 2-10 in Q2.
# 7 if 7 in Q1, or 1 in Q1 and 77 in Q2
# 9 if 9 in Q1 or 1 in Q1 and 99 in Q2
# Blank for the rest.

# CHECKUP: How long since last routine checkup?
# Always: 1 for within 1 year, 2 for within 1-2 year, 3 for within 2-5 year, 4 for 5 year or more, 
# 7 don't know/not sure, 8 never, 9 refuse to answer, blank: not asked

# LASTDEN2/LASTDEN3/LASTDEN4: Last time to a dentist's office?
# Always: 1 for within 1 year, 2 for within 1-2 year, 3 for within 2-5 year, 4 for 5 year or more, 
# 7 don't know/not sure, 8 never, 9 refuse to answer, blank: not asked

# RMVTEETH/RMVTETH3/RMVTETH4: How many teeth removed due to tooth decay or gum disease
# Always: 1 for less than 5, 2 for 6 but not all, 3 for all, 7 don't know/unsure,
# 8 none, 9 refused to answer, blank not asked

# DENCLEAN: Got your teeth cleaned by dentist or dental hygienist?
# NOT appearing after 10
# Always: 1 for less than 5, 2 for 6 but not all, 3 for all, 7 don't know/unsure,
# 8 Never, 9 refused to answer, blank not asked


# DENTLINS: Do you have dental insurance?
# REASDENT: why didn't visit dental clinic last year?
# These two were included only in 2001 and 2002. Just check codebook for interpretation.

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
  "DENTLINS", # Dental Insurance coverage. Only in '01 and '00
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


# 
# for(i in 1:length(Names_to_be_used)){
#   print(Final_List_of_Variables[i])
#   print(Names_to_be_used[i])
# }

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

# result is backup before name changer here.

if(!dir.exists("BRFSS_Data_Processed_Dump_Site")){
  dir.create("BRFSS_Data_Processed_Dump_Site")
}

result = file_Reader(1999)[, Var_List_Assembler(1999)]
result = data_processor(input_data = result, year = 1999)
result = name_changer_gap_filler(result)
write.csv(result, file = paste("./BRFSS_Data_Processed_Dump_Site/Processed_BRFSS_", 1999, ".csv", sep = ""))
for(year in 2000:2020){
  new_result = file_Reader(year)[, Var_List_Assembler(year)]
  new_result = data_processor(input_data = new_result, year = year)
  new_result = name_changer_gap_filler(new_result)
  write.csv(new_result, file = paste("./BRFSS_Data_Processed_Dump_Site/Processed_BRFSS_", year, ".csv", sep = ""))
  result = rbind(result, new_result)
  print(paste("Year", year, "Data cleaning and processing done!"))
}

write_dta(data = result, path = "./BRFSS_Data_Processed_Dump_Site/BRFSS_99_20_Selected_Var.dta")

for(i in 1:ncol(result)){
  attr(result[, which(colnames(result) == as.character(Final_List_of_Variables[[i]]) )], "label") <- Labels_for_Final_Variables[i]
}






