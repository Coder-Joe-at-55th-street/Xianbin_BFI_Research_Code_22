
library("readxl") #To read Excel
library("haven") #To export DTA


##This script grabs the forms of acceptance rate from each year and format them into ones easily merged.

#Do the 2009 first

raw_2009_applicant = read_excel(path = "ADEA_2009_Applicant_By_State.xls", .name_repair = "minimal")
# The raw data was in excel format with some useless columns. Drop them
colnames(raw_2009_applicant) = raw_2009_applicant[2, ]
raw_2009_applicant = raw_2009_applicant[3:60, ]
raw_2009_applicant$In_State_App = 1

# The 2009 data have had EACH state's applicant listed out. This is pretty useless. We just find total application
# And in-state application, then minus them.
for(i in 1:nrow(raw_2009_applicant)){
  raw_2009_applicant$In_State_App[i] = raw_2009_applicant[i, 
                                                          names(raw_2009_applicant) == raw_2009_applicant$State[i]]
}
raw_2009_applicant = raw_2009_applicant[, -(4:(ncol(raw_2009_applicant) - 1))]
colnames(raw_2009_applicant) = c("State", "School_Name", "Total_App", "In_State_App")

# Do the same for 2009 enrollees, which follows the same pattern as above.
raw_2009_enrollee = read_excel(path = "ADEA_2009_Enroll_By_State.xls", .name_repair = "minimal")
colnames(raw_2009_enrollee) = raw_2009_enrollee[2, ]
raw_2009_enrollee = raw_2009_enrollee[3:60, ]
raw_2009_enrollee$In_State_App = 1

for(i in 1:nrow(raw_2009_enrollee)){
  raw_2009_enrollee$In_State_App[i] = raw_2009_enrollee[i, 
                                                          names(raw_2009_enrollee) == raw_2009_enrollee$State[i]]
}
raw_2009_enrollee = raw_2009_enrollee[, -(4:(ncol(raw_2009_enrollee) - 1))]
colnames(raw_2009_enrollee) = c("State", "School_Name", "Total_Enroll", "In_State_Enroll")


# Merge to make 2009 dataset.
merge(raw_2009_applicant, raw_2009_enrollee[, -1], by = "School_Name") -> raw_adm_data_2009

#Swap the columns so they look better.
raw_adm_data_2009 = raw_adm_data_2009[, c("State", "School_Name", colnames(raw_adm_data_2009)[3:6])]

# Convert to numeric for later calculation
for(i in 3:6){
  raw_adm_data_2009[, i] = as.numeric(raw_adm_data_2009[, i])
}

# Find admission rate, in-state and total
raw_adm_data_2009$Total_Adm_Rate = raw_adm_data_2009[, 5] / raw_adm_data_2009[, 3]
raw_adm_data_2009$In_State_Adm_Rate = raw_adm_data_2009[, 6] / raw_adm_data_2009[, 4]


Yearly_Table_Adm_Rate_Extractor = function(year){
  #This function grabs the table and make some minor fixing
  
  file_name = paste("ADEA Dental School Applicants and Enrollees", year, "Entering Class.xlsx")
  raw = read_excel(path = file_name, sheet = "Table 7", .name_repair = "minimal")
  
  #To fix that 2017 table has "School State" instead of "State"
  raw[which(raw[, 1] == "School State"), 1] = "State"
  
  # Find the row where variable names would be stored, and drop everything above.
  raw = raw[-c(1: (which(raw[, 1] == "State") - 1)), ]
  raw = as.data.frame(raw)
  #Change the names to abbreviations

  if(!(raw[2,1] %in% state.abb) ){
    for(i in 2:nrow(raw)){
      if(is.na(raw[i, 1])){
        
      }else if(raw[i, 1] == "Puerto Rico"){
        raw[i, 1] = "PR"
      }else if(raw[i, 1] == "District of Columbia"){
        raw[i, 1] = "DC"
      } else {
        raw[i, 1] = state.abb[match(raw[i, 1], state.name)]
      }
    }
    raw = raw[!is.na(raw[, 1]), ]
  } else{
    for(i in 2:nrow(raw)){
      if(!is.na(raw[i, 1]) & raw[i, 1] == "CN"){
        raw[i, 1] = "CT"
        #They messed up state code for Conneticut in data of 2012
      }
    }
    raw = raw[-c(which(raw[, 1] == "WV") + 1: nrow(raw)), ]
  }
  #Each form has some useless columns (since it came from Excel)
  # so we are gonna drop them
  raw = raw[, !is.na(raw[1, ])]
  # Change the column names
  colnames(raw) = raw[1, ]
  raw = raw[-1, ]
  
  raw = raw[, -c(5, 8)] 
  #No need for pct of in-state application. We can calculate it in no time later
  # just took too much space
  
  for(i in 3:6){
    raw[, i] = as.numeric(raw[, i])
  }
  
  #Fix some issues. The data have problem: in 2015 they put Columbia University to Montana.
  for(i in 1:nrow(raw)){
    if(raw[i, 2] == "Columbia University College of Dental Medicine (CUL)"){
      raw[i, 1] = "NY"
    }
  }
  
  # Calculate acceptance rate etc.
  raw$Total_Accept_Rate = raw[, 5] / raw[, 3]
  raw$In_State_Accept_Rate = raw[, 6] / raw[, 4]
  colnames(raw) = c("State", "School_Name",
                    "Total_App", "In_State_App", "Total_Enroll", "In_State_Enroll",
                    "Total_Adm_Rate", "In_State_Adm_Rate")
  
  return(raw)
}


#This shall standardize all names of school for automatic comparison.
# Most of exceptions in school names are captured here.
School_Name_Standardizer = function(Input_Data){
  
  # Firstly, drop all comma and change all double-space to single-space. In this case we would have easier time
  # Comparing university names later on.
  Input_Data$School_Name_No_Comma = gsub(", ", "", 
                                                  gsub("([a-zA-Z]),", "\\1 ", 
                                                       Input_Data$School_Name))
  Input_Data$School_Name_No_Comma = gsub("  ", " ", 
                                                  gsub("([a-zA-Z]),", "\\1 ",  
                                                       Input_Data$School_Name_No_Comma))
  
  #This deletes the comma
  
  #Baylor is dentistry school of Texas A&M. Since we are replacing & later we must first deal with them here.
  
  Input_Data$School_Name_No_Comma = gsub("Baylor College of Dentistry", 
                                         "Texas A&M", Input_Data$School_Name_No_Comma)
  
  # Texas A&M and Texas A&M University is the same stuff
  Input_Data$School_Name_No_Comma = gsub("Texas A&M University", 
                                         "Texas A&M", Input_Data$School_Name_No_Comma)
  
  Input_Data$School_Name_No_Comma = gsub("-", " ", Input_Data$School_Name_No_Comma)
  Input_Data$School_Name_No_Comma = gsub("&", "and", Input_Data$School_Name_No_Comma)
  #This is for University of Arizona-Something
  Input_Data$School_Name_No_Comma = gsub("The ", "", Input_Data$School_Name_No_Comma)
  Input_Data$School_Name_No_Comma = gsub("the ", "", Input_Data$School_Name_No_Comma)
  #Drop these useless words
  
  Input_Data$School_Name_No_Comma = gsub("\\s*(\\([^()]*(?:(?1)[^()]*)*\\))", "", Input_Data$School_Name_No_Comma, perl=TRUE)
  #Drop things in parenthesis
  
  Input_Data$School_Name_No_Comma = gsub("NYU", "New York University", Input_Data$School_Name_No_Comma)
  Input_Data$School_Name_No_Comma = gsub("UT", "University of Texas", Input_Data$School_Name_No_Comma)
  Input_Data$School_Name_No_Comma = gsub("USC", "University of Southern California", Input_Data$School_Name_No_Comma)
  
  Input_Data$School_Name_No_Comma = gsub("DentistryTemple", "Dentistry Temple", Input_Data$School_Name_No_Comma)
  #Data from 2015 to 2017 spelled the name of The Maurice H. Kornberg School of DentistryTemple University wrong.
  
  Input_Data$School_Name_No_Comma = gsub("University of Illinois at Chicago", "University of Illinois Chicago", Input_Data$School_Name_No_Comma)
  
  #Change a few wierd names that changed over time.
  Input_Data$School_Name_No_Comma = gsub("Medical College of Georgia", 
                                         "Augusta University", Input_Data$School_Name_No_Comma)
  Input_Data$School_Name_No_Comma = gsub("Georgia Regents University College of Dental Medicine", 
                                         "Augusta University", Input_Data$School_Name_No_Comma)
  Input_Data$School_Name_No_Comma = gsub("Georgia Health Sciences University College of Dental Medicine", 
                                         "Augusta University", Input_Data$School_Name_No_Comma)
  Input_Data$School_Name_No_Comma = gsub("Georgia Health Sciences University", 
                                         "Augusta University", Input_Data$School_Name_No_Comma)
  
  Input_Data$School_Name_No_Comma = gsub("University of Colorado Denver", 
                                         "University of Colorado School of Dental Medicine", Input_Data$School_Name_No_Comma)
  
  
  Input_Data$School_Name_No_Comma = gsub("University of Texas Health Science Center at San Antonio Dental School", 
                                         "University of Texas San Antonio", Input_Data$School_Name_No_Comma)
  Input_Data$School_Name_No_Comma = gsub("University of Texas Health Science Center at", 
                                         "University of Texas", Input_Data$School_Name_No_Comma)
  Input_Data$School_Name_No_Comma = gsub("Henry M Goldman School of Dental Medicine", 
                                         "Henry M. Goldman School of Dental Medicine", Input_Data$School_Name_No_Comma)
  
  #I found that the University of Medicine and Dentistry of New Jersey was merged into Rutgers, State University of New Jersy,
  # in 2013. I thus considered them the same school, though I can change if you consider it otherwise.
  # Source: https://www.nj.com/news/2012/06/nj_assembly_passes_bill_for_ru.html
  
  Input_Data$School_Name_No_Comma = gsub("University of Medicine and Dentistry of New Jersey New Jersey Dental School", 
                                         "Rutgers New Jersey", Input_Data$School_Name_No_Comma)
  Input_Data$School_Name_No_Comma = gsub("Univeristy of Medicine and Dentistry of New Jersey", 
                                         "Rutgers New Jersey", Input_Data$School_Name_No_Comma)
  Input_Data$School_Name_No_Comma = gsub("University of Medicine and Dentistry of New Jersey", 
                                         "Rutgers New Jersey", Input_Data$School_Name_No_Comma)
  
  
  # Change the school names in School_Name_No_Comma, and return the whole dataframe back.
  
  return(Input_Data)
}


School_Name_Comparer = function(List_Name_2021, Raw_Data_Input){
  #This helps compare school names with List_Names_2021, which is usally a list of schools 
  # operating in 2021
  # The List_Names_2021 should be 2021 data, with AT LEAST "State", "School Names",
  # and "School_Names_No_Comma" Included.
  # With this script we change all schools' names to what they appear to be in 2021.
  
  Raw_Data = Raw_Data_Input #Don't wanna mess up original data!
  Raw_Data = School_Name_Standardizer(Raw_Data_Input)
  names(Raw_Data)[names(Raw_Data) == "School_Name_No_Comma"] <- "School_Name_2021_Standard"
  
  Raw_Data$Found_Correspond_2021 = FALSE
  for(i in 1:nrow(Raw_Data)){
    # we shall do this for each school in dataset.
    # It's usually only 60 or so rows, so calculation time is perfectly acceptable.
    
    # Drop the schools that weren't even in the state
    Possible_List_O_School = List_Name_2021[List_Name_2021$State == Raw_Data$State[i], ]
    
    Found_In_2021 = FALSE
    
    # Split the school's name by comma, into a vector of characters.
    This_School_Name_Split = unlist(strsplit(Raw_Data$School_Name_2021_Standard[i], split=" "))
    
    for(j in 1:nrow(Possible_List_O_School)){
      Compare_School_Name_Split = unlist(strsplit(Possible_List_O_School$School_Name_No_Comma[j], split=" "))
      
      # We can correspond this school to another school in 2021 if ALL words in school name 
      # was found in 2021 school name
      # This system generally works.
      # We will register 2021 name in variable School_Name_2021_Standard
      if( all(This_School_Name_Split %in% Compare_School_Name_Split)){
        Found_In_2021 = TRUE
        Raw_Data$Found_Correspond_2021[i] = TRUE
        
        Raw_Data$School_Name_2021_Standard[i] = Possible_List_O_School$School_Name[j]
        break
      }
      
    }
    
    # If not found, register the name as it is.
    if(Found_In_2021 == FALSE){
      Raw_Data$School_Name_2021_Standard[i] = Raw_Data$School_Name[i]
    }
  }
  
  #Sometimes there's parenthesis that does no good. We'd better clean it.
  # Drop any and all parenthesis whatsoever.
  Raw_Data$School_Name_2021_Standard = gsub("\\s*(\\([^()]*(?:(?1)[^()]*)*\\))", "", 
                                            Raw_Data$School_Name_2021_Standard, perl=TRUE)
  return(Raw_Data)
}

Full_List_O_Schools = Yearly_Table_Adm_Rate_Extractor(2021)[, c(1:2)] #This is 2021 school names/state.

Full_List_O_Schools = School_Name_Standardizer(Full_List_O_Schools)

# Prepare the final table. Begin from 2009
Final_Table_Adm = raw_adm_data_2009
Final_Table_Adm = School_Name_Comparer(List_Name_2021 = Full_List_O_Schools, Raw_Data_Input = Final_Table_Adm)
# Drop the useless last column
Final_Table_Adm = Final_Table_Adm[, -ncol(Final_Table_Adm)]
Final_Table_Adm = Final_Table_Adm[, -2] #Drop school name now.
Final_Table_Adm$year = 2009


for(i in 2010:2021){
  raw = Yearly_Table_Adm_Rate_Extractor(i)
  raw = School_Name_Comparer(List_Name_2021 = Full_List_O_Schools, Raw_Data_Input = raw)
  raw = raw[, -c(2, ncol(raw))]
  raw$year = i
  
  Final_Table_Adm = rbind(Final_Table_Adm, raw)
}

#Sort by State Code
Final_Table_Adm = Final_Table_Adm[order(Final_Table_Adm$State), ]

#Export in csv and dta
write.csv(Final_Table_Adm, file = "Admission_Data_2009_2021.csv")
write_dta(Final_Table_Adm, "Admission_Data_2009_2021.dta")


