library("readxl")
library("dplyr")
library("haven") #To export DTA

i = 2009

if(!dir.exists("BLS_Data_Unzip_Folder")){
  dir.create("BLS_Data_Unzip_Folder")
}

# download.file(url = "https://www.bls.gov/oes/special.requests/oesm09st.zip",
#               destfile = "./BLS_Data_Unzip_Folder/OESM_2009_State.zip")
download.file(url = "https://www.bls.gov/oes/special.requests/oesm09ma.zip",
              destfile = "./BLS_Data_Unzip_Folder/OESM_2009_MSA.zip")


for(i in 10:21){
  # download.file(url = paste("https://www.bls.gov/oes/special.requests/oesm", i, "st.zip", sep = ""),
  #               destfile = paste("./BLS_Data_Unzip_Folder/OESM_", i + 2000, "_State.zip", sep = ""))
  download.file(url = paste("https://www.bls.gov/oes/special.requests/oesm", i, "ma.zip", sep = ""),
                destfile = paste("./BLS_Data_Unzip_Folder/OESM_", i + 2000, "_MSA.zip", sep = ""))
  
  print(i)
}




for(i in 9:21){
  # unzip(zipfile = paste("./BLS_Data_Unzip_Folder/OESM_", i+2000, "_State.zip", sep = ""), 
  #       exdir = "./BLS_Data_Unzip_Folder")
  unzip(zipfile = paste("./BLS_Data_Unzip_Folder/OESM_", i+2000, "_MSA.zip", sep = ""), 
        exdir = "./BLS_Data_Unzip_Folder")
  #print(i + 2000)
}

OCC_CODE_Changer_Pre_18 = function(temp){
  #Used for year of or before 2018
  #Input is dataframe
  Intermediate_Data = temp[temp$OCC_CODE == "29-1021" | #Dentist
                             temp$OCC_CODE == "29-2021" | #Dental Hygienists
                             temp$OCC_CODE == "29-1062" | #Family and General Practitioners. I put it same as family medicine physician
                             temp$OCC_CODE == "29-1069" | #Physician and Surgeons, all other. I put is same as physicians, all other
                             temp$OCC_CODE == "29-1111" | #Registered Nurse 
                           temp$OCC_CODE == "29-1141", ] #Registered Nurse after 2012. 
  #the code change for nurse changed from 1111 to 1141 by 2012. However, before that there ain't no 1141, so
  # we can safely just include it instead of use a new function.
  #Source: https://www.bls.gov/soc/2000/soc-structure-2000.pdf 
  
  Intermediate_Data %>%
    mutate(OCC_CODE_2021 = recode(OCC_CODE, "29-2021" = "29-1292", "29-1062" = "29-1215",
                                  "29-1069" = "29-1228", "29-1111" = "29-1141") ) -> Intermediate_Data
  
  return(Intermediate_Data)
  
  
  
}

OCC_CODE_Changer_Past_18 = function(temp){
  #Used for year of or before 2018
  #Input is dataframe
  colnames(temp) = toupper(colnames(temp))
  Intermediate_Data = temp[temp$OCC_CODE == "29-1021" | #Dentist
                             temp$OCC_CODE == "29-1292" | #Dental Hygienists
                             temp$OCC_CODE == "29-1215" | #family medicine physician
                             temp$OCC_CODE == "29-1228" | #physicians, all other
                             temp$OCC_CODE == "29-1141" #Registered Nurse 
                           , ]
  #Source: https://www.bls.gov/soc/2010/2010_major_groups.htm#29-0000 
  Intermediate_Data$OCC_CODE_2021 = Intermediate_Data$OCC_CODE
  
  names(Intermediate_Data)[names(Intermediate_Data) == "AREA_TITLE"] <- "AREA_NAME"
  

  return(Intermediate_Data)
}

MSA_BOS_Processor = function(Input_Year, Input_Data){
  if(Input_Year <= 2018){
    return(select(OCC_CODE_Changer_Pre_18(Input_Data), 
                               #Bring in the list needed
                               AREA, AREA_NAME, OCC_CODE_2021, OCC_TITLE, TOT_EMP, JOBS_1000, H_MEAN, A_MEAN, H_MEDIAN, A_MEDIAN) %>%
      mutate(year = Input_Year))
    #Intermediate_Data[, c(5:10)] = as.numeric(Intermediate_Data[, c(5:10)])
    
  }
  else{
    return(select(OCC_CODE_Changer_Past_18(Input_Data), 
                  #Bring in the list needed
                  AREA, AREA_NAME, OCC_CODE_2021, OCC_TITLE, TOT_EMP, JOBS_1000, H_MEAN, A_MEAN, H_MEDIAN, A_MEDIAN) %>%
             mutate(year = Input_Year))
  }
}


MSA_Extractor = function(year){
  if(year == 2009){
    temp = read_xls("./BLS_Data_Unzip_Folder/MSA_dl_1.xls")
    temp = MSA_BOS_Processor(Input_Year = 2009, Input_Data = temp)
    temp = rbind(temp, MSA_BOS_Processor(Input_Year = 2009, Input_Data = read_xls("./BLS_Data_Unzip_Folder/MSA_dl_2.xls")))
    temp = rbind(temp, MSA_BOS_Processor(Input_Year = 2009, Input_Data = read_xls("./BLS_Data_Unzip_Folder/MSA_dl_3.xls")))

  }else if(year == 2010){
    
    temp = read_xls("./BLS_Data_Unzip_Folder/MSA_M2010_dl_1.xls")
    temp = MSA_BOS_Processor(Input_Year = 2010, Input_Data = temp)
    temp = rbind(temp, MSA_BOS_Processor(Input_Year = 2010, Input_Data = read_xls("./BLS_Data_Unzip_Folder/MSA_M2010_dl_2.xls")))
    temp = rbind(temp, MSA_BOS_Processor(Input_Year = 2010, Input_Data = read_xls("./BLS_Data_Unzip_Folder/MSA_M2010_dl_3.xls")))

  }else if(year <= 2013){
    
    temp = read_xls(paste("./BLS_Data_Unzip_Folder/MSA_M", year, "_dl_1_AK_IN.xls", sep = ""))
    
    temp = MSA_BOS_Processor(Input_Year = year, Input_Data = temp)
    
    temp1 = read_xls(paste("./BLS_Data_Unzip_Folder/MSA_M", year, "_dl_2_KS_NY.xls", sep = ""))
    
    temp = rbind(temp, MSA_BOS_Processor(Input_Year = year, Input_Data = temp1))
    
    temp1 = read_xls(paste("./BLS_Data_Unzip_Folder/MSA_M", year, "_dl_3_OH_WY.xls", sep = ""))
    
    temp = rbind(temp, MSA_BOS_Processor(Input_Year = year, Input_Data = temp1))
  }else{
    temp = read_xlsx(paste("./BLS_Data_Unzip_Folder/oesm", year - 2000, "ma/MSA_M", year, "_dl.xlsx", sep = ""))
    temp = MSA_BOS_Processor(Input_Year = year, Input_Data = temp)
    
  }
  return(temp)
}


result = MSA_Extractor(2009)
for(i in 2010:2021){
  result = rbind(result, MSA_Extractor(i))
  print(paste("MSA Area Data for Year", i, "Processed"))
}

for(i in 5:11){
  result[, i] = as.numeric(unlist(result[, i]))
}


write.csv(result, file = "Wages_By_MSA.csv")
write_dta(result, "Wages_By_MSA.dta")


None_Metro_Area_Extractor = function(year){
  if(year <= 2013){
    
    temp = read_xls(paste("./BLS_Data_Unzip_Folder/BOS_M", year, "_dl.xls", sep = ""))
    
    temp = MSA_BOS_Processor(Input_Year = year, Input_Data = temp)
  }else{
    temp = read_xlsx(paste("./BLS_Data_Unzip_Folder/oesm", year - 2000, "ma/BOS_M", year, "_dl.xlsx", sep = ""))
    temp = MSA_BOS_Processor(Input_Year = year, Input_Data = temp)
    
  }
  return(temp)
}

result = None_Metro_Area_Extractor(2009)
for(i in 2010:2021){
  
  result = rbind(result, None_Metro_Area_Extractor(i))
  print(paste("Non MSA Area Data for Year", i, "Processed"))
}

for(i in 5:11){
  result[, i] = as.numeric(unlist(result[, i]))
}

write.csv(result, file = "Wages_By_Non_MSA_Area.csv")
write_dta(result, "Wages_By_Non_MSA_Area.dta")


#Use the following line if you don't need these stuff afterwards.
#unlink("BLS_Data_Unzip_Folder", recursive = TRUE)





