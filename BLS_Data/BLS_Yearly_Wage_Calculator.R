library("readxl")
library("dplyr")
library("haven") #To export DTA

#result = read.csv("Wages_By_MSA.csv", stringsAsFactors = FALSE)


Ratio_Calculator = function(Input_Data){
  Input_Data = na.omit(Input_Data)
  Input_Data$a_mean_den_doc = Input_Data$a_mean_dentist / Input_Data$a_mean_doctor
  Input_Data$a_mean_den_nurse = Input_Data$a_mean_dentist / Input_Data$a_mean_nurse
  return(Input_Data)
}


State_Consolidate = function(result){
  
  MSA_DENTIST_A_MEAN = unique(result[result$occ_code == 1021, c("st", "year", "a_mean")])
  colnames(MSA_DENTIST_A_MEAN) = c("st", "year", "a_mean_dentist")
  
  MSA_DOCTOR_A_MEAN = unique(result[result$occ_code == 1062, c("st", "year", "a_mean")])
  colnames(MSA_DOCTOR_A_MEAN) = c("st", "year", "a_mean_doctor")
  
  MSA_NURSE_A_MEAN = unique(result[result$occ_code == 1141, c("st", "year", "a_mean")])
  colnames(MSA_NURSE_A_MEAN) = c("st", "year", "a_mean_nurse")
  
  merge(MSA_DENTIST_A_MEAN, MSA_DOCTOR_A_MEAN, by = c("st", "year")) -> MSA_Data_Consolidated
  merge(MSA_Data_Consolidated, MSA_NURSE_A_MEAN, by = c("st", "year")) -> MSA_Data_Consolidated
  
  MSA_Data_Consolidated = Ratio_Calculator(MSA_Data_Consolidated)
  return(MSA_Data_Consolidated)
}


#result = read.csv("Wages_By_State.csv", stringsAsFactors = FALSE)
State_Data_Consolidated = State_Consolidate(read.csv("Wages_By_State.csv", stringsAsFactors = FALSE))


write.csv(State_Data_Consolidated, file = "State_Wage_Ratio.csv")

#Add labels
attr(State_Data_Consolidated$st, "label") <- "State Abbreviation"
attr(State_Data_Consolidated$year, "label") <- "Year of the record"
attr(State_Data_Consolidated$a_mean_dentist, "label") <- "Mean Annual Wage, Dentist"
attr(State_Data_Consolidated$a_mean_doctor, "label") <- "Mean Annual Wage, Doctor (Family and General Practitioners)"
attr(State_Data_Consolidated$a_mean_nurse, "label") <- "Mean Annual Wage, Registered Nurse"
attr(State_Data_Consolidated$a_mean_den_doc, "label") <- "Ratio of Mean Annual Wage, Dentist / Doctor"
attr(State_Data_Consolidated$a_mean_den_nurse, "label") <- "Ratio of Mean Annual Wage, Dentist / Nurse"


write_dta(State_Data_Consolidated, "State_Wage_Ratio.dta")


# 
# MSA_BOS_Consolidate = function(result){
#   MSA_DENTIST_A_MEAN = unique(result[result$OCC_CODE_2021 == "29-1021", c("AREA", "year", "AREA_NAME", "A_MEAN")])
#   colnames(MSA_DENTIST_A_MEAN) = c("AREA", "year", "AREA_NAME", "A_MEAN_DENTIST")
#   MSA_DOCTOR_A_MEAN = unique(result[result$OCC_CODE_2021 == "29-1215", c("AREA", "year", "AREA_NAME", "A_MEAN")])
#   colnames(MSA_DOCTOR_A_MEAN) = c("AREA", "year", "AREA_NAME", "A_MEAN_DOCTOR")
#   MSA_NURSE_A_MEAN = unique(result[result$OCC_CODE_2021 == "29-1141", c("AREA", "year", "AREA_NAME", "A_MEAN")])
#   colnames(MSA_NURSE_A_MEAN) = c("AREA", "year", "AREA_NAME", "A_MEAN_NURSE")
#   
#   merge(MSA_DENTIST_A_MEAN, MSA_DOCTOR_A_MEAN, by = c("AREA", "year", "AREA_NAME")) -> MSA_Data_Consolidated
#   merge(MSA_Data_Consolidated, MSA_NURSE_A_MEAN, by = c("AREA", "year", "AREA_NAME")) -> MSA_Data_Consolidated
#   
#   MSA_Data_Consolidated = Ratio_Calculator(MSA_Data_Consolidated)
#   return(MSA_Data_Consolidated)
# }
# 
# MSA_Data_Consolidated = MSA_BOS_Consolidate(read.csv("Wages_By_MSA.csv", stringsAsFactors = FALSE))
# 
# 
# write.csv(MSA_Data_Consolidated, file = "MSA_Wage_Ratio.csv")
# write_dta(MSA_Data_Consolidated, "MSA_Wage_Ratio.dta")
# 
# BOS_Data_Consolidated = MSA_BOS_Consolidate(read.csv("Wages_By_Non_MSA_Area.csv", stringsAsFactors = FALSE))
# 
# write.csv(BOS_Data_Consolidated, file = "Non_MSA_Area_Wage_Ratio.csv")
# write_dta(BOS_Data_Consolidated, "Non_MSA_Area_Wage_Ratio.dta")





