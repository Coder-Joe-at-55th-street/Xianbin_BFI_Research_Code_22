
library("readxl") #To read Excel
library("haven") #To export DTA


##This script grabs the table telling GPA and DTA score from each year's applicant, 2000 to 2021.

# Basically this code just grab a bit from 2021 data.
raw = read_excel(path = "ADEA Dental School Applicants and Enrollees 2021 Entering Class.xlsx", sheet = "Table 12", .name_repair = "minimal")

#Trim it a little bit
raw = raw[3:75, ]

raw = raw[, -2]

raw[1,1] = raw[2,1]

raw[26, 1] = raw[27, 1]

raw[51, 1] = raw[52, 1]

raw = raw[-c(2, 27, 52), ]

#Extract certain rows and make them into proper fomat
Extractor = function(beginning_line, end_line){
  temp = raw[c(beginning_line:end_line), ]
  colnames(temp) = temp[1, ]
  temp = temp[-1, ]
}

# Extract the applicant data (from row 1-23)
Applicant_Data = Extractor(1,23)
colnames(Applicant_Data) = c("Year", "GPA_Science", "GPA_Total", "DAT_Academic_Average",
                             "DAT_Perceptual_Ability", "DAT_Total_Science")
Applicant_Data$Type = "Applicant"

# Extract the enrollee data (from row 25 - 47)
Additional_Data = Extractor(25, 47)
colnames(Additional_Data) = c("Year", "GPA_Science", "GPA_Total", "DAT_Academic_Average",
                             "DAT_Perceptual_Ability", "DAT_Total_Science")

Additional_Data$Type = "First Year First Time Enrollee"

Applicant_Data = rbind(Applicant_Data, Additional_Data)

# Extract the first time first year enrollee data (from row 49-70)
Additional_Data = Extractor(49, 70)
colnames(Additional_Data) = c("Year", "GPA_Science", "GPA_Total", "DAT_Academic_Average",
                             "DAT_Perceptual_Ability", "DAT_Total_Science")

# Additional_Data = rbind(Additional_Data, c(2021, NA, NA, NA, NA, NA))
Additional_Data$Type = "Total First Year Enrollee"

Applicant_Data = rbind(Applicant_Data, Additional_Data)

# write the data.
write.csv(Applicant_Data, "GPA_DAT_National_Level_00_21.csv")
write_dta(Applicant_Data, "GPA_DAT_National_Level_00_21.dta")

