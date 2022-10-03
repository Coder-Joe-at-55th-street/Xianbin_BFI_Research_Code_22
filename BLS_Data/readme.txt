Since the raw data is about 200 MB in size and annoying to be downloaded one by one on BLS site, I have incorporated downloading
within the scripts.

The order of running is:
Data Compiler MSA.R or Data Compiler State.R first
Make sure you have internet connection and Population_Data_Folder, with st-est00int-01.xls, nst-est2020.csv, and
NST-EST2021-POP.xlsx in it.
Do NOT move output files---the next script requires it.
It might take several minutes.

Then, run Yearly_Data_Processor.R

The first two scripts download the data and unzip and format them in proper manner.
The last script depends on previous output and find wage ratio, of dentist/doctor and dentist/nurse, of 
all possible (that is, had data) MSA, State, or Non-MSA area, every (possible) year.

Due to the lack of some year's data, the wage ratio might not be available in all year, especially in MSAs.

The first two scripts has the last line commented out. By un-commenting it you can delete downloaded data and unzipped file.

Some libraries might require installation.