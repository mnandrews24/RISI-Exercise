#Read in CSVs from Github
library (readr)
urlfile="https://raw.githubusercontent.com/mnandrews24/RISI-Exercise/main/data/suicide_rural_middle_america_2000_2004_by_5yr_age_group.csv"
mydata<-read_csv(url(urlfile))

#Get a list of files in the directory to put in a For loop
ldf <- list() # creates a list
listcsv <- dir(pattern = "*.csv") # creates the list of all the csv files in the directory
for (k in 1:length(listcsv)){
  ldf[[k]] <- read.csv(listcsv[k])
}
str(ldf[[1]])
