#Activate libraries
#library (readr)
library (tidyverse)

#Get a list of files in the directory to put in a for loop
ldf <- list() #creates a list
listcsv <- dir(pattern = "*.csv") #creates the list of all the csv files in the directory
for (k in 1:length(listcsv)){ #starts for loop
  ldf[[k]] <- read.csv(listcsv[k]) #Reads the CSV at position k and saves output to ldf[[k]] (should be dataframe)
}

#Loop to save each dataframe as a variable
listDF <- list()
for (k in 1:length(ldf)){
  newList <- head(ldf[[k]], -6) #remove last X number of rows from dataframe (total row would be -1)
  newList2 <- tail(newList, -3) #remove first 3 rows from dataframe (suppressed children <10 values)
  newList3 <- newList2 %>% filter(newList2$deaths != "Suppressed") #removes any remaining Suppressed values
  assign(str_c("A",k), newList3) #assigns each df in ldf to a variable name
  listDF[[k]] <- newList3 #populates the empty list with the cleaned dataframes
}

#Merge all dataframes together
massive <- do.call("rbind", listDF)
massive$deaths <- as.numeric(as.character(massive$deaths)) #convert deaths col to numeric for calculations
massive$population <- as.numeric(as.character(massive$population)) #convert pop col to numeric for calculations

#Separate out my age groups
#Teen = 10-19
#Young Adult = 20-34
#Adult = 35-65
#Elderly = 65+

#address Unreliable crude rates (where deaths are <20?)

#Teens
teens <- massive %>% 
  filter(massive$five_year_age_groups_code == "10-14" |  massive$five_year_age_groups_code == "15-19") %>% #filter for teens
  group_by(time_period) %>%
  summarise(deaths = sum(deaths), population = sum(population))

teens$newRate <- (teens$deaths/teens$population)*100000 #calculate new rate
teens$ageCat <- "Teens"

#Young Adults
yadults <- massive %>% 
  filter(massive$five_year_age_groups_code == "20-24" |  massive$five_year_age_groups_code == "25-29" | 
           massive$five_year_age_groups_code == "30-34") %>% #filter for young adults
  group_by(time_period) %>%
  summarise(deaths = sum(deaths), population = sum(population))

yadults$newRate <- (yadults$deaths/yadults$population)*100000 #calculate new rate
yadults$ageCat <- "Young Adults"

#Adults
adults <- massive %>% 
  filter(massive$five_year_age_groups_code == "35-39" | massive$five_year_age_groups_code == "40-44" | 
           massive$five_year_age_groups_code == "45-49" | massive$five_year_age_groups_code == "50-54" |
           massive$five_year_age_groups_code == "55-59" | massive$five_year_age_groups_code == "60-64") %>% #filter for adults
  group_by(time_period) %>%
  summarise(deaths = sum(deaths), population = sum(population))

adults$newRate <- (adults$deaths/adults$population)*100000 #calculate new rate
adults$ageCat <- "Adults"

#Elderly
elderly <- massive %>% 
  filter(massive$five_year_age_groups_code == "65-69" | massive$five_year_age_groups_code == "70-74" | 
           massive$five_year_age_groups_code == "75-79" | massive$five_year_age_groups_code == "80-84") %>% #filter for elderly
  group_by(time_period) %>%
  summarise(deaths = sum(deaths), population = sum(population))

elderly$newRate <- (elderly$deaths/elderly$population)*100000 #calculate new rate
elderly$ageCat <- "Elderly"

#Merge age ranges together
mergedAges <- rbind(teens, yadults, adults, elderly)

#Create Line Graph
ggplot(mergedAges, aes(y = newRate, x = time_period, color = ageCat)) +
  geom_line()
#is the issue that I have non-numbers on my X axis? No chart showing
