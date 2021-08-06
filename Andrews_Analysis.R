###SET WORKING DIRECTORY TO DATA FOLDER (the one with all the CSV's) BEFORE RUNNING SCRIPT###

#Activate libraries
library (tidyverse)


### Data Cleaning
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


### Chart 1

##To Do:
#Separate out my age groups
#Teen = 10-19
#Young Adult = 20-34
#Adult = 35-65
#Elderly = 65+


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
ggplot(mergedAges, aes(y = newRate, x = time_period, group = ageCat)) +
  geom_line(size = 0.65, color = "grey33") +
  geom_point(size = 3.5, aes(color = ageCat, shape = ageCat)) +
  scale_shape_manual(values=c(15,16,17,18))+
  xlab("Time Period") +
  ylab("Suicide Rate") +
  ggtitle("Suicide Rates by Age Group") +
  labs(color = "Age Group", shape = "Age Group")

#Changes I Would Make:
## Make purple diamonds larger with a custom size scale (tried and couldn't get to work)
## add sub label in smaller font to y axis label saying rate is per 100k people
## reorder legend to reflect order of lines in chart

### Chart 2

##To Do:
#Separate out my age groups again
#Teen = 10-19
#Young Adult = 20-34
#Adult = 35-65
#Elderly = 65+

#Separate out Rural vs Urban
#Rural = Working Class Country & Rural Middle America
#Urban = Big Cities & Urban Suburbs

#Filter for 2015-2019 only


#Teens
teens1 <- massive %>% 
  filter(massive$acp_name == "Big Cities" | massive$acp_name == "Urban Suburbs" | massive$acp_name == "Working Class Country" | 
           massive$acp_name == "Rural Middle America")

#aggregate acp names into urban and rural
teens1$locality <- "" #create empty locality field
for(k in 1:nrow(teens1)) { #loops through teens1 and populates locality based on acp_name using nested if statements
  if(teens1[k,"acp_name"] == "Big Cities") {
    teens1[k, "locality"] <- "Urban"
  } else if(teens1[k,"acp_name"] == "Urban Suburbs") {
    teens1[k, "locality"] <- "Urban"
  } else if(teens1[k,"acp_name"] == "Working Class Country") {
    teens1[k, "locality"] <- "Rural"
  } else {
    teens1[k, "locality"] <- "Rural"
  }
}

teens1a <- teens1 %>%
  filter(teens1$time_period == "2015-2019") #filters for 2015-2019 time frame

teens_2 <- teens1a %>% 
  filter(teens1a$five_year_age_groups_code == "10-14" |  teens1a$five_year_age_groups_code == "15-19") %>% #filter for teens
  group_by(time_period, locality) %>%
  summarise(deaths = sum(deaths), population = sum(population))

teens_2$newRate <- (teens_2$deaths/teens_2$population)*100000 #calculate new rate
teens_2$ageCat <- "Teens"

#Young Adults
yadults1 <- massive %>% 
  filter(massive$acp_name == "Big Cities" | massive$acp_name == "Urban Suburbs" | massive$acp_name == "Working Class Country" | 
           massive$acp_name == "Rural Middle America")

#aggregate acp names into urban and rural
yadults1$locality <- "" #create empty locality field
for(k in 1:nrow(yadults1)) { #loops through teens1 and populates locality based on acp_name using nested if statements
  if(yadults1[k,"acp_name"] == "Big Cities") {
    yadults1[k, "locality"] <- "Urban"
  } else if(yadults1[k,"acp_name"] == "Urban Suburbs") {
    yadults1[k, "locality"] <- "Urban"
  } else if(yadults1[k,"acp_name"] == "Working Class Country") {
    yadults1[k, "locality"] <- "Rural"
  } else {
    yadults1[k, "locality"] <- "Rural"
  }
}

yadults1a <- yadults1 %>%
  filter(yadults1$time_period == "2015-2019") #filters for 2015-2019 time frame

yadults_2 <- yadults1a %>%
  filter(yadults1a$five_year_age_groups_code == "20-24" |  yadults1a$five_year_age_groups_code == "25-29" | 
           yadults1a$five_year_age_groups_code == "30-34") %>% #filter for young adults
  group_by(time_period, locality) %>%
  summarise(deaths = sum(deaths), population = sum(population))

yadults_2$newRate <- (yadults_2$deaths/yadults_2$population)*100000 #calculate new rate
yadults_2$ageCat <- "Young Adults"

#Adults
adults1 <- massive %>% 
  filter(massive$acp_name == "Big Cities" | massive$acp_name == "Urban Suburbs" | massive$acp_name == "Working Class Country" | 
           massive$acp_name == "Rural Middle America")

#aggregate acp names into urban and rural
adults1$locality <- "" #create empty locality field
for(k in 1:nrow(adults1)) { #loops through teens1 and populates locality based on acp_name using nested if statements
  if(adults1[k,"acp_name"] == "Big Cities") {
    adults1[k, "locality"] <- "Urban"
  } else if(adults1[k,"acp_name"] == "Urban Suburbs") {
    adults1[k, "locality"] <- "Urban"
  } else if(adults1[k,"acp_name"] == "Working Class Country") {
    adults1[k, "locality"] <- "Rural"
  } else {
    adults1[k, "locality"] <- "Rural"
  }
}

adults1a <- adults1 %>%
  filter(adults1$time_period == "2015-2019") #filters for 2015-2019 time frame

adults_2 <- adults1a %>% 
  filter(adults1a$five_year_age_groups_code == "35-39" | adults1a$five_year_age_groups_code == "40-44" | 
           adults1a$five_year_age_groups_code == "45-49" | adults1a$five_year_age_groups_code == "50-54" |
           adults1a$five_year_age_groups_code == "55-59" | adults1a$five_year_age_groups_code == "60-64") %>% #filter for adults
  group_by(time_period, locality) %>%
  summarise(deaths = sum(deaths), population = sum(population))

adults_2$newRate <- (adults_2$deaths/adults_2$population)*100000 #calculate new rate
adults_2$ageCat <- "Adults"

#Elderly
elderly1 <- massive %>% 
  filter(massive$acp_name == "Big Cities" | massive$acp_name == "Urban Suburbs" | massive$acp_name == "Working Class Country" | 
           massive$acp_name == "Rural Middle America")

#aggregate acp names into urban and rural
elderly1$locality <- "" #create empty locality field
for(k in 1:nrow(elderly1)) { #loops through teens1 and populates locality based on acp_name using nested if statements
  if(elderly1[k,"acp_name"] == "Big Cities") {
    elderly1[k, "locality"] <- "Urban"
  } else if(elderly1[k,"acp_name"] == "Urban Suburbs") {
    elderly1[k, "locality"] <- "Urban"
  } else if(elderly1[k,"acp_name"] == "Working Class Country") {
    elderly1[k, "locality"] <- "Rural"
  } else {
    elderly1[k, "locality"] <- "Rural"
  }
}

elderly1a <- elderly1 %>%
  filter(elderly1$time_period == "2015-2019") #filters for 2015-2019 time frame

elderly_2 <- elderly1a %>% 
  filter(elderly1a$five_year_age_groups_code == "65-69" | elderly1a$five_year_age_groups_code == "70-74" | 
           elderly1a$five_year_age_groups_code == "75-79" | elderly1a$five_year_age_groups_code == "80-84") %>% #filter for elderly
  group_by(time_period, locality) %>%
  summarise(deaths = sum(deaths), population = sum(population))

elderly_2$newRate <- (elderly_2$deaths/elderly_2$population)*100000 #calculate new rate
elderly_2$ageCat <- "Elderly"

#Merge age ranges together
mergedAges_2 <- rbind(teens_2, yadults_2, adults_2, elderly_2)

#Create Line Graph
ggplot(mergedAges_2, aes(y = newRate, x = ageCat, group = locality)) +
  geom_bar(aes(fill = locality), position = "dodge", stat = "identity") + #sets up double bar chart
  geom_text(aes(label = round(newRate, digits = 3)), vjust = 1.5, position = position_dodge(.9), color = "white") + #sets up label
  xlab("Age Group") + #sets x axis label
  ylab("Suicide Rate") + #sets y axis label
  ggtitle("Suicide Rates by Age Group and Locality") + #sets plot title
  labs(fill = "Locality") #sets legend title

#Changes I Would Make:
## add sub label in smaller font to y axis label saying rate is per 100k people
## reorder age groups from youngest to oldest