#Compile focals for longterm database 

#Motivation: This R script provides some example code for how to prepare focal observations collected using the focal app (developed by E.Studd) to be merged with the long term database. Please note that this is example code only and will need to be modified according to the focal observations that need to be merged in a given year. 

#Last modified on November 19, 2019 by E.Siracusa

#Required packages

library(plyr)
library(dplyr)
library(magrittr)
library(lubridate)

#Import focal data------

#Please note that if you are attempting to read focal files in as a loop (as I have done here) you MUST create a separate folder and put ONLY the focal files that you want read in in that folder. You then need to set your working directory to that same folder.

setwd("/Volumes/USB DISK/KRSP 2019- Head Tech Key/Mast Crew/Focal Data/Focal files")

#This tells R where to look for those focal files and what format they are in (i.e. ".csv")

temp <- list.files(path = "/Volumes/USB DISK/KRSP 2019- Head Tech Key/Mast Crew/Focal Data/Focal files", pattern="*.csv") 

#Create a dataframe to store all the focals in

focals19 <- data.frame()

#Use a loop to read in all of the focal files (this is particularly convienent if there are a lot of focal files that you want merged into one)
n <- length(temp)
for (i in 1:n){
	print(i)
	focals <-read.csv(temp[i], header=T)
	focals19 <- rbind(focals19,focals)
}

#Check for focals that are not the right length
groupid <- focals19 %>% 
	group_by(FOCALID) %>%
	summarize(n= n())

#Here I am looking for and filtering out focals that are missing observations and are therefore not the appropriate length
toofew <- groupid %>% filter(n < 13)

focals19 %<>%
	filter(!FOCALID %in% toofew$FOCALID)

#Now I am looking for focals that appear to be too long -- usually this problem arises when an observer has given two (or more) focals the same Focal ID

toomany <- groupid %>% filter(n > 17) #I have checked this focal and it looks okay

#Make sure the date is in the appropriate format

focals19 %<>% mutate(DATE = mdy(DATE))

#Change working directory so not saving in the same spot as the .csv files (this matters for being able to read in the files in a loop as I did above)

setwd("/Volumes/USB DISK/KRSP 2019- Head Tech Key/Mast Crew/Focal Data")

#If desired, save a copy of these merged focals so you don't have to run the code above again

write.csv(focals19, "MastCrew_Compiled_Focals.csv")

#Add in tags for squirrels------
#The focal app does not automatically add the tags for individuals so you need to bring in a copy of the recent (2019 in this case) not long term squirrel table to merge this with the focal data

focals19 <- read.csv("MastCrew_Compiled_Focals.csv", header = T)

sqtable <- read.csv("squirrel.csv", header = T)

sqtable %<>%
	select(id, taglft, tagrt) %>%
	rename(SQUIRREL_ID = id)

focals19 %<>% left_join(sqtable, by = "SQUIRREL_ID")

#Check for any errors where no matches were found - this usually occurs if an observer entered the wrong squirrel ID for a focal
tag_errors <- focals19 %>% filter(is.na(taglft) & is.na(tagrt))


#Code behaviours------

#First check what the different behaviours are as there may be variations on a theme (e.g. groom/grooming/GROOM) - this technically shouldn't happen but often does when an observer is cleaning up the data after downloading it from the app
levels(focals19$BEHAVIOUR)

#Replace the named behaviour with the appropriate code

focals19$BEHAVIOUR <- gsub("feeding ","1",focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("feeding","1",focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("eating","1",focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("vocalization", "2", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("vocalization ", "2", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("bark", "2", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("travelling", "3", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("travelling ", "3", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("resting", "4", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("resting ", "4", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("sitting", "4", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("in nest", "5", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("in nest ", "5", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("nest building", "14", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("nest", "5", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("interaction with another squirrel ", "7", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("interaction with another squirrel", "7", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("interaction with other squirrel", "7", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("interaction", "7", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("caching ", "8", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("caching", "8", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("grooming", "10", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("grooming ", "10", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("groom", "10", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("GROOM", "10", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("scratching", "10", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("foraging ", "12", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("foraging", "12", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("not visible", "13", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("not visible ", "13", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("out of sight, temporarily lost squirrel", "13", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("out of sight", "13", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("lost", "13", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("lost ", "13", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("scent marking", "16", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("vigilant", "19", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("foot stomping", "21", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("foot stomp", "21", focals19$BEHAVIOUR)
focals19$BEHAVIOUR <- gsub("mating chase", "22", focals19$BEHAVIOUR)

#Turn your behaviour column into a factor and check the levels of your factor
focals19$BEHAVIOUR <- as.factor(focals19$BEHAVIOUR)
levels(focals19$BEHAVIOUR)

#Look for instances where there was no behaviour recorded and get rid of focals where behaviour is missing
focals19 %<>% 
	filter(FOCALID != "ALC145",
				 FOCALID != "ALC220",
				 FOCALID != "ALC230",
				 FOCALID != "ALC242")

#Combine cases where a space makes it look like there are 2 levels for for the same factor (e.g. "2" and "2 ") -- NOTE: make sure you make the behaviour column a CHARACTER first or this will mess up your coding!!
focals19 %<>%
	mutate(BEHAVIOUR = as.character(BEHAVIOUR)) %>%
	mutate(BEHAVIOUR = ifelse(BEHAVIOUR == "2 ", "2", BEHAVIOUR),
				 BEHAVIOUR = ifelse(BEHAVIOUR == "10 ", "10", BEHAVIOUR),
				 BEHAVIOUR = ifelse(BEHAVIOUR == "13 ", "13", BEHAVIOUR),
				 BEHAVIOUR = ifelse(BEHAVIOUR == "3 ", "3", BEHAVIOUR),
				 BEHAVIOUR = ifelse(BEHAVIOUR == "4 ", "4", BEHAVIOUR),
				 BEHAVIOUR = ifelse(BEHAVIOUR == "5 ", "5", BEHAVIOUR))

#Turn into a factor again and check
focals19$BEHAVIOUR <- as.factor(focals19$BEHAVIOUR)
levels(focals19$BEHAVIOUR)
summary(focals19$BEHAVIOUR)

#Figure out why there is a focal with NA for behaviour
focals19 %>% filter(is.na(BEHAVIOUR))
focals19 %>% filter(FOCALID == "SLC076")

#Get rid of this focal where most of the data is missing
focals19 %<>% filter(!FOCALID == "SLC076")

#Code details ------

#Separate out behaviours that don't have details
focals.no.detail <- subset(focals19, !focals19$BEHAVIOUR=="1" & !focals19$BEHAVIOUR=="2" & !focals19$BEHAVIOUR=="3" & !focals19$BEHAVIOUR=="7" & !focals19$BEHAVIOUR=="8" & !focals19$BEHAVIOUR=="12")

#Check this
focals.no.detail$BEHAVIOUR <- droplevels(focals.no.detail$BEHAVIOUR)
summary(focals.no.detail$BEHAVIOUR)

#Now we are going to separate out each behaviour with detail options and apply the appropriate coding for the details, following the same method that we used for coding the behaviours above

#Feeding
feeding <- focals19 %>% filter(BEHAVIOUR=="1")

feeding$DETAIL <- droplevels(feeding$DETAIL)
summary(feeding$DETAIL)

feeding$DETAIL <- gsub("animal material", "1", feeding$DETAIL)
feeding$DETAIL <- gsub("old cones", "2", feeding$DETAIL)
feeding$DETAIL <- gsub("old cone", "2", feeding$DETAIL)
feeding$DETAIL <- gsub("old cone ", "2", feeding$DETAIL)
feeding$DETAIL <- gsub("cone", "2", feeding$DETAIL)
feeding$DETAIL <- gsub("buds", "3", feeding$DETAIL)
feeding$DETAIL <- gsub("old mushroom, truffle", "4", feeding$DETAIL)
feeding$DETAIL <- gsub("mushroom, truffle", "4", feeding$DETAIL)
feeding$DETAIL <- gsub("mushroom", "4", feeding$DETAIL)
feeding$DETAIL <- gsub("bark", "5", feeding$DETAIL)
feeding$DETAIL <- gsub("snow", "30", feeding$DETAIL)
feeding$DETAIL <- gsub("unknown material", "25", feeding$DETAIL)
feeding$DETAIL <- gsub("unknown", "25", feeding$DETAIL)

feeding$DETAIL<- as.factor(feeding$DETAIL)
summary(feeding$DETAIL)
levels(feeding$DETAIL)

#Combine cases where a space makes it look like there are 2 levels for for the same factor (e.g. "2" and "2 ")

feeding %<>%
	mutate(DETAIL = as.character(DETAIL)) %>%
	mutate(DETAIL = ifelse(DETAIL == "2 ", "2", DETAIL))

feeding$DETAIL<- as.factor(feeding$DETAIL)
levels(feeding$DETAIL)

#Vocalizing

voc <- focals19 %>% filter(BEHAVIOUR=="2")

voc$DETAIL <- droplevels(voc$DETAIL)
summary(voc$DETAIL)

voc$DETAIL <- gsub("rattle-chew", "6", voc$DETAIL)	
voc$DETAIL <- gsub("rattle", "1", voc$DETAIL)
voc$DETAIL <- gsub("RAttle", "1", voc$DETAIL)
voc$DETAIL <- gsub("squeaking", "4", voc$DETAIL)
voc$DETAIL <- gsub("squeak", "4", voc$DETAIL)
voc$DETAIL <- gsub("bark", "5", voc$DETAIL)
voc$DETAIL <- gsub("buzz", "7", voc$DETAIL)

voc$DETAIL<- as.factor(voc$DETAIL)
summary(voc$DETAIL)
levels(voc$DETAIL)

#Traveling

travel <- focals19 %>% filter(BEHAVIOUR=="3")
travel$DETAIL <- droplevels(travel$DETAIL)
summary(travel$DETAIL)

#Get rid of detials that don't make sense -- this is usually due to human error coding an incorrect detail, but I often go back and check the data to make sure that I haven't coded a behaviour incorrectly

travel %<>% 
	mutate(DETAIL = as.character(DETAIL)) %>%
	mutate(DETAIL = ifelse(DETAIL == "buds", "", DETAIL),
				 DETAIL = ifelse(DETAIL == "old cone", "", DETAIL))

travel$DETAIL <- as.factor(travel$DETAIL)
summary(travel$DETAIL)

travel$DETAIL <- gsub("on ground", "1", travel$DETAIL)
travel$DETAIL <- gsub("on ground ", "1", travel$DETAIL)
travel$DETAIL <- gsub("in ground", "1", travel$DETAIL)
travel$DETAIL <- gsub("in tree", "2", travel$DETAIL)
travel$DETAIL <- gsub("on tree", "2", travel$DETAIL)

travel$DETAIL<- as.factor(travel$DETAIL)
summary(travel$DETAIL)
levels(travel$DETAIL)

#Combine cases where a space makes it look like there are 2 levels for for the same factor (e.g. "2" and "2 ")

travel %<>%
	mutate(DETAIL = as.character(DETAIL)) %>%
	mutate(DETAIL = ifelse(DETAIL == "1 ", "1", DETAIL))

travel$DETAIL<- as.factor(travel$DETAIL)
levels(travel$DETAIL)

#Interacting

interact <- focals19 %>% filter(BEHAVIOUR=="7")

interact$DETAIL <- droplevels(interact$DETAIL)
summary(interact$DETAIL)

#Get rid of detials that don't make sense

interact %<>% 
	mutate(DETAIL = as.character(DETAIL)) %>%
	mutate(DETAIL = ifelse(DETAIL == "other tagged", "", DETAIL))

interact$DETAIL <- as.factor(interact$DETAIL)
summary(interact$DETAIL)

interact$DETAIL <- gsub("non-neighbour", "7", interact$DETAIL)
interact$DETAIL <- gsub("neighbor", "1", interact$DETAIL)
interact$DETAIL <- gsub("neighbour", "1", interact$DETAIL)
interact$DETAIL <- gsub("sibling", "6", interact$DETAIL)
interact$DETAIL <- gsub("unidentified sq.", "4", interact$DETAIL)

interact$DETAIL<- as.factor(interact$DETAIL)
summary(interact$DETAIL)

#Caching

cache <- focals19 %>% filter(BEHAVIOUR=="8")

cache$DETAIL <- droplevels(cache$DETAIL)
summary(cache$DETAIL)

cache$DETAIL <- gsub("cache cone", "5", cache$DETAIL)
cache$DETAIL <- gsub("cache mushroom", "4", cache$DETAIL)
cache$DETAIL <- gsub("cache mushrm", "4", cache$DETAIL)
cache$DETAIL <- gsub("cache mush", "4", cache$DETAIL)
cache$DETAIL <- gsub("digging holes", "6", cache$DETAIL)
cache$DETAIL <- gsub("digging", "6", cache$DETAIL)
cache$DETAIL <- gsub("travel/mush", "2", cache$DETAIL)
cache$DETAIL <- gsub("travel /c shroom", "2", cache$DETAIL)
cache$DETAIL <- gsub("travel with cone", "3", cache$DETAIL)
cache$DETAIL <- gsub("travel with old cone", "3", cache$DETAIL)


cache$DETAIL<- as.factor(cache$DETAIL)
summary(cache$DETAIL)

#Foraging 

forage <- focals19 %>% filter(BEHAVIOUR=="12")	

forage$DETAIL <- droplevels(forage$DETAIL)
summary(forage$DETAIL)

#Get rid of detials that don't make sense

forage %<>% 
	mutate(DETAIL = as.character(DETAIL)) %>%
	mutate(DETAIL = ifelse(DETAIL == "buds", "", DETAIL),
				 DETAIL = ifelse(DETAIL == "non-neighbour", "", DETAIL),
				 DETAIL = ifelse(DETAIL == "nest", "", DETAIL))

forage$DETAIL <- as.factor(forage$DETAIL)
summary(forage$DETAIL)

forage$DETAIL <- gsub("on ground ", "1", forage$DETAIL)
forage$DETAIL <- gsub("on ground", "1", forage$DETAIL)
forage$DETAIL <- gsub("in ground", "1", forage$DETAIL)
forage$DETAIL <- gsub("ground", "1", forage$DETAIL)
forage$DETAIL <- gsub("in/on midden", "4", forage$DETAIL)
forage$DETAIL <- gsub("in midden", "4", forage$DETAIL)
forage$DETAIL <- gsub("on midden", "4", forage$DETAIL)
forage$DETAIL <- gsub("on midden ", "4", forage$DETAIL)
forage$DETAIL <- gsub("midden", "4", forage$DETAIL)
forage$DETAIL <- gsub("in tree", "2", forage$DETAIL)
forage$DETAIL <- gsub("on trap", "3", forage$DETAIL)

forage$DETAIL<- as.factor(forage$DETAIL)
summary(forage$DETAIL)
levels(forage$DETAIL)

#Combine cases where a space makes it look like there are 2 levels for for the same factor (e.g. "2" and "2 ")

forage %<>%
	mutate(DETAIL = as.character(DETAIL)) %>%
	mutate(DETAIL = ifelse(DETAIL == "4 ", "4", DETAIL))

forage$DETAIL<- as.factor(forage$DETAIL)
levels(forage$DETAIL)

#Recombine all focals -----
#Now we are rbinding all of these dataframes together again -- it is important at this stage to make sure your newly bound dataframe is the same length as the initial dataframe

focals19_final <- rbind(focals.no.detail, feeding, voc, travel, interact, cache, forage)

#Provide appropriate coding for the mode of data collection for the focal
focals19_final$MODE <- gsub("casobs", "adfoc", focals19_final$MODE)
focals19_final$MODE <- gsub("adfoc", "3", focals19_final$MODE)

#Check that this worked
focals19_final$MODE <- as.factor(focals19_final$MODE)
levels(focals19_final$MODE)

#Fix the coding for handedness
summary(focals19_final$HANDEDNESS)

focals19_final$HANDEDNESS <- gsub("left", "L", focals19_final$HANDEDNESS)
focals19_final$HANDEDNESS <- gsub("right", "R", focals19_final$HANDEDNESS)
focals19_final$HANDEDNESS <- gsub("RIGHT", "R", focals19_final$HANDEDNESS)

#Check that this worked
focals19_final$HANDEDNESS <- as.factor(focals19_final$HANDEDNESS)
summary(focals19_final$HANDEDNESS)

#Create the final version for the database 
focals19_final_fordatabase <- focals19_final %>%
	mutate (id = "",
					version = "",
					date_created = DATE,
					last_updated = DATE) %>% 
	select(id, version, behaviour = BEHAVIOUR, color_left = LCOLOUR,color_right= RCOLOUR, comments = COMMENTS ,date = DATE, date_created, detail = DETAIL, grid = GRID, hand = HANDEDNESS, last_updated, locx = LOCX, locy = LOCY, mode = MODE, observer = OBS, squirrel_id = SQUIRREL_ID, tag_left = taglft, tag_right = tagrt, time = TIME)

	#Make sure date fields are in date format (this seems to be important to ensure the dates are in the correct format when merging with database)
	focals19_final_fordatabase %<>%
		mutate(date = ymd(date),
					 date_created = ymd(date_created),
					 last_updated = ymd(last_updated))
	
#Export final version
write.csv(focals19_final_fordatabase, "2019_MastCrew_focalsfordatabase.csv")

