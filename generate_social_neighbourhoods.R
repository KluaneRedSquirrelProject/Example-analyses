#Code calculating familiarity of red squirrel social neighbourhoods

#Motivation: This code enables the user to establish social neighbourhoods for individual squirrels and to then calculate pairwise familiarity between each focal individual and all neighbours. These measures are then averaged to create a measure of average familiarity for each social neighbourhood.

#NOTE: This is a snippet of example code only that the user should be able to tweak and apply to calculate familiarity for their individuals of interest. Please note that there are other challenges if you want to calculate familiarity across the entire long term data (e.g. such needing to merge the dbamidden and census files). Please feel free to contact Erin Siracusa (erinsiracusa@gmail.com) if you would like additional code or assistance with applying this across this long term data.

#Last modified on November 25, 2019 by E.Siracusa 

#Required packages

library(plyr)
library(dplyr)
library(magrittr)
library(lubridate)

#Connect to database - replace with your username below
con <- krsp::krsp_connect(host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
													dbname ="krsp",
													user="esiracus",
													password = rstudioapi::askForPassword("Password")
)

#Bring in the dbamidden - depending on what data you're working with you may need the dbamidden file or the census file or both
Mid <- tbl(con, "dbamidden") %>%
	collect(dbamidden)

#Clean up census file
Mid %<>% 
	mutate(date = ymd(date),
				 year = year(date),
				 month = month(date),
				 locX = as.numeric(locX),
				 locY = as.numeric(locY),
				 reflo = as.character(reflo),
				 Sex = as.character(Sex),
				 grid = as.factor(grid),
				 tagLft = as.character(tagLft),
				 tagRt = as.character(tagRt),
				 squirrel_id = as.character(squirrel_id)) %>%
#Eliminate all entries with no reflo, LocX or LocY
	filter(!is.na(reflo),
				 !is.na(locX),
				 !is.na(locY)) %>%
#Select only grids you are interested in
	filter(grid == "KL" | grid =="SU") %>%
	droplevels() %>%
#Select primary middens only (Note that this code will change if you are using the census rather than the dbamidden file)
	filter(def == "4") %>%
#Select only the columns that are relevant 
	select(squirrel_id, grid, date, year, month, reflo, locX, locY, tagLft, tagRt, col, Sex, def)

#Bring in the data that you want to create social neighbourhoods for -- here I am just taking a snippet of the census to use as example code 

data <- Mid %>% 
	filter(date == "2008-05-15",
				 grid == "KL" | grid == "SU",
				 !is.na(squirrel_id))

#Calculating Neighbourhoods
distance <- 130 #establish your radius for collecting neighbourhood data
neighbours.all <- data.frame() #create an empty data frame to store the iterations of your loop in

n <- length(data$squirrel_id) #This is the length of your data (i.e. the squirrels that you want to create neighbourhoods and calculate density for). You can substitute squirrel_id for any column. This is the data the code below will loop through.

for (j in 1:n) {
	print(j)
	#this selects neighbours in the same grid, year, month and within 130 m of your focal individual.
	neighbours <- subset(Mid, Mid$grid==data$grid[j] & Mid$year==data$year[j] & Mid$month==data$month[j] & (30*data$locX[j]-30*Mid$locX)^2+(30*data$locY[j]-30*Mid$locY)^2<=(distance)^2)
	#this creates new columns in your 'neighbours' dataframe so that you can identify which focal individual you just created a neighbourhood for and adds back some of the information about the focal individual
	neighbours$Focal.ID <- data$squirrel_id[j]
	neighbours$Focal.locX <- data$locX[j]
	neighbours$Focal.locY <- data$locY[j]
	neighbours$Focal.tagLft <- data$tagLft[j]
	neighbours$Focal.tagRt <- data$tagRt[j]
	neighbours$Focal.reflo <- data$reflo[j]
	neighbours$Focal.Sex <- data$Sex[j]
	neighbours$Focal.Date <- data$date[j]
	#this makes sure your focal squirrel is not included in your neighbours.
	neighbours <- subset(neighbours, !neighbours$Focal.ID==neighbours$squirrel_id)
	
#Calculate Distance			
	n <- length(neighbours$Focal.ID)
	for(i in 1:n){
		dis <- sqrt((30*(neighbours$Focal.locX[i])-30*(neighbours$locX[i]))^2+(30*(neighbours$Focal.locY[i])-30*(neighbours$locY[i]))^2)
		neighbours[i,"Nbor.dis"] <- dis
	}	
	
#this serves as a double check and helps catch any mistakes in the database where two squirrels were assigned the same primary midden				
	neighbours %<>% filter(Nbor.dis > 0)
	
#Calculate Familiarity - here we are going to cycle through each neighbour and see how long the focal squirrel and neighbouring squirrel have occupied their current territories next to each other
	n <- length(neighbours$Focal.ID)
	for(i in 1:n){
		#this selects only census information that is on or before the date that you want to calculate familiarity from	
		fam1 <- subset(Mid, Mid$date<=neighbours$date[i])
		#here I am using an if/else clause to prevent the loop from ending prematurely if it encounters an error	-- instead I will be able to look back and see where it encountered a problem by noting where familiarity was calculated as "NA"	
		if (neighbours$Focal.ID[i] %in% fam1$squirrel_id & neighbours$squirrel_id[i] %in% fam1$squirrel_id){
			#select all instances where the focal squirrel lived at this reflo
			fam2 <- subset(fam1, fam1$squirrel_id==neighbours$Focal.ID[i] & fam1$reflo==neighbours$Focal.reflo[i])
			#select the earliest date that that the focal squirrel lived at this reflo
			own <- min(fam2$date)
			#select all instances where the neighbouring squirrel lived at this reflo
			fam3 <- subset(fam1, fam1$squirrel_id==neighbours$squirrel_id[i] & fam1$reflo==neighbours$reflo[i])
			#selet the earliest date that the neighbouring squirrel lived at this reflo
			nbor <- min(fam3$date)
			#take the latest date of these two (this is the earliest date that these individuals lived next to each other  -- it does not take into account the possibility that the neighbouring squirrel moved from another nearby midden, but anecdotal evidence from the field suggests that anytime a neighbour moves to another nearby midden the surrounding squirrels treat that individual as a "new" neighbour)
			f <- max(own,nbor)
			#subtract this date from the current date to get the length of time these individuals have lived next to each other
			f <- neighbours$date[i]-f
			neighbours[i,"Nbor.familiarity"] <- f
		} else {
			neighbours[i,"Nbor.familiarity"] <- NA
		}
	}

#NOTE: Because neighbours are occassionally censused in the same month but a few days after the 'owner' this code occassionally creates negative familiarity values. For example if the owner census date is May 15, 2001 and the neighbour census date is May 17, 2001, and the first date that both squirrels are observed on neighbouring middens is May 17, 2001 this will lead to a negative familarity value of -2 (May 15, 2001 - May 17, 2001). To avoid these negative familiarity values you can change all negative values to zeros using the code below. This is a rough approximation but appropriate given that our measure of familiarity is also an approximation (i.e. familiarity is only updated twice per year).
	
	#neighbours %<>% 
	#	mutate(Nbor.ten = ifelse(Nbor.ten < 0, 0, Nbor.ten))
	
#Calculate averages for the neighbourhood	
	neighbours %<>% 
		mutate(Avg.familiarity = mean(Nbor.familiarity, na.rm=T))
	
#Put it all together 
	neighbours.all <- rbind(neighbours.all,neighbours)
}

#The neighbours.all dataframe has a row for each neighbour. If you are only interested in having a single row per neighbourhood you can condense the dataframe:

neighbourhoods.all <- neighbours.all %>%
	mutate(DateIDRef = paste(Focal.Date,Focal.ID,Focal.reflo, sep = " ")) %>%
	filter(!duplicated(DateIDRef))

#As a quick check- the number of neighbourhoods (i.e. rows in the "neighbourhoods.all" dataframe) should match the number of rows in your intial dataframe (in this case the "data" dataframe)