#Code for calculating local density

#Motivation: This code enables the user to pull out local squirrel density at a designated distance from the focal individual, as set by the "distance" argument below. 

#Last modified on November 20, 2019 by E.Siracusa 

#Required packages

library(plyr)
library(dplyr)
library(magrittr)
library(lubridate)

distance <- 130 #establish your radius for collecting neighbourhood data
neighbours.all <- data.frame() #create an empty data frame to store the iterations of your loop in

#Connect to database - replace with your username below
con <- krsp::krsp_connect(host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
													dbname ="krsp",
													user="esiracus",
													password = rstudioapi::askForPassword("Password")
)

#Bring in the census - depending on what data you're working with you may need the dbamidden file or the census file or both
census <- tbl(con, "dbamidden") %>%
	collect(census)

census %<>% 
	mutate(date = ymd(date),
			 year = year(date),
			 month = month(date),
			 locX = as.numeric(locX),
			 locY = as.numeric(locY))

#Bring in the data that you want to calculate local density for -- here I am just taking a snippet of the census to use as example code 

data <- census %>% 
	filter(date == "2002-05-15",
				 grid == "KL" | grid == "SU",
				 !is.na(squirrel_id),
				 !duplicated(squirrel_id)) %>%
	mutate(local.density = "") #this creates a new column in which to store your calculated local density

n <- length(data$squirrel_id) #This is the length of your data (i.e. the squirrels that you want to create neighbourhoods and calculate density for). You can substitute squirrel_id for any column. This is the data the code below will loop through.

for (i in 1:n) {
	print(i)
	neighbours <- subset(census, census$grid==data$grid[i] & census$year==data$year[i] & census$month == data$month[i] & (30*data$locX[i]-30*census$locX)^2+(30*data$locY[i]-30*census$locY)^2<=(distance)^2) #This selects neighbours in the same grid, year, month and within 130 m of your focal individual.
	neighbours$Focal_ID <- data$squirrel_id[i] #this creates a column in your new 'neighbours' dataframe so that you can identify which focal individual you just created a neighbourhood for. 
	neighbours <- subset(neighbours, !neighbours$Focal_ID==neighbours$squirrel_id) #this makes sure your focal squirrel is not included in your neighbours.
	
	#Calculate Density (squirrels per hectare: 53,092 m2 = 5.3 hectares)
	num.indiv <-  length(unique(neighbours$squirrel_id))
	density <- num.indiv/((pi*distance^2)/10000)
	
	#Put it all together
	data[i,"local.density"] <- density
	
}

#Your data frame called "data" should now have a new column called "local.density" with the calculated values for each individual