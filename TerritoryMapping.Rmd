---
title: "TERRITORYMAPS"
author: "AE WISHART"
date: '2018-09-22'
output: pdf_document
---
This code is for pulling behaviour data from the local database at Squirrel Camp and focal data saved to the local drive, restricting it by date, subsetting by individual squirrel ID, and then mapping that squirrel's territorial behaviours on the appropriate grid. The spatial coverage of the map can be adjusted to eliminate extreme outliers (i.e., a rattle several gridstakes over) to focus on the squirrel's territory. Maps can then be printed and taken into the field to track boundaries with GPS tracks (see Territory Boundary Protocol from 2017). 
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(krsp)
library(lubridate)
library(ggplot2)

```

# Import Data into R from the local instance of the `krsp` database
```{r send-query, collapse=TRUE}

#Connect to local database at camp. MUST UPDATE HOST AND USERNAME HERE. 
connection_1 <- krsp_connect(host = "________", user = "___________")

#Select behaviours - codes from DbaseIV codes Edited 2017

#0 = other (details in comments)	
#1 = feeding	
              # 1 = animal material
              #	2 = cone
	            # 3 = buds 
              # 4 = mushroom, truffle 
	            # 5 = bark 
              #	6 = new cone 
	            # 7 = willow leaves 
	           #   8 = poplar buds 
	            #  9 = bearberry flower 
	             # 10 = sunflower  seeds
	              #11 = pollen cone 
              	#12 = wood (nestbox)
              	#13 = new spruce needle 
	              #14 = PB on trap 
	              #15 = off aspen leaves 
              	#16 = bearberry berries 
              	#17 = off grass 
              	#18 = fireweed 
              	#19 = witches broom 
              	#20 = white scales on willow bark (red stuff inside) 
              	#21 = aspen catkin
              	#22 = licking water off leaves
              	#23 = lichen/fungus
	              #24 = PB
              	#25 = unknown material
              	#26 = apple (e.g., from hare traps)
              	#27 = willow buds (new in 2000)
              	#28 = bark beetle larvae (new in 2002)
	              #29 = insects (new in 2004)
	              #30 = snow
              	#31 = new mushroom
              	#32 = feeding on truffle (observed digging up)
	
#2 = vocalizations 	
              # 1 = rattle
              #	2 = screech
              #	3 = wheeze
              #  4 = squeak
	            #  5 = bark
	           #   6 = rattle-chew
	            #  7= buzz
#3 = travelling
              #	1 = on ground
              #	2 = in tree
#4 = resting
#5 = in nest 
#6 = off territory
#7 = interaction with another squirrel
              #	1 = neighbour
              #	2 = untagged sq.
	           #   3 = juvenile
	            #  4 = unidentified sq.
              #	5 = mom
              #	6 = sibling
              #	7 = non-neighbour
#8 = caching behav 
               # 1 = cutting cones
              #	2 = travel /c shroom
	             # 3 = travel with cone
	             # 4 = cache mushrm 
              #	5 = cache cone
              #	6 = digging holes
#9 = dead*	
               # 1 = terrestrial pred.
	             # 2 = avian pred.
              #	3 = unknown pred. 
              #	4 = 'natural' death
	             # 5 = unknown cause
              #	6 = trap/handling
              #	7 = road kill
              #	8 = planned euthanasia

#10 = groom
#11 = play
#12 = foraging	
               # 1 = on ground
              #	2 = in tree
	             # 3 = on trap 
	             # 4 = in/on midden (new in 1997)
#13 = out of sight
#14 = nest building
#15 = unknown- not visible (but know where it is)
#16 = scent marking
#17 = moving kids
#18 = trapped off territory/not handled
#19 = vigilant
#20 = digging for truffles
#21 = foot stomping
#22 = mating chase (new in 1995)	
#23 = suckling (new in 1997)

#Fates codes of selected territorial behaviours
terrbehavs <- c(2, 7, 8, 14, 16, 21 )


#Importing behavioural data
behavs2 <-tbl(connection_1, "behaviour") %>%
  dplyr::select(squirrel_id, id, locx, locy, grid, date, behaviour, detail, color_left, color_right) %>%
  collect %>%
  mutate(locy = loc_to_numeric(locy),
         year = year(ymd(date)),
        month = month(ymd(date))) %>%
    filter(!is.na(squirrel_id), !is.na(behaviour), behaviour %in% terrbehavs, grid=="AG") 

#Import local focal data from within season
focalbehavs <-read.csv("terrbehavs.csv") 
focalbehavs$X <- NULL
focalbehavs$locxn <- NULL
focalbehavs$loclabels <- NULL
focalbehavs$X.1 <- NULL
focalbehavs$X.2 <- NULL
focalbehavs$X.3 <- NULL
focalbehavs$X.4 <- NULL
focalbehavs$locy <- as.numeric(focalbehavs$locy)


focalbehavs <- focalbehavs %>%
  dplyr::select(squirrel_id, id, locx, locy, grid, date, behaviour, detail, color_left, color_right) %>%
  collect %>%
  mutate(locy = loc_to_numeric(locy),
         year = year(ymd(date)),
        month = month(ymd(date))) %>%
    filter(!is.na(squirrel_id), !is.na(behaviour), behaviour %in% terrbehavs, grid=="LL") 

behavs <- rbind(behavs2, focalbehavs)
behavs <- behavs2
#Get list of squirrel IDs 
squirrels <- unique(behavs$squirrel_id)
squirrels

 
#Get data into right format
behavs <- as.data.frame(behavs)
behavs$squirrel_id <- as.factor(behavs$squirrel_id)
squ_id <- behavs$squirrel_id
behavs$locxn <- loc_to_numeric(behavs$locx)
behavs$loclabels <- paste(behavs$locx, behavs$locy, sep = ", ")
```
#Run this code for individual squirrels selected from list of squirrel IDs just generated.
```{r}
#Filter for individual squirrel - change squirrel ID as necessary
sqbehav <- behavs %>%
  filter(squirrel_id == "22925")
  
#define locX and locY breakpoints 
locpoints <- c(-0.2, 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3.0)

locpoints2 <- c(-0.2, 0.0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6, 2.8, 3.0)

xbreaks <- c(min(sqbehav$locxn) + locpoints2)
ybreaks <- c(min(sqbehav$locy) + locpoints2)

xmin <- (min(sqbehav$locxn) - 2.2)
xmax <- (max(sqbehav$locxn) + 2.2)
ymin <- (min(sqbehav$locy) - 2.2)
ymax <- (max(sqbehav$locy) + 2.2)
    
#Use this piece of code to filter based on time (e.g., month >=8 will limit the dataset to any behaviours that occurred Aug 1 or later in the year). The next filtering criteria is to limit locs to a local scale - especially useful if a squirrel has moved territories or had one behaviour in an odd loc. Recommend to run it only after plotting initial behaviours and assessing the map. Most times just specifying >= or <= one loc value will suffice. Example:filter(locxn >=3, locxn<= 7, locy >= 9, locy <= 19 )

sqbehav <- sqbehav %>%
  filter(month >=8) 
#%>%
#  filter(locxn >=5, locy <=4)


#Finally, plot the behaviours. 
territory <- ggplot(sqbehav, aes(x=locxn, y=locy, color = sqbehav$behaviour)) + 
 geom_point(aes(shape=squirrel_id)) +
  coord_fixed() + #fix unit ratio to default 1:1 
  geom_text(aes(label=sqbehav$loclabels), size=3, color = "black") + #label locs
  geom_point(position = position_jitter(w=0.03, h=0.03)) + #jitter to reveal multiple behavs at same loc
  theme(panel.grid.minor = element_line(size= 0.1), panel.grid.major = element_line(size = (0.1)), panel.background = element_rect(fill = "white"), axis.text = element_text(size = 10),legend.text = element_text(size = 10), plot.margin = margin(.4, .4, .4, .4, "cm")) #+
 # scale_x_continuous(breaks = xbreaks) + 
 # scale_y_continuous(breaks = ybreaks)+
 #coord_cartesian(ylim=c(-1,10))

territory


```

#This code can be used to plot selected behaviours (as defined above) for all squirrels across a given grid (as defined above). This can help zero in on squirrels that have sufficient behavioural observations to confidentely map. 
```{r}

gridbehavs <- behavs # %>%
#  filter(locxn >=8, locxn<=9,
#         locy >=5.5, locy <=7)
grid <- ggplot(gridbehavs, aes(x=locxn, y=locy, color = gridbehavs$squirrel_id)) + 
  coord_fixed() + #fix unit ratio to default 1:1
  geom_text(aes(label=gridbehavs$squirrel_id), size=3, color = "black") + #label locs on plot
  geom_point(position = position_jitter(w=0.03, h=0.03)) + #jitter to reveal multiple behavs at same loc
  theme(panel.grid.minor = element_line(size= 0.1), panel.grid.major = element_line(size = (0.1)), panel.background = element_rect(fill = "white"), axis.text = element_text(size = 10),legend.text = element_text(size = 10), plot.margin = margin(.4, .4, .4, .4, "cm")) +
  scale_x_continuous(breaks = xbreaks) + 
  scale_y_continuous(breaks = ybreaks)

grid

```




