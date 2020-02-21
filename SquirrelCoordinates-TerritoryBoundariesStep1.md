---
title: "Squordinates"
author: "AEW"
date: '2019-03-24'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---
This Squordinates code (Squirrel Coordinates) reads in individual GPX files, i.e., "territory tracks", obtained by walking the boundaries of a defended squirrel territory with a Garmin GPS handheld unit, following Territory Boundaries protocol. Briefly, territorial behaviours are mapped for individual squirrels (can use AEW's TerritoryMapping.md code in the field to map and scale behaviours for individual squirrels), maps are printed, and the defended borders are walked by the squirreler *SLOWLY* ensuring good GPS signal and resolution, and then saving the track with the date, grid, reflo, and squirrel ID in the filename on the GPS unit. These GPX files are then uploaded to a computer using the GPS-USB cable and read into R using the code below. Territory boundaries are typically mapped in the fall (September-October) once squirrels have settled. 


##Load Packages

```r
library(tidyverse)
```

```
## ── Attaching packages ───────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 3.1.1       ✔ purrr   0.3.2  
## ✔ tibble  2.1.1       ✔ dplyr   0.8.0.1
## ✔ tidyr   0.8.3       ✔ stringr 1.4.0  
## ✔ readr   1.3.1       ✔ forcats 0.4.0
```

```
## ── Conflicts ──────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(here)
```

```
## here() starts at /Users/drea/Documents/PhD/Analysis - Sq Territories
```

```r
library(sf)
```

```
## Linking to GEOS 3.6.1, GDAL 2.1.3, PROJ 4.9.3
```

```r
library(ggmap)
```

```
## Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.
```

```
## Please cite ggmap if you use it! See citation("ggmap") for details.
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:here':
## 
##     here
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(adehabitatHR)
```

```
## Loading required package: sp
```

```
## Loading required package: deldir
```

```
## deldir 0.1-16
```

```
## Loading required package: ade4
```

```
## Loading required package: adehabitatMA
```

```
## Loading required package: adehabitatLT
```

```
## Loading required package: CircStats
```

```
## Loading required package: MASS
```

```
## 
## Attaching package: 'MASS'
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```
## Loading required package: boot
```

```
## 
## Attaching package: 'adehabitatLT'
```

```
## The following object is masked from 'package:dplyr':
## 
##     id
```

```r
library(plotKML)
```

```
## plotKML version 0.5-9 (2019-01-04)
```

```
## URL: http://plotkml.r-forge.r-project.org/
```

```r
library(rgdal)
```

```
## rgdal: version: 1.4-3, (SVN revision 828)
##  Geospatial Data Abstraction Library extensions to R successfully loaded
##  Loaded GDAL runtime: GDAL 2.1.3, released 2017/20/01
##  Path to GDAL shared files: /Library/Frameworks/R.framework/Versions/3.5/Resources/library/rgdal/gdal
##  GDAL binary built with GEOS: FALSE 
##  Loaded PROJ.4 runtime: Rel. 4.9.3, 15 August 2016, [PJ_VERSION: 493]
##  Path to PROJ.4 shared files: /Library/Frameworks/R.framework/Versions/3.5/Resources/library/rgdal/proj
##  Linking to sp version: 1.3-1
```
##23263_2017
## Each chunk is named with the squirrel ID and year (as a given squirrel can be tracked multiple years). 

```r
track23263_2017 <- readGPX("Track_2017-09-27_LL_-3.0_23263.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)
#Now look at the Environment tab (if using R Studio). The object just named trackXXXXX_20XX should be a List of 5, with tracks containing a List of 1. Under tracks, you will see after the $ information from the filename, in this case, "2017-09-27 LL -3.0" followed by ":'data.frame':....". You may need to copy the first bit after the dollar sign and past it into the next line of code if it is in a different format:

sq23263_2017 <- track23263_2017$tracks[[1]][["2017-09-27 LL -3.0"]] #here we are creating a new dataframe from the Tracks contained within the GPX file. 
sq23263_2017$squirrel_id <- "23263" #create a column for squirrel_id
sq23263_2017$year <- "2017" #create a column for year
sq23263_2017$gr <- "LL" #create a column for grid 
sq23263_2017$reflo <- "-3.0" #create a column for reflo

#Your new dataframe should contain X obs (dependent on how many GPS points the unit picked up in the field) of 8 variables.
write.csv(sq23263_2017, "sq23263_2017.csv") #write the data as its own CSV file.
```
#Repeat the above chunk for each GPX file in your folder. There is probably a way to loop these...

###Bind 2017 data into a single dataframe

```r
squordinates2017 <- rbind(sq23302_2017, 
      sq23313_2017) #include all your squirrel-year dataframes you have created above

squordinates2017 <- unite_(squordinates2017, "sqyear", c("squirrel_id", "year")) 
write.csv(squordinates2017, "squordinates2017.csv") #where "sqyear" concatenates squirrel ID and year
```
#Repeat for 2018, 2019 etc.
