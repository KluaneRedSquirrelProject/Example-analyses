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
#These chunks are repeated for each GPX file in your folder. There is probably a way to loop these...

##23302_2017

```r
track23302_2017 <- readGPX("Track_2017-09-27_LL_-5.7._23302.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23302_2017 <- track23302_2017$tracks[[1]][["2017-09-27 LL -5.7."]]
sq23302_2017$squirrel_id <- "23302"
sq23302_2017$year <- "2017"
sq23302_2017$gr <- "LL"
sq23302_2017$reflo <- "-5.7."
write.csv(sq23302_2017, "sq23302_2017.csv")
```

##23306_2017

```r
track23306_2017 <- readGPX("Track_2017-09-27_LL_-32_23306.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23306_2017 <- track23306_2017$tracks[[1]][["2017-09-27 LL -32"]]
sq23306_2017$squirrel_id <- "23306"
sq23306_2017$year <- "2017"
sq23306_2017$gr <- "LL"
sq23306_2017$reflo <- "-32"
write.csv(sq23306_2017, "sq23306_2017.csv")
```

##23314_2017

```r
track23314_2017 <- readGPX("Track_2017-09-27_LL_-41_23314.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23314_2017 <- track23314_2017[["tracks"]][[1]][["2017-09-27 LL -41"]]
sq23314_2017$squirrel_id <- "23314"
sq23314_2017$year <- "2017"
sq23314_2017$gr <- "LL"
sq23314_2017$reflo <- "-41"
write.csv(sq23314_2017, "sq23314_2017.csv")
```

##23307_2017

```r
track23307_2017 <- readGPX("Track_2017-09-27_LL_-52._23307.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23307_2017 <- track23307_2017[["tracks"]][[1]][["2017-09-27 LL -52."]]
sq23307_2017$squirrel_id <- "23307"
sq23307_2017$year <- "2017"
sq23307_2017$gr <- "LL"
sq23307_2017$reflo <- "-52"
write.csv(sq23307_2017, "sq23307_2017.csv")
```

##23276_2017

```r
track23276_2017 <- readGPX("Track_2017-09-27_LL_-65._23276.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23276_2017 <- track23276_2017[["tracks"]][[1]][["2017-09-27 -65. LL"]]
sq23276_2017$squirrel_id <- "23276"
sq23276_2017$year <- "2017"
sq23276_2017$gr <- "LL"
sq23276_2017$reflo <- "-65."
write.csv(sq23276_2017, "sq23276_2017.csv")
```

##23290_2017

```r
track23290_2017 <- readGPX("Track_2017-09-27_LL_A7_23290.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23290_2017 <- track23290_2017[["tracks"]][[1]][["2017-09-27 LL A7"]]
sq23290_2017$squirrel_id <- "23290"
sq23290_2017$year <- "2017"
sq23290_2017$gr <- "LL"
sq23290_2017$reflo <- "A7"
write.csv(sq23290_2017, "sq23290_2017.csv")
```

##22042_2017

```r
track22042_2017 <- readGPX("Track_2017-09-27_LL_C.1_22042.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq22042_2017 <- track22042_2017[["tracks"]][[1]][["2017-09-27 LL C.1"]]
sq22042_2017$squirrel_id <- "22042"
sq22042_2017$year <- "2017"
sq22042_2017$gr <- "LL"
sq22042_2017$reflo <- "C.1"
write.csv(sq22042_2017, "sq22042_2017.csv")
```

##23298_2017

```r
track23298_2017 <- readGPX("Track_2017-09-27_LL_E.4_23298.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23298_2017 <- track23298_2017[["tracks"]][[1]][["2017-09-27 LL E.4"]]
sq23298_2017$squirrel_id <- "23298"
sq23298_2017$year <- "2017"
sq23298_2017$gr <- "LL"
sq23298_2017$reflo <- "E.4"
write.csv(sq23298_2017, "sq23298_2017.csv")
```

##23297_2017

```r
track23297_2017 <- readGPX("Track_2017-09-27_LL_E6_23297.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23297_2017 <- track23297_2017[["tracks"]][[1]][["2017-09-27 LL E6"]]
sq23297_2017$squirrel_id <- "23297"
sq23297_2017$year <- "2017"
sq23297_2017$gr <- "LL"
sq23297_2017$reflo <- "E6"
write.csv(sq23297_2017, "sq23297_2017.csv")
```

##23244_2017

```r
track23244_2017 <- readGPX("Track_2017-09-27_LL_G0._23244.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23244_2017 <- track23244_2017[["tracks"]][[1]][["2017-09-27 LL G0."]]
sq23244_2017$squirrel_id <- "23244"
sq23244_2017$year <- "2017"
sq23244_2017$gr <- "LL"
sq23244_2017$reflo <- "G0."
write.csv(sq23244_2017, "sq23244_2017.csv")
```

##23261_2017

```r
track23261_2017 <- readGPX("Track_2017-09-27_LL_H.4._23261.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23261_2017 <- track23261_2017[["tracks"]][[1]][["2017-09-27 LL H.4."]]
sq23261_2017$squirrel_id <- "23261"
sq23261_2017$year <- "2017"
sq23261_2017$gr <- "LL"
sq23261_2017$reflo <- "H.4."
write.csv(sq23261_2017, "sq23261_2017.csv")
```

##20373_2017

```r
track20373_2017 <- readGPX("Track_2017-09-27_LL_L2_20373.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq20373_2017 <- track20373_2017[["tracks"]][[1]][["2017-09-27 LL L2"]]
sq20373_2017$squirrel_id <- "20373"
sq20373_2017$year <- "2017"
sq20373_2017$gr <- "LL"
sq20373_2017$reflo <- "L2"
write.csv(sq20373_2017, "sq20373_2017.csv")
```

##21304_2017

```r
track21304_2017 <- readGPX("Track_2017-09-27_LL_M.0._21304.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq21304_2017 <- track21304_2017[["tracks"]][[1]][["2017-09-27 LL M.0."]]
sq21304_2017$squirrel_id <- "21304"
sq21304_2017$year <- "2017"
sq21304_2017$gr <- "LL"
sq21304_2017$reflo <- "M.0."
write.csv(sq21304_2017, "sq21304_2017.csv")
```

##23271_2017

```r
track23271_2017 <- readGPX("Track_2017-09-27_LL_M.5_23271.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23271_2017 <- track23271_2017[["tracks"]][[1]][["2017-09-27 LL M.5"]]
sq23271_2017$squirrel_id <- "23271"
sq23271_2017$year <- "2017"
sq23271_2017$gr <- "LL"
sq23271_2017$reflo <- "M.5"
write.csv(sq23271_2017, "sq23271_2017.csv")
```

##23215_2017

```r
track23215_2017 <- readGPX("Track_2017-09-28_LL_-9.1._23215.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23215_2017 <- track23215_2017[["tracks"]][[1]][["2017-09-28 LL -9.1."]]
sq23215_2017$squirrel_id <- "23215"
sq23215_2017$year <- "2017"
sq23215_2017$gr <- "LL"
sq23215_2017$reflo <- "-9.1."
write.csv(sq23215_2017, "sq23215_2017.csv")
```

##23275_2017

```r
track23275_2017 <- readGPX("Track_2017-09-28_LL_-85._23275.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23275_2017 <- track23275_2017[["tracks"]][[1]][["2017-09-28 LL -85."]]
sq23275_2017$squirrel_id <- "23275"
sq23275_2017$year <- "2017"
sq23275_2017$gr <- "LL"
sq23275_2017$reflo <- "-85."
write.csv(sq23275_2017, "sq23275_2017.csv")
```

##23213_2017

```r
track23213_2017 <- readGPX("Track_2017-09-28_LL_J.0._23213.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23213_2017 <- track23213_2017[["tracks"]][[1]][["2017-09-28 LL J.0."]]
sq23213_2017$squirrel_id <- "23213"
sq23213_2017$year <- "2017"
sq23213_2017$gr <- "LL"
sq23213_2017$reflo <- "J.0."
write.csv(sq23213_2017, "sq23213_2017.csv")
```

##20371_2017

```r
track20371_2017 <- readGPX("Track_2017-09-28_LL_M.6_20371.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq20371_2017 <- track20371_2017[["tracks"]][[1]][["2017-09-28 LL M.6"]]
sq20371_2017$squirrel_id <- "20371"
sq20371_2017$year <- "2017"
sq20371_2017$gr <- "LL"
sq20371_2017$reflo <- "M.6"
write.csv(sq20371_2017, "sq20371_2017.csv")
```

##20406_2017

```r
track20406_2017 <- readGPX("Track_2017-09-28_LL_N.8_20406.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq20406_2017 <- track20406_2017[["tracks"]][[1]][["2017-09-28 LL N.8"]]
sq20406_2017$squirrel_id <- "20406"
sq20406_2017$year <- "2017"
sq20406_2017$gr <- "LL"
sq20406_2017$reflo <- "N.8"
write.csv(sq20406_2017, "sq20406_2017.csv")
```

##23227_2017

```r
track23227_2017 <- readGPX("Track_2017-09-28_LL_Q.4._23227.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23227_2017 <- track23227_2017[["tracks"]][[1]][["2017-09-28 LL Q.4."]]
sq23227_2017$squirrel_id <- "23227"
sq23227_2017$year <- "2017"
sq23227_2017$gr <- "LL"
sq23227_2017$reflo <- "Q.4."
write.csv(sq23227_2017, "sq23227_2017.csv")
```

##23258_2017

```r
track23258_2017 <- readGPX("Track_2017-09-28_LL_T.6_23258.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23258_2017 <- track23258_2017[["tracks"]][[1]][["2017-09-28 LL T.6"]]
sq23258_2017$squirrel_id <- "23258"
sq23258_2017$year <- "2017"
sq23258_2017$gr <- "LL"
sq23258_2017$reflo <- "T.6"
write.csv(sq23258_2017, "sq23258_2017.csv")
```

##12858_2017

```r
track12858_2017 <- readGPX("Track_2017-09-28_LL_W-0._12858.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq12858_2017 <- track12858_2017[["tracks"]][[1]][["2017-09-28 LL W-0."]]
sq12858_2017$squirrel_id <- "12858"
sq12858_2017$year <- "2017"
sq12858_2017$gr <- "LL"
sq12858_2017$reflo <- "W-0."
write.csv(sq12858_2017, "sq12858_2017.csv")
```

##23264_2017

```r
track23264_2017 <- readGPX("Track_2017-10-09_LL_-4.6._23264.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23264_2017 <- track23264_2017[["tracks"]][[1]][["2017-10-09 LL-4.6."]]
sq23264_2017$squirrel_id <- "23264"
sq23264_2017$year <- "2017"
sq23264_2017$gr <- "LL"
sq23264_2017$reflo <- "-4.6."
write.csv(sq23264_2017, "sq23264_2017.csv")
```

##23266_2017

```r
track23266_2017 <- readGPX("Track_2017-10-09_LL_A4_23266.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23266_2017 <- track23266_2017[["tracks"]][[1]][["2017-10-09 LLA4"]]
sq23266_2017$squirrel_id <- "23266"
sq23266_2017$year <- "2017"
sq23266_2017$gr <- "LL"
sq23266_2017$reflo <- "A4"
write.csv(sq23266_2017, "sq23266_2017.csv")
```

##23285_2017

```r
track23285_2017 <- readGPX("Track_2017-10-09_LL_R3_23285.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23285_2017 <- track23285_2017[["tracks"]][[1]][["2017-10-09 LLR3"]]
sq23285_2017$squirrel_id <- "23285"
sq23285_2017$year <- "2017"
sq23285_2017$gr <- "LL"
sq23285_2017$reflo <- "R3"
write.csv(sq23285_2017, "sq23285_2017.csv")
```

##23286_2017

```r
track23286_2017 <- readGPX("Track_2017-10-09_LL_S.3._23286.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23286_2017 <- track23286_2017[["tracks"]][[1]][["2017-10-09 LLS.3."]]
sq23286_2017$squirrel_id <- "23286"
sq23286_2017$year <- "2017"
sq23286_2017$gr <- "LL"
sq23286_2017$reflo <- "S.3."
write.csv(sq23286_2017, "sq23286_2017.csv")
```

##23259_2017

```r
track23259_2017 <- readGPX("Track_2017-10-09_LL_V.6_23259.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23259_2017 <- track23259_2017[["tracks"]][[1]][["2017-10-09 LLV.6"]]
sq23259_2017$squirrel_id <- "23259"
sq23259_2017$year <- "2017"
sq23259_2017$gr <- "LL"
sq23259_2017$reflo <- "V.6"
write.csv(sq23259_2017, "sq23259_2017.csv")
```

##23308_2017

```r
track23308_2017 <- readGPX("Track_2017-10-09_LL_V1._23308.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23308_2017 <- track23308_2017[["tracks"]][[1]][["2017-10-09 LLV1."]]
sq23308_2017$squirrel_id <- "23308"
sq23308_2017$year <- "2017"
sq23308_2017$gr <- "LL"
sq23308_2017$reflo <- "V1."
write.csv(sq23308_2017, "sq23308_2017.csv")
```

##23313_2017

```r
track23313_2017 <- readGPX("Track_2017-10-09_LL_X1_23313.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23313_2017 <- track23313_2017[["tracks"]][[1]][["2017-10-09 LLX1"]]
sq23313_2017$squirrel_id <- "23313"
sq23313_2017$year <- "2017"
sq23313_2017$gr <- "LL"
sq23313_2017$reflo <- "X1"
write.csv(sq23313_2017, "sq23313_2017.csv")
```

###Bind 2017 data into a single dataframe

```r
squordinates2017 <- rbind(sq23302_2017, 
      sq23306_2017,
      sq23314_2017, 
      sq23307_2017, 
      sq23276_2017, 
      sq23290_2017,
      sq22042_2017,
      sq23298_2017,
      sq23297_2017,
      sq23244_2017,
      sq23261_2017,
      sq20373_2017,
      sq21304_2017,
      sq23271_2017,
      sq23215_2017,
      sq23275_2017,
      sq23213_2017,
      sq20371_2017,
      sq20406_2017,
      sq23227_2017,
      sq23258_2017,
      sq12858_2017,
      sq23264_2017,
      sq23266_2017,
      sq23285_2017,
      sq23286_2017,
      sq23259_2017,
      sq23308_2017,
      sq23313_2017)

squordinates2017 <- unite_(squordinates2017, "sqyear", c("squirrel_id", "year")) 
write.csv(squordinates2017, "squordinates2017.csv") #where "sqyear" concatenates squirrel ID and year
```

######################
#####2018############
####################
##23263_2017

```r
track23263_2017 <- readGPX("Track_2017-09-27_LL_-3.0_23263.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23263_2017 <- track23263_2017$tracks[[1]][["2017-09-27 LL -3.0"]]
sq23263_2017$squirrel_id <- "23263"
sq23263_2017$year <- "2017"
sq23263_2017$gr <- "LL"
sq23263_2017$reflo <- "-3.0"
write.csv(sq23263_2017, "sq23263_2017.csv")
```

##23302_2017 

```r
track23302_2017 <- readGPX("Track_2017-09-27_LL_-5.7._23302.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23302_2017 <- track23302_2017$tracks[[1]][["2017-09-27 LL -5.7."]]
sq23302_2017$squirrel_id <- "23302"
sq23302_2017$year <- "2017"
sq23302_2017$gr <- "LL"
sq23302_2017$reflo <- "-5.7."
write.csv(sq23302_2017, "sq23302_2017.csv")
```

##23306_2017

```r
track23306_2017 <- readGPX("Track_2017-09-27_LL_-32_23306.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23306_2017 <- track23306_2017$tracks[[1]][["2017-09-27 LL -32"]]
sq23306_2017$squirrel_id <- "23306"
sq23306_2017$year <- "2017"
sq23306_2017$gr <- "LL"
sq23306_2017$reflo <- "-32"
write.csv(sq23306_2017, "sq23306_2017.csv")
```

##23314_2017

```r
track23314_2017 <- readGPX("Track_2017-09-27_LL_-41_23314.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23314_2017 <- track23314_2017[["tracks"]][[1]][["2017-09-27 LL -41"]]
sq23314_2017$squirrel_id <- "23314"
sq23314_2017$year <- "2017"
sq23314_2017$gr <- "LL"
sq23314_2017$reflo <- "-41"
write.csv(sq23314_2017, "sq23314_2017.csv")
```

##23307_2017

```r
track23307_2017 <- readGPX("Track_2017-09-27_LL_-52._23307.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23307_2017 <- track23307_2017[["tracks"]][[1]][["2017-09-27 LL -52."]]
sq23307_2017$squirrel_id <- "23307"
sq23307_2017$year <- "2017"
sq23307_2017$gr <- "LL"
sq23307_2017$reflo <- "-52"
write.csv(sq23307_2017, "sq23307_2017.csv")
```

##23276_2017

```r
track23276_2017 <- readGPX("Track_2017-09-27_LL_-65._23276.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23276_2017 <- track23276_2017[["tracks"]][[1]][["2017-09-27 -65. LL"]]
sq23276_2017$squirrel_id <- "23276"
sq23276_2017$year <- "2017"
sq23276_2017$gr <- "LL"
sq23276_2017$reflo <- "-65."
write.csv(sq23276_2017, "sq23276_2017.csv")
```

##23290_2017

```r
track23290_2017 <- readGPX("Track_2017-09-27_LL_A7_23290.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23290_2017 <- track23290_2017[["tracks"]][[1]][["2017-09-27 LL A7"]]
sq23290_2017$squirrel_id <- "23290"
sq23290_2017$year <- "2017"
sq23290_2017$gr <- "LL"
sq23290_2017$reflo <- "A7"
write.csv(sq23290_2017, "sq23290_2017.csv")
```

##22042_2017

```r
track22042_2017 <- readGPX("Track_2017-09-27_LL_C.1_22042.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq22042_2017 <- track22042_2017[["tracks"]][[1]][["2017-09-27 LL C.1"]]
sq22042_2017$squirrel_id <- "22042"
sq22042_2017$year <- "2017"
sq22042_2017$gr <- "LL"
sq22042_2017$reflo <- "C.1"
write.csv(sq22042_2017, "sq22042_2017.csv")
```

##23298_2017

```r
track23298_2017 <- readGPX("Track_2017-09-27_LL_E.4_23298.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23298_2017 <- track23298_2017[["tracks"]][[1]][["2017-09-27 LL E.4"]]
sq23298_2017$squirrel_id <- "23298"
sq23298_2017$year <- "2017"
sq23298_2017$gr <- "LL"
sq23298_2017$reflo <- "E.4"
write.csv(sq23298_2017, "sq23298_2017.csv")
```

##23297_2017

```r
track23297_2017 <- readGPX("Track_2017-09-27_LL_E6_23297.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23297_2017 <- track23297_2017[["tracks"]][[1]][["2017-09-27 LL E6"]]
sq23297_2017$squirrel_id <- "23297"
sq23297_2017$year <- "2017"
sq23297_2017$gr <- "LL"
sq23297_2017$reflo <- "E6"
write.csv(sq23297_2017, "sq23297_2017.csv")
```

##23244_2017

```r
track23244_2017 <- readGPX("Track_2017-09-27_LL_G0._23244.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23244_2017 <- track23244_2017[["tracks"]][[1]][["2017-09-27 LL G0."]]
sq23244_2017$squirrel_id <- "23244"
sq23244_2017$year <- "2017"
sq23244_2017$gr <- "LL"
sq23244_2017$reflo <- "G0."
write.csv(sq23244_2017, "sq23244_2017.csv")
```

##23261_2017

```r
track23261_2017 <- readGPX("Track_2017-09-27_LL_H.4._23261.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23261_2017 <- track23261_2017[["tracks"]][[1]][["2017-09-27 LL H.4."]]
sq23261_2017$squirrel_id <- "23261"
sq23261_2017$year <- "2017"
sq23261_2017$gr <- "LL"
sq23261_2017$reflo <- "H.4."
write.csv(sq23261_2017, "sq23261_2017.csv")
```

##20373_2017

```r
track20373_2017 <- readGPX("Track_2017-09-27_LL_L2_20373.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq20373_2017 <- track20373_2017[["tracks"]][[1]][["2017-09-27 LL L2"]]
sq20373_2017$squirrel_id <- "20373"
sq20373_2017$year <- "2017"
sq20373_2017$gr <- "LL"
sq20373_2017$reflo <- "L2"
write.csv(sq20373_2017, "sq20373_2017.csv")
```

##21304_2017

```r
track21304_2017 <- readGPX("Track_2017-09-27_LL_M.0._21304.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq21304_2017 <- track21304_2017[["tracks"]][[1]][["2017-09-27 LL M.0."]]
sq21304_2017$squirrel_id <- "21304"
sq21304_2017$year <- "2017"
sq21304_2017$gr <- "LL"
sq21304_2017$reflo <- "M.0."
write.csv(sq21304_2017, "sq21304_2017.csv")
```

##23271_2017

```r
track23271_2017 <- readGPX("Track_2017-09-27_LL_M.5_23271.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23271_2017 <- track23271_2017[["tracks"]][[1]][["2017-09-27 LL M.5"]]
sq23271_2017$squirrel_id <- "23271"
sq23271_2017$year <- "2017"
sq23271_2017$gr <- "LL"
sq23271_2017$reflo <- "M.5"
write.csv(sq23271_2017, "sq23271_2017.csv")
```

##23215_2017

```r
track23215_2017 <- readGPX("Track_2017-09-28_LL_-9.1._23215.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23215_2017 <- track23215_2017[["tracks"]][[1]][["2017-09-28 LL -9.1."]]
sq23215_2017$squirrel_id <- "23215"
sq23215_2017$year <- "2017"
sq23215_2017$gr <- "LL"
sq23215_2017$reflo <- "-9.1."
write.csv(sq23215_2017, "sq23215_2017.csv")
```

##23275_2017

```r
track23275_2017 <- readGPX("Track_2017-09-28_LL_-85._23275.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23275_2017 <- track23275_2017[["tracks"]][[1]][["2017-09-28 LL -85."]]
sq23275_2017$squirrel_id <- "23275"
sq23275_2017$year <- "2017"
sq23275_2017$gr <- "LL"
sq23275_2017$reflo <- "-85."
write.csv(sq23275_2017, "sq23275_2017.csv")
```

##23213_2017

```r
track23213_2017 <- readGPX("Track_2017-09-28_LL_J.0._23213.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23213_2017 <- track23213_2017[["tracks"]][[1]][["2017-09-28 LL J.0."]]
sq23213_2017$squirrel_id <- "23213"
sq23213_2017$year <- "2017"
sq23213_2017$gr <- "LL"
sq23213_2017$reflo <- "J.0."
write.csv(sq23213_2017, "sq23213_2017.csv")
```

##20371_2017

```r
track20371_2017 <- readGPX("Track_2017-09-28_LL_M.6_20371.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq20371_2017 <- track20371_2017[["tracks"]][[1]][["2017-09-28 LL M.6"]]
sq20371_2017$squirrel_id <- "20371"
sq20371_2017$year <- "2017"
sq20371_2017$gr <- "LL"
sq20371_2017$reflo <- "M.6"
write.csv(sq20371_2017, "sq20371_2017.csv")
```

##20406_2017

```r
track20406_2017 <- readGPX("Track_2017-09-28_LL_N.8_20406.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq20406_2017 <- track20406_2017[["tracks"]][[1]][["2017-09-28 LL N.8"]]
sq20406_2017$squirrel_id <- "20406"
sq20406_2017$year <- "2017"
sq20406_2017$gr <- "LL"
sq20406_2017$reflo <- "N.8"
write.csv(sq20406_2017, "sq20406_2017.csv")
```

##23227_2017

```r
track23227_2017 <- readGPX("Track_2017-09-28_LL_Q.4._23227.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23227_2017 <- track23227_2017[["tracks"]][[1]][["2017-09-28 LL Q.4."]]
sq23227_2017$squirrel_id <- "23227"
sq23227_2017$year <- "2017"
sq23227_2017$gr <- "LL"
sq23227_2017$reflo <- "Q.4."
write.csv(sq23227_2017, "sq23227_2017.csv")
```

##23258_2017

```r
track23258_2017 <- readGPX("Track_2017-09-28_LL_T.6_23258.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23258_2017 <- track23258_2017[["tracks"]][[1]][["2017-09-28 LL T.6"]]
sq23258_2017$squirrel_id <- "23258"
sq23258_2017$year <- "2017"
sq23258_2017$gr <- "LL"
sq23258_2017$reflo <- "T.6"
write.csv(sq23258_2017, "sq23258_2017.csv")
```

##12858_2017

```r
track12858_2017 <- readGPX("Track_2017-09-28_LL_W-0._12858.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq12858_2017 <- track12858_2017[["tracks"]][[1]][["2017-09-28 LL W-0."]]
sq12858_2017$squirrel_id <- "12858"
sq12858_2017$year <- "2017"
sq12858_2017$gr <- "LL"
sq12858_2017$reflo <- "W-0."
write.csv(sq12858_2017, "sq12858_2017.csv")
```

##23264_2017

```r
track23264_2017 <- readGPX("Track_2017-10-09_LL_-4.6._23264.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23264_2017 <- track23264_2017[["tracks"]][[1]][["2017-10-09 LL-4.6."]]
sq23264_2017$squirrel_id <- "23264"
sq23264_2017$year <- "2017"
sq23264_2017$gr <- "LL"
sq23264_2017$reflo <- "-4.6."
write.csv(sq23264_2017, "sq23264_2017.csv")
```

##23266_2017

```r
track23266_2017 <- readGPX("Track_2017-10-09_LL_A4_23266.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23266_2017 <- track23266_2017[["tracks"]][[1]][["2017-10-09 LLA4"]]
sq23266_2017$squirrel_id <- "23266"
sq23266_2017$year <- "2017"
sq23266_2017$gr <- "LL"
sq23266_2017$reflo <- "A4"
write.csv(sq23266_2017, "sq23266_2017.csv")
```

##23285_2017

```r
track23285_2017 <- readGPX("Track_2017-10-09_LL_R3_23285.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23285_2017 <- track23285_2017[["tracks"]][[1]][["2017-10-09 LLR3"]]
sq23285_2017$squirrel_id <- "23285"
sq23285_2017$year <- "2017"
sq23285_2017$gr <- "LL"
sq23285_2017$reflo <- "R3"
write.csv(sq23285_2017, "sq23285_2017.csv")
```

##23286_2017

```r
track23286_2017 <- readGPX("Track_2017-10-09_LL_S.3._23286.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23286_2017 <- track23286_2017[["tracks"]][[1]][["2017-10-09 LLS.3."]]
sq23286_2017$squirrel_id <- "23286"
sq23286_2017$year <- "2017"
sq23286_2017$gr <- "LL"
sq23286_2017$reflo <- "S.3."
write.csv(sq23286_2017, "sq23286_2017.csv")
```

##23259_2017

```r
track23259_2017 <- readGPX("Track_2017-10-09_LL_V.6_23259.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23259_2017 <- track23259_2017[["tracks"]][[1]][["2017-10-09 LLV.6"]]
sq23259_2017$squirrel_id <- "23259"
sq23259_2017$year <- "2017"
sq23259_2017$gr <- "LL"
sq23259_2017$reflo <- "V.6"
write.csv(sq23259_2017, "sq23259_2017.csv")
```

##23308_2017

```r
track23308_2017 <- readGPX("Track_2017-10-09_LL_V1._23308.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23308_2017 <- track23308_2017[["tracks"]][[1]][["2017-10-09 LLV1."]]
sq23308_2017$squirrel_id <- "23308"
sq23308_2017$year <- "2017"
sq23308_2017$gr <- "LL"
sq23308_2017$reflo <- "V1."
write.csv(sq23308_2017, "sq23308_2017.csv")
```

##23313_2017

```r
track23313_2017 <- readGPX("Track_2017-10-09_LL_X1_23313.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)

sq23313_2017 <- track23313_2017[["tracks"]][[1]][["2017-10-09 LLX1"]]
sq23313_2017$squirrel_id <- "23313"
sq23313_2017$year <- "2017"
sq23313_2017$gr <- "LL"
sq23313_2017$reflo <- "X1"
write.csv(sq23313_2017, "sq23313_2017.csv")
```

###Bind 2017 data####

```r
squordinates2017 <- rbind(sq23302_2017, 
      sq23306_2017,
      sq23314_2017, 
      sq23307_2017, 
      sq23276_2017, 
      sq23290_2017,
      sq22042_2017,
      sq23298_2017,
      sq23297_2017,
      sq23244_2017,
      sq23261_2017,
      sq20373_2017,
      sq21304_2017,
      sq23271_2017,
      sq23215_2017,
      sq23275_2017,
      sq23213_2017,
      sq20371_2017,
      sq20406_2017,
      sq23227_2017,
      sq23258_2017,
      sq12858_2017,
      sq23264_2017,
      sq23266_2017,
      sq23285_2017,
      sq23286_2017,
      sq23259_2017,
      sq23308_2017,
      sq23313_2017)

squordinates2017 <- unite_(squordinates2017, "sqyear", c("squirrel_id", "year")) 
write.csv(squordinates2017, "squordinates2017.csv")
```
