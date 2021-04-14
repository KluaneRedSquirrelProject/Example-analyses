###################
## Andrew McAdam ##
##   April 2021  ##
###################

library (krsp)
library (tidyverse)
library(lubridate)
library (knitr)
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp_suppl",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)


trees<-tbl(con, "trees") %>% 
  filter(tree=="tree") %>% 
  collect()

plot(y~x, data=trees, pch=19, cex=0.05, col="green", xlab="UTM easting", ylab="UTM northing")

plot(y~x, data=trees, pch=19, cex=trees$radius*0.645-1.2, col="green", xlab="UTM easting", ylab="UTM northing")


plot(y~x, data=trees, pch=19, 
     cex=trees$radius*0.645, 
     col="green", 
     xlab="UTM easting", 
     ylab="UTM northing",
     xlim=c(660650, 660700),
     ylim=c(6761650, 6761700))


hist(trees$height)
