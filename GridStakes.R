require(rgdal)
library (tidyverse)
library (krsp)

############################
# Connection to krsp_suppl #
############################
con_suppl <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                      dbname ="krsp_suppl",
                      username = Sys.getenv("krsp_user"),
                      password = Sys.getenv("krsp_password")
)

#####################################
# Import Grid Stake Lat Long Values #
#####################################
grid_stakes<-tbl(con_suppl, "grid_stakes") %>% 
  filter(!is.na(north),
         !is.na(west)) %>% 
  collect()

# Convert to spatial coordinates
cord.dec = SpatialPoints(cbind(-grid_stakes$west, grid_stakes$north), proj4string=CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS("+proj=utm +zone=7N +datum=WGS84"))

summary(cord.UTM)


##################
# Still Problems:
# BT - S1
# Others still to be checked.  See GridStakes_error_checking.R

#######################################
# Tree plot with grid stakes over top #
#######################################
trees<-tbl(con_suppl, "trees") %>% 
  filter(tree=="tree") %>% 
  collect() %>% 
  mutate (grid = as.factor(grid),
          tree = as.factor(tree))

plot(y~x, data=trees, pch=19, cex=0.05, col="green", xlab="UTM easting", ylab="UTM northing")
points(cord.UTM$coords.x1, cord.UTM$coords.x2, pch=19, cex=0.2)

