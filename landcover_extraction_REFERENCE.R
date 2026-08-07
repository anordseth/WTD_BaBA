# Load required libraries
library(sf)
library(terra)
library(exactextractr)
library(dplyr)

#use for guidance
#https://mbjoseph.github.io/posts/2018-12-27-categorical-spatial-data-extraction-around-buffered-points-in-r/
  

#lets use siu since it is smaller
#can skip
xy<- st_as_sf(Dsiuc_BaBA_502, coords = c("easting", "northing"), crs=32616)
class(xy)
str(xy)
crs(xy)

getwd()
setwd("F:/gbr_postdoc/BaBA_paper")

DSIUC <- as.data.frame(Dsiuc_BaBA_50) #to use to cbind later
#DSIUC <- as.data.frame(Dsiuc_BaBA_502) #to use to cbind later

buffer_points <- st_buffer(xy,dist =  100)
# Creates a buffer of50units around each point
st_write(buffer_points, "buffer_points_SIU.shp")
crs(buffer_points)
buffer_points <- st_read("buffer_points_SIU.shp")

wd <-setwd("F:/gbr_postdoc/GIS layers/rasters/nlcd_2023_sm")
dir()
cover<-raster("NLCD_2023_sm.tif")# 
crs(cover)


#example~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#get the midpoint of the df
midpoint <- data.frame(lat = mean(Dsiuc_BaBA_502$northing), 
                       lon = mean(Dsiuc_BaBA_502$easting))

midpoint<- st_as_sf(midpoint, coords = c("lon", "lat"), crs=32616)
crs(midpoint)
#midpoint <- spTransform(midpoint, projection(cover))

buffer_distance_meters<-500
landcover <- extract(cover, midpoint, buffer = buffer_distance_meters)

landcover_proportions <- lapply(landcover, function(x) {
  counts_x <- table(x)
  proportions_x <- prop.table(counts_x)
  sort(proportions_x)
})
sort(unlist(landcover_proportions))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##cover categories 
#11 - open water
#41 - forest
#21 - developed opened space
#22 - developed low med high
#52 - scrub
#71 -grassland
#81- crops and pasture
#90 wetlands
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
landcover_proportions3[[2]]
landcover3[[2]]
#> sort(unlist(landcover_proportions))
#
#11        41        21        22 
#0.1120815 0.1193595 0.1950509 0.5735080

#this worked well for the one midpoint of everything but we need it to be all buffers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
buffer_distance_meters<-100 #100m
landcover2 <- extract(cover, xy, buffer = buffer_distance_meters, include_cols = "burstID")
#this is faster for some reason

landcover3[[1]]
landcover3 <- extract(nlcd_modified, buffer_points, include_cols = "burstID")
#3 did not change it...

#make a loop and turn it into factors
#coauthor John Vinson helped with the below code
covercat<- c(11,41,21,22 ,52 ,71,81,90, 100)

landcover_proportions3 <- lapply(landcover3, function(x) {
  #x.factor<- as.factor(x)
  x.factor = factor(x, levels = covercat)
  #levels(x.factor)<- covercat
  counts_x <- table(x.factor)
  proportions_x <- as.data.frame(prop.table(counts_x))$Freq
  names(proportions_x) <- covercat
  return(proportions_x)#need to do this when doing lapply
})

landcover3[[3]]#check outputs
landcover_proportions3[[3]]

prop_by_burst<- NULL #empty df
for(i in 1:length(landcover_proportions3)) {
  prop_by_burst<-rbind(prop_by_burst, landcover_proportions3[[i]])
  
}#should create df that goes by each row is different encounter

prop_by_burst <-as.data.frame(prop_by_burst)
names(prop_by_burst)


prop_burst4<- cbind(DSIUC, prop_by_burst) #lulc have capital X
#prop_burst2<- cbind(xy, prop_by_burst) #lulc have capital X
#names(prop_burst4)[11:19]<-covercat #remove the X in front of the LULC type
  
#sort(unlist(landcover_proportions3)) #makes vectors
write.csv(prop_burst4, "prop_landcover_SIUroadfixed.csv")
getwd()
write_rds(prop_burst4, "prop_landcover_SIUroadfixed.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Replace NLCD values where roads exist 
#I believe this worked but I used ArcPro to do it for the whole dataset

# Load NLCD raster
nlcd<-cover

# Load road shapefile (vector layer)
roads <- shp_roadSouth

# Reproject roads to match NLCD raster if needed
if (!compareCRS(cover, roads)) {
  roads <- project(roads, crs(cover))
}

# Rasterize road layer to match NLCD resolution and extent
road_raster <- rasterize(roads, nlcd, field=1, background=NA)

# Replace NLCD values where roads exist
nlcd_modified <- nlcd
nlcd_modified[!is.na(road_raster)] <- 100

setwd("F:/gbr_postdoc/GIS layers/rasters")
# Save the modified raster
writeRaster(nlcd_modified, "nlcd_modified.tif", overwrite=TRUE)
##this did work but is only for south roads

