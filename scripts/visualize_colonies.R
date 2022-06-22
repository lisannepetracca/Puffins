library(here)
library(ggplot2)
library(sf)
library(tidyverse)
library(sf)
library(ggmap)

#reading in colony data from WA and AK
data_AK <- read.csv("data/AK/AMNWR_tufted_puffin_colonies.csv", header=T)
data_WA <- read.csv("data/WA/S_Krock/QRY_export_for_UW_20220601.csv", header=T)

head(data_AK)
head(data_WA)

#let's transfer to an sf object and assign a coordinate system
data_AK <- st_as_sf(data_AK, coords = 
                         c("location_midpoint_lng", "location_midpoint_lat"), crs = 4326)
data_WA <- st_as_sf(data_WA, coords = 
                      c("LongDecDeg", "LatDecDeg"), crs = 4326)

#these need to be transformed to display properly w google basemap (weird)
data_AK_proj <- st_transform(data_AK, crs =3857)
data_WA_proj <- st_transform(data_WA, crs =3857)

#get bounding box in WGS84
area <- c(left = -180, bottom = 41, right = -118, top = 73)

#get stamen map for bounding box
basemap <- get_stamenmap(area, zoom = 3, maptype = "terrain") 

#here is a function that will allow sf objects to plot properly on google basemaps
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), c("ymin", "xmin", "ymax", "xmax"))
  # Convert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

#using the function
myMap <- ggmap_bbox(basemap) 

#plotting the colonies
ggmap(myMap) +
  geom_sf(data = data_AK_proj, color = "darkblue", inherit.aes=FALSE) +
  geom_sf(data = data_WA_proj, color = "purple", inherit.aes=FALSE) +
  coord_sf(crs = st_crs(3857))+
  ggtitle("Puffin Colony Locations in WA and OR")
