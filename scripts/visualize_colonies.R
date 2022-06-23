library(here)
library(ggplot2)
library(sf)
library(tidyverse)
library(sf)
library(ggmap)

#reading in colony data from WA and AK
data_AK <- read.csv("data/AK/AMNWR_tufted_puffin_colonies.csv", header=T)
data_WA1 <- read.csv("data/WA/S_Krock/QRY_export_for_UW_20220601.csv", header=T)
data_WA2 <- read.csv("data/WA/J_Brusa/TUPUPelagic.csv", header=T)
data_WA3 <- read.csv("data/WA/J_Brusa/TUPUSalishSea.csv", header=T)
data_WA4 <- read.csv("data/WA/J_Brusa/TUPUZone2No2013.csv", header=T)
data_WA5 <- read.csv("data/WA/S_Pearson/TUPU_Master_2016_15March2017.csv", header=T)

#getting coordinate column names
head(data_AK)
head(data_WA1)
head(data_WA2)
head(data_WA3) #no coordinates in Salish Sea dataset
head(data_WA4) #no coordinates in TUPU Zone 2 dataset for "all years"; coords wo 2013 only
head(data_WA5)

#renaming column names for the merge 
#also getting new column for data type
newColumnName <- "source"

data_AK <- data_AK %>% rename(long = location_midpoint_lng) %>% rename(lat = location_midpoint_lat) %>%  mutate(!!newColumnName := "AK Colony Database")
data_WA1 <- data_WA1 %>% rename(long = LongDecDeg) %>% rename(lat = LatDecDeg ) %>%  mutate(!!newColumnName := "WA Colony Database")
data_WA2 <- data_WA2 %>% rename(long = Longitude) %>% rename(lat = Latitude ) %>%  mutate(!!newColumnName := "Salish Sea At-Sea")
data_WA4 <- data_WA4 %>% rename(long = Longitude) %>% rename(lat = Latitude ) %>%  mutate(!!newColumnName := "Zone 2 At-Sea")
data_WA5 <- data_WA5 %>% rename(long = Long) %>% rename(lat = Lat ) %>%  mutate(!!newColumnName := "WDFW At-Sea")

#get rid of rows without locations in two datasets
data_WA4 <- data_WA4 %>% filter(!is.na(long)) %>% filter(!is.na(lat))
data_WA5 <- data_WA5 %>% filter(!is.na(long)) %>% filter(!is.na(lat))

#let's merge these datasets together

df_list <- list(data_AK, data_WA1, data_WA2, data_WA4, data_WA5)      

#merge all data frames together
test <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)  
head(test)

sum(is.na(test$long))
sum(is.na(test$lat))

test$lat = as.numeric(test$lat)
test <- test %>% filter(!is.na(lat)) 

#let's transfer to an sf object and assign a coordinate system

TUPU <- st_as_sf(test, coords = 
                         c("long", "lat"), crs = 4326)

#these need to be transformed to display properly w google basemap (weird)
TUPU_proj <- st_transform(TUPU, crs =3857)

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

head(TUPU_proj)
#plotting the colonies

colors <- c("darkblue", "purple", "black", "orange", "red")
ggmap(myMap) +
  geom_sf(data = TUPU_proj, aes(color=as.factor(source)), size=1.5, inherit.aes=F)+
  scale_color_manual(values = colors, name = "Data source") + 
  coord_sf(crs = st_crs(3857))+
  ggtitle("Puffin Data Sources in AK and WA")
ggsave("data/figures/AK_and_WA.jpg")


#get rid of AK for WA map
TUPU_proj_noAK <- TUPU_proj %>% filter(source != "AK Colony Database")

#get bounding box **for WA only** in WGS84
WA_area <- c(left = -126.5, bottom = 46, right = -122, top = 49.5)

#get stamen map for bounding box
basemap <- get_stamenmap(WA_area, zoom = 7, maptype = "terrain") 

#using the function
myMap <- ggmap_bbox(basemap) 

colors <- c("purple", "black", "orange", "red")
ggmap(myMap) +
  geom_sf(data = TUPU_proj_noAK, aes(color=as.factor(source)), size=1.5, inherit.aes=F)+
  scale_color_manual(values = colors, name = "Data source") + 
  coord_sf(crs = st_crs(3857))+
  ggtitle("Puffin Data Sources for WA")
ggsave("data/figures/WA.jpg")
