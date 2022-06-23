library(here)
library(ggplot2)
library(sf)
library(tidyverse)
library(sf)
library(ggmap)
library(MetBrewer)
library(RColorBrewer)

getwd()

#reading in data from WA and AK
data_AK <- read.csv("data/AK/AMNWR_tufted_puffin_colonies.csv", header=T)
data_WA1 <- read.csv("data/WA/S_Krock/QRY_export_for_UW_20220601.csv", header=T)
data_WA2 <- read.csv("data/WA/J_Brusa/TUPUPelagic.csv", header=T)
data_WA3 <- read.csv("data/WA/J_Brusa/TUPUSalishSea.csv", header=T) #no coordinates
data_WA4 <- read.csv("data/WA/J_Brusa/TUPUZone2No2013.csv", header=T)
data_WA5 <- read.csv("data/WA/S_Pearson/TUPU_Master_2016_15March2017.csv", header=T)

#reading in regional datasets
NPSDP <- read.csv("data/N_Pacific_Seabird_Data_Portal/seabird_data_download/status_record_download.csv", header=T)
head(NPSDP)
unique(NPSDP$country)
NPSDP <- NPSDP %>% filter(country=="United States" | country=="Canada")

#getting coordinate column names
head(data_AK)
head(data_WA1)
head(data_WA2)
head(data_WA3) #no coordinates in Salish Sea dataset
head(data_WA4) #no coordinates in TUPU Zone 2 dataset for "all years"; coords wo 2013 only
head(data_WA5)
head(NPSDP)

#renaming column names for the merge 
#also getting new column for data type
newColumnName <- "source"

data_AK <- data_AK %>% rename(long = location_midpoint_lng) %>% rename(lat = location_midpoint_lat) %>%  mutate(!!newColumnName := "TUPU AK Colony Register")
data_WA1 <- data_WA1 %>% rename(long = LongDecDeg) %>% rename(lat = LatDecDeg ) %>%  mutate(!!newColumnName := "All WA Colony Register")
data_WA2 <- data_WA2 %>% rename(long = Longitude) %>% rename(lat = Latitude ) %>%  mutate(!!newColumnName := "All Salish Sea At-Sea")
data_WA4 <- data_WA4 %>% rename(long = Longitude) %>% rename(lat = Latitude ) %>%  mutate(!!newColumnName := "All Zone 2 Nearshore")
data_WA5 <- data_WA5 %>% rename(long = Long) %>% rename(lat = Lat ) %>%  mutate(!!newColumnName := "All WDFW At-Sea")
NPSDP <- NPSDP %>% rename(long = location_midpoint_lng) %>% rename(lat = location_midpoint_lat ) %>%  mutate(!!newColumnName := "N Pacific Seabird Data Portal")

#get rid of rows without locations in two datasets
data_WA4 <- data_WA4 %>% filter(!is.na(long)) %>% filter(!is.na(lat))
data_WA5 <- data_WA5 %>% filter(!is.na(long)) %>% filter(!is.na(lat))

#let's make versions where there are TUPU records
head(data_AK) #all are TUPU
head(data_WA1)
head(data_WA2)
unique(data_WA2$Species)
head(data_WA4) #no coordinates in TUPU Zone 2 dataset for "all years"; coords wo 2013 only
unique(data_WA4$Species)
head(data_WA5)
head(NPSDP)

data_WA1_TUPU <- data_WA1 %>% filter(SPECIES_CO=="TUPU") %>%  mutate(!!newColumnName := "TUPU WA Colony Register")
data_WA2_TUPU <- data_WA2 %>% filter(Species=="TUPU") %>%  mutate(!!newColumnName := "TUPU Salish Sea At-Sea")
data_WA4_TUPU <- data_WA4 %>% filter(Species=="TUPU") %>%  mutate(!!newColumnName := "TUPU Zone 2 Nearshore")
data_WA5_TUPU <- data_WA1 %>% filter(SPECIES_CO=="TUPU") %>%  mutate(!!newColumnName := "TUPU WDFW At-Sea")
NPSDP_TUPU <- NPSDP %>% filter(species_common_name=="Tufted Puffin") %>%  mutate(!!newColumnName := "TUPU N Pacific Seabird Data Portal")

#let's merge these datasets together
df_list <- list(data_AK, 
                data_WA1, data_WA1_TUPU,
                data_WA2, data_WA2_TUPU,
                data_WA4, data_WA4_TUPU,
                data_WA5, data_WA5_TUPU)      

#merge all data frames together
data <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)  
head(data)

data$lat <- as.numeric(data$lat)
data <- data %>% filter(!is.na(data$lat))

#let's transfer to an sf object and assign a coordinate system
TUPU <- st_as_sf(data, coords = 
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

colors <- met.brewer("Java",n=9,type="continuous")
ggmap(myMap) +
  geom_sf(data = TUPU_proj, aes(color=as.factor(source)), size=1.5, inherit.aes=F)+
  scale_color_manual(values = colors, name = "Data source") + 
  coord_sf(crs = st_crs(3857))+
  ggtitle("Puffin Data Sources in AK and WA")
ggsave("data/figures/AK_and_WA.jpg")

####---- LETS DO A MAP WITH NPSDP ONLY ####

#let's merge these datasets together
df_list2 <- list(NPSDP, NPSDP_TUPU)      

#merge all data frames together
data2 <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list2)  

data2$lat <- as.numeric(data2$lat)
data2 <- data2 %>% filter(!is.na(data2$lat))

#let's transfer to an sf object and assign a coordinate system
NPSDP_TUPU <- st_as_sf(data2, coords = 
                         c("long", "lat"), crs = 4326)

#these need to be transformed to display properly w google basemap (weird)
NPSDP_TUPU_proj <- st_transform(NPSDP_TUPU, crs =3857)

#lets map
colors <- c("darkmagenta", "green3")
ggmap(myMap) +
  geom_sf(data = NPSDP_TUPU_proj, aes(color=as.factor(source)), size=1.5, inherit.aes=F)+
  scale_color_manual(values = colors, name = "",
                     labels=c("all data", "TUPU only")) + 
  coord_sf(crs = st_crs(3857))+
  ggtitle("North Pacific Seabird Data Portal")#+
#theme(legend.position="none")
ggsave("data/figures/NPSDP.jpg")


####---- LETS DO A WA MAP WITH SOURCES ####

#get rid of AK for WA map
TUPU_proj_noAK <- TUPU_proj %>% filter(source != "TUPU AK Colony Register")

#get bounding box **for WA only** in WGS84
WA_area <- c(left = -126.5, bottom = 46, right = -122, top = 49.5)

#get stamen map for bounding box
basemap <- get_stamenmap(WA_area, zoom = 7, maptype = "terrain") 

#using the function
myMap <- ggmap_bbox(basemap) 

ggmap(myMap) +
  geom_sf(data = TUPU_proj_noAK, aes(color=as.factor(source)), size=1.5, inherit.aes=F)+
  scale_fill_brewer(palette = "Set1", name="Data source")+
  coord_sf(crs = st_crs(3857))+
  ggtitle("Puffin Data Sources for WA")
ggsave("data/figures/WA.jpg")






