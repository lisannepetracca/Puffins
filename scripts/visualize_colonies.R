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
data_OR1 <- read.csv("data/OR/S_Stephensen/OR_SpeciesSurvey.csv", header=T)
data_OR2 <- read.csv("data/OR/S_Stephensen/OR_ColonyLocations.csv", header=T)

#reading in regional datasets
NPSDP <- read.csv("data/N_Pacific_Seabird_Data_Portal/seabird_data_download/status_record_download.csv", header=T)
head(NPSDP)
unique(NPSDP$country)
NPSDP <- NPSDP %>% filter(country=="United States" | country=="Canada")

#getting coordinate column names
head(data_AK)
tail(data_AK)
head(data_WA1)
head(data_WA2)
head(data_WA3) #no coordinates in Salish Sea dataset
head(data_WA4) #no coordinates in TUPU Zone 2 dataset for "all years"; coords wo 2013 only
head(data_WA5)
head(data_OR1)
head(data_OR2)
head(NPSDP)

#renaming column names for the merge 
#also getting new column for data type
newColumnName <- "source"

#need to combine Oregon data into 1
data_OR1 <- data_OR1 %>% rename(survey_year = intSurveyYear) %>% 
  mutate(!!newColumnName := "All OR Colony Register")
data_OR2 <- data_OR2 %>% rename(long = dblLongitude) %>% rename(lat = dblLatitude) 
data_OR1 <- left_join(data_OR1, data_OR2, by="strColonyCode")
data_OR1 <- data_OR1 %>% rename(site_name = strColonyCode)

#now we rename columns from the others
data_AK <- data_AK %>% rename(long = location_midpoint_lng) %>% rename(lat = location_midpoint_lat) %>%  
  rename(site_name = location_index) %>% rename(survey_year = survey_end_year) %>% 
  mutate(!!newColumnName := "TUPU AK Colony Register")
data_WA1 <- data_WA1 %>% rename(long = LongDecDeg) %>% rename(lat = LatDecDeg ) %>%  
  rename(site_name = SITE_NAME) %>% rename(survey_year = YEAR_) %>% 
  mutate(!!newColumnName := "All WA Colony Register")
data_WA2 <- data_WA2 %>% rename(long = Longitude) %>% rename(lat = Latitude ) %>%  
  rename(survey_year = YYYY) %>% 
  mutate(!!newColumnName := "All Salish Sea At-Sea")
data_WA4 <- data_WA4 %>% rename(long = Longitude) %>% rename(lat = Latitude ) %>%  
  rename(survey_year = YYYY) %>% 
  mutate(!!newColumnName := "All Zone 2 Nearshore")
data_WA5 <- data_WA5 %>% rename(long = Long) %>% rename(lat = Lat ) %>%  
  rename(site_name = Island) %>% rename(survey_year = Year) %>% 
  mutate(!!newColumnName := "All WDFW At-Sea")
NPSDP <- NPSDP %>% rename(long = location_midpoint_lng) %>% rename(lat = location_midpoint_lat ) %>%  
  rename(site_name = location_id) %>% rename(survey_year = survey_end_year) %>% 
  mutate(!!newColumnName := "All N Pacific Seabird Data Portal")

#get rid of rows without locations in two datasets
data_WA4 <- data_WA4 %>% filter(!is.na(long)) %>% filter(!is.na(lat))
data_WA5 <- data_WA5 %>% filter(!is.na(long)) %>% filter(!is.na(lat))

#let's make versions where there are TUPU records
head(data_AK) #all are TUPU
unique(data_AK$location_name)
unique(data_AK$location_index)
head(data_WA1)
unique(data_WA1$SITE_NAME)
unique(data_WA1$SITE_NUMBE)
head(data_WA2)
unique(data_WA2$Species)
head(data_WA4) #no coordinates in TUPU Zone 2 dataset for "all years"; coords wo 2013 only
unique(data_WA4$Species)
head(data_WA5)
unique(data_WA5$Island)
head(data_OR1)
head(data_OR2)
head(NPSDP)

data_WA1_TUPU <- data_WA1 %>% filter(SPECIES_CO=="TUPU") %>%  mutate(!!newColumnName := "TUPU WA Colony Register")
data_WA2_TUPU <- data_WA2 %>% filter(Species=="TUPU") %>%  mutate(!!newColumnName := "TUPU Salish Sea At-Sea")
data_WA4_TUPU <- data_WA4 %>% filter(Species=="TUPU") %>%  mutate(!!newColumnName := "TUPU Zone 2 Nearshore")
data_WA5_TUPU <- data_WA1 %>% filter(SPECIES_CO=="TUPU") %>%  mutate(!!newColumnName := "TUPU WDFW At-Sea")
NPSDP_TUPU <- NPSDP %>% filter(species_common_name=="Tufted Puffin") %>%  mutate(!!newColumnName := "TUPU N Pacific Seabird Data Portal")
NPSDP_AK <- NPSDP %>% filter(state_province == "Alaska") %>% mutate(!!newColumnName := "All AK Colony Register")
data_OR1_TUPU <- data_OR1 %>% filter(strAOUCode =="TUPU") %>% mutate(!!newColumnName := "TUPU OR Colony Register")

#let's merge these datasets together
df_list <- list(NPSDP_AK, data_AK, 
                data_WA1, data_WA1_TUPU,
                data_WA2, data_WA2_TUPU,
                data_WA4, data_WA4_TUPU,
                data_WA5, data_WA5_TUPU,
                data_OR1, data_OR1_TUPU)      

#merge all data frames together
data <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)  
head(data)

data$lat <- as.numeric(data$lat)
data <- data %>% filter(!is.na(data$lat))

####---- LET'S DO A MAP WITH AK, OR, and WA ----####

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

colors <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99')
ggmap(myMap) +
  geom_sf(data = TUPU_proj, aes(color=as.factor(source)), size=1.5, inherit.aes=F)+
  scale_color_manual(values = colors, name = "Data source") + 
  coord_sf(crs = st_crs(3857))+
  ggtitle("Puffin Data Sources in AK, WA, and OR")
ggsave("data/figures/AK_and_WA_and_OR.jpg")

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
TUPU_proj_noAK <- TUPU_proj %>% filter(source != "TUPU AK Colony Register" &
                                        source != "TUPU OR Colony Register" &
                                        source != "All OR Colony Register")
unique(TUPU_proj_noAK$source)

#get bounding box **for WA only** in WGS84
WA_area <- c(left = -126.5, bottom = 46, right = -122, top = 49.5)

#get stamen map for bounding box
basemap <- get_stamenmap(WA_area, zoom = 7, maptype = "terrain") 

#using the function
myMap <- ggmap_bbox(basemap) 

colors <- c('#1f78b4','#33a02c','#fb9a99','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99')
ggmap(myMap) +
  geom_sf(data = TUPU_proj_noAK, aes(color=as.factor(source)), size=1.5, inherit.aes=F)+
  scale_color_manual(values = colors, name = "Data source") + 
  coord_sf(crs = st_crs(3857))+
  ggtitle("Puffin Data Sources for WA")
ggsave("data/figures/WA.jpg")

####---- LETS DO AN OR + WA MAP WITH SOURCES ####

#get rid of AK for WA map
TUPU_proj_noAK <- TUPU_proj %>% filter(source != "TUPU AK Colony Register")

#get bounding box **for WA AND OR only** in WGS84
WA_area <- c(left = -126.5, bottom = 41.5, right = -122, top = 49.5)
plot(WA_area)
#get stamen map for bounding box
basemap <- get_stamenmap(WA_area, zoom = 7, maptype = "terrain") 

#using the function
myMap <- ggmap_bbox(basemap) 

colors <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99')
ggmap(myMap) +
  geom_sf(data = TUPU_proj_noAK, aes(color=as.factor(source)), size=1.5, inherit.aes=F)+
  scale_color_manual(values = colors, name = "Data source") + 
  coord_sf(crs = st_crs(3857))+
  ggtitle("Puffin Data Sources for WA and OR")
ggsave("data/figures/WA_and_OR.jpg")


####---- LETS GET FINAL YEAR THERE WAS PUFFIN DATA ####

#using "data" created around L. 113

head(data)

#this will select cols of interest
data_simple <- data %>% dplyr::select(long, lat, survey_year, site_name, source) 

#this will subset to those w TUPU sightings
data_simple <- data_simple[!grepl("All", data_simple$source),]
unique(data_simple$source)

#get rid of those without survey year
data_simple <- data_simple %>% filter(!is.na(survey_year))

#need to group by last year for the colony sites; at-sea can remain as is
data_atsea <- data_simple[grepl("At-Sea|Nearshore", data_simple$source),]
data_colony <-  data_simple[!grepl("At-Sea|Nearshore", data_simple$source),]
data_colony <- data_colony %>% group_by(site_name) %>% slice_max(survey_year)

data_simple2 <- rbind(data_colony, data_atsea)

data_simple2$year_cat <- cut(data_simple2$survey_year, 
                   breaks=c(-Inf, 1899, 1949, 1999, 2009, Inf), 
                   labels=c("pre-1900","pre-1950","1950-2000", "2001-2010", "2011-present"))

#let's transfer to an sf object and assign a coordinate system
TUPU_year <- st_as_sf(data_simple2, coords = 
                   c("long", "lat"), crs = 4326)

#these need to be transformed to display properly w google basemap (weird)
TUPU_proj <- st_transform(TUPU_year, crs =3857)

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
unique(TUPU_proj$year_cat)
#plotting the colonies

colors <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')
ggmap(myMap) +
  geom_sf(data = TUPU_proj, aes(color=as.factor(year_cat)), size=1.5, inherit.aes=F)+
  scale_color_manual(values = colors, name = "Last year of TUPU record") + 
  coord_sf(crs = st_crs(3857))#+
  #ggtitle("Puffin Data Sources in AK, WA, and OR")
ggsave("data/a_figures/LastYr_AK_and_WA_and_OR.jpg")

#subset to OR and WA
#get bounding box **for WA AND OR only** in WGS84
WA_area <- c(left = -126.5, bottom = 41.5, right = -122, top = 49.5)
plot(WA_area)
#get stamen map for bounding box
basemap <- get_stamenmap(WA_area, zoom = 7, maptype = "terrain") 

#using the function
myMap <- ggmap_bbox(basemap) 

colors <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')
ggmap(myMap) +
  geom_sf(data = TUPU_proj, aes(color=as.factor(year_cat)), size=1.5, inherit.aes=F)+
  scale_color_manual(values = colors, name = "Last year of TUPU record") + 
  coord_sf(crs = st_crs(3857))#+
  #ggtitle("Puffin Data Sources in AK, WA, and OR")
ggsave("data/a_figures/LastYr_WA_and_OR.jpg")

####---- LET'S MAKE 5 x 5 GRID FOR TUPU COLONIES ONLY ----####

#using data_colony on L. 258

#let's transfer to an sf object and assign a coordinate system
TUPU_colony <- st_as_sf(data_colony, coords = 
                        c("long", "lat"), crs = 4326)

TUPU_colony$year_cat <- cut(TUPU_colony$survey_year, 
                             breaks=c(-Inf, 1899, 1949, 1999, 2009, Inf), 
                             labels=c("pre-1900","pre-1950","1950-2000", "2001-2010", "2011-present"))

#these need to be transformed to display properly w google basemap (weird)
TUPU_proj <- st_transform(TUPU_colony, crs =3857)

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

colors <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')
ggmap(myMap) +
  geom_sf(data = TUPU_proj, aes(color=as.factor(year_cat)), size=1.5, inherit.aes=F)+
  scale_color_manual(values = colors, name = "Last year of TUPU record") + 
  coord_sf(crs = st_crs(3857))#+
#ggtitle("Puffin Data Sources in AK, WA, and OR")
ggsave("data/a_figures/LastYr_ColoniesOnly_AK_and_WA_and_OR.jpg")

#theyre already in meters, which is awesome (3857)
#3857 is Pseudo-Mercator (Web Mercator; Spherical Mercator) [Google Maps, etc.]

#ok doesn't work though bc there are some islands that technically go over the 180 deg longitude line
#will alaska albers work?
TUPU_proj <- st_transform(TUPU_proj, crs =3338)

#let's make grid over those colonies
#what if we wanted to do hexagons instead?
TUPU_grid_1600km2 <- st_make_grid(
  TUPU_proj,
  cellsize = 40000, #this makes grids of 1600 km2
  crs = 3338,
  what = "polygons",
  square = TRUE)

extent <- st_bbox(TUPU_grid_1600km2)

# library(rnaturalearth)
# test <- ne_download(scale=50, type="states", category="cultural",returnclass="sf")
# states <- test %>% filter(name_en=="Alaska" | name_en=="Oregon" |
#                             name_en=="Washington" | name_en=="British Columbia")
# states <- st_transform(states, crs =3338)

ggplot() +
  geom_sf(data=states, fill=NA, color="black", size=1)+
  geom_sf(data = TUPU_proj, color="blue", size=1.5, inherit.aes=F)+
  geom_sf(data=TUPU_grid_1600km2, fill=NA, color = "darkgrey", size=0.25)+
  coord_sf(crs=3338, xlim=c(extent[[1]], extent[[3]]), ylim=c(extent[[2]], extent[[4]]))
  
ggsave("data/a_figures/ColoniesOnly_w1600km2Grid.jpg")

library(gstat)
library(rasterize)
library(raster)

r <- raster(ncol=200, nrow=100)
extent(r) <- extent(TUPU_proj)
rp <- rasterize(TUPU_proj, r, 1)
plot(rp)

#rs is a raster with 1s for where there are colonies

#http://santiago.begueria.es/2010/10/generating-spatially-correlated-random-fields-with-r/
test <- as.data.frame(rp, row.names=NULL, optional=FALSE,
              xy=TRUE, na.rm=T)
head(test)

# define the gstat object (spatial model)
g.dummy <- gstat(formula=z~1+y, locations=~x+y, dummy=T, beta=25, model=vgm(psill=10,model="Exp",range=15), nmax=20)
yy <- predict(g.dummy, newdata=test, nsim=4)

# show one realization
gridded(yy) = ~x+y
spplot(yy[1])

#would then use poly2nb and run dcar_normal in nimble?
#can i generate data from a dcar_normal process?

#simulating data from nimble
#https://r-nimble.org/nimbleExamples/simulation_from_model.html


