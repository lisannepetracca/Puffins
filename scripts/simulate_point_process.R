library(sf)
library(ggplot2)
pt.df   <- data.frame(pt = 1, x = 100, y = 100)
pt.sf   <- st_as_sf(pt.df, coords = c("x", "y")) 
island <- st_buffer(pt.sf, dist = 50) #this is an area of 50 m radius
plot(island, col="white")

#create suitable buffer that is 40 m from perimeter
suitable_buffer <- st_buffer(island, dist = -40)

ggplot() +
  geom_sf(data = island, color = "black", fill = "white", size=2) +
  geom_sf(data=suitable_buffer, color = "darkgreen", size=1)#+

#calculate the difference of these two polygons to get the suitable habitat
suitable_habitat <- st_difference(island, suitable_buffer)
plot(suitable_habitat[1], col="lightgreen")

#let's get regular points spaced apart by 5 m using package terra
library(terra)

#create raster w desired res
rast <- rast(suitable_habitat, resolution = c(5,5))

# Fake values, for demo only
values(rast) <- 1:ncell(rast)

#convert to points, mask to habitat
points <- as.points(rast, na.rm = TRUE)
sf_points <- sf::st_as_sf(points)
sf_points <- st_intersection(sf_points, suitable_habitat)

ggplot() +
  geom_sf(data = suitable_habitat, color = "black", fill = "white", size=1) +
  geom_sf(data=th, color = "darkgreen", size=1)+
  geom_sf(data=sf_points, color="purple")

#let's build 2.5-m plots across island
plots <- st_buffer(sf_points, dist = 2.5)
#let's only include those completely within polygon
plots <- st_join(plots, suitable_habitat, join = st_within, left=FALSE)

ggplot() +
  geom_sf(data = suitable_habitat, color = "black", fill = "white", size=1) +
  geom_sf(data=plots, color="purple")

#https://keen-swartz-3146c4.netlify.app/pointpatterns.html#spatial-sampling-and-simulating-a-point-process
#kappa represents the intensity of the poisson process of cluster centers
#mu is mean number of points per cluster
#scale is standard deviation of random displacement (along each coordinate axis) of a point from the cluster center
kappa = 2000 / st_area(suitable_habitat) # intensity
kappa2 = (2000/4) / st_area(suitable_habitat) # intensity

th = st_sample(suitable_habitat, kappa = kappa, mu = 1.1, scale = 1, type = "Thomas") 
th2 = st_sample(suitable_habitat, kappa = kappa2, mu = 4, scale = 1, type = "Thomas") 
#?rThomas
#The help function obtained by ?rThomas details the meaning of the parameters kappa, mu and scale. 
#Simulating point processes means that the intensity is given, not the sample size. 
#The sample size within the observation window obtained this way is a random variable.

#?rThomas
#spdep package??

ggplot() +
  geom_sf(data = suitable_habitat, color = "black", fill = "white", size=1) +
  geom_sf(data=th, color = "darkgreen", size=1)
ggplot() +
  geom_sf(data = suitable_habitat, color = "black", fill = "white", size=1) +
  geom_sf(data=th2, color = "darkgreen", size=1)

#let's see how many burrows per plot
(plots$burrow_count <- lengths(st_intersects(plots, th)))
mean(plots$burrow_count)
sd(plots$burrow_count)
(plots$burrow_count <- lengths(st_intersects(plots, th2)))
mean(plots$burrow_count)
sd(plots$burrow_count)

#ok but now we have to exclude habitat that cannot be visited
?st_buffer
nogo_buffer <- st_buffer(island, dist = -20)
nogo_habitat <- st_difference(island, nogo_buffer)
survey_habitat <- st_difference(suitable_habitat, nogo_habitat)

ggplot() +
  geom_sf(data = island, color = "black", fill = "white", size=1) +
  geom_sf(data = suitable_habitat, color = "black", fill = "black", size=1) +
  geom_sf(data = survey_habitat, color = "black", fill = "grey", size=1)

#let's see what plots are left
plots <- st_join(plots, survey_habitat, join = st_within, left=FALSE)
ggplot() +
  geom_sf(data = island, color = "black", fill = "white", size=1) +
  geom_sf(data = suitable_habitat, color = "black", fill = "black", size=1) +
  geom_sf(data = survey_habitat, color = "black", fill = "grey", size=1)+
  geom_sf(data=plots, color = "blue", size=1)+
  geom_sf(data=th2, color = "purple", size=1)
  
#need to assign occupancy to all of these burrows -- should use real data, but let's use 0.75 for now

#true density of burrows in suitable habitat is
nrow(th)/st_area(suitable_habitat) #0.323726/m2

#and let's see true number burrows in plots
(plots$burrow_count <- lengths(st_intersects(plots, th)))
mean(plots$burrow_count)
sd(plots$burrow_count)

prop <- c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
nSamples <- round(72*prop) 

my.samples <- list()
mean <- sd <- matrix(nrow=6,ncol=100)
temp <- list()
nsim <- 100

for(j in 1:nsim){
  for(i in 1:length(nSamples)){
      temp[[j]] <- sample(c(1:nrow(plots)),nSamples[i])  
      mean[i,j] <- mean(lengths(st_intersects(plots[temp[[j]],], th)))
      sd[i,j] <- sd(lengths(st_intersects(plots[temp[[j]],], th)))  
  }}

mean <- as.data.frame(mean)
sd <- as.data.frame(sd)

mean$prop <- c("10", "25", "50", "75", "90", "100")
sd$prop <- c("10", "25", "50", "75", "90", "100")

library(tidyverse)
means <-   mean %>% pivot_longer(
  cols = starts_with("V"),
  values_to = "mean",
  values_drop_na = TRUE
)
sds <-   sd %>% pivot_longer(
  cols = starts_with("V"),
  values_to = "sd",
  values_drop_na = TRUE
)

means$sd <- sds$sd
means$prop <- factor(means$prop, levels = c("10", "25", "50", "75", "90", "100"))

library(ggplot2)

ggplot(means, aes(x=prop, y=mean)) +
  geom_boxplot()+
  # geom_bar(position=position_dodge(), stat="identity",
  #          colour='black') +
  # geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2)+
  geom_hline(yintercept=5.319444, linetype="dashed", 
               color = "red", size=1)+
  # geom_hline(yintercept=5.319444+2.968503, linetype="dashed",
  #            color = "red", size=1)+
  # geom_hline(yintercept=5.319444-2.968503, linetype="dashed",
  #            color = "red", size=1)+
  scale_x_discrete(breaks=c("10","25","50","75","90","100"),
                   labels=c("10%", "25%", "50%", "75%", "90%", "100%"))+
    xlab("Proportion of plots surveyed")

#and let's see true number burrows in plots
(plots$burrow_count <- lengths(st_intersects(plots, th2)))
mean(plots$burrow_count)
sd(plots$burrow_count)

prop <- c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
nSamples <- round(72*prop) 

my.samples <- list()
mean <- sd <- matrix(nrow=6,ncol=100)
temp <- list()
nsim <- 100

for(j in 1:nsim){
  for(i in 1:length(nSamples)){
    temp[[j]] <- sample(c(1:nrow(plots)),nSamples[i])  
    mean[i,j] <- mean(lengths(st_intersects(plots[temp[[j]],], th2)))
    sd[i,j] <- sd(lengths(st_intersects(plots[temp[[j]],], th2)))  
  }}

mean <- as.data.frame(mean)
sd <- as.data.frame(sd)

mean$prop <- c("10", "25", "50", "75", "90", "100")
sd$prop <- c("10", "25", "50", "75", "90", "100")

library(tidyverse)
means <-   mean %>% pivot_longer(
  cols = starts_with("V"),
  values_to = "mean",
  values_drop_na = TRUE
)
sds <-   sd %>% pivot_longer(
  cols = starts_with("V"),
  values_to = "sd",
  values_drop_na = TRUE
)

means$sd <- sds$sd
means$prop <- factor(means$prop, levels = c("10", "25", "50", "75", "90", "100"))

library(ggplot2)

ggplot(means, aes(x=prop, y=mean)) +
  geom_boxplot()+
  # geom_bar(position=position_dodge(), stat="identity",
  #          colour='black') +
  # geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2)+
  geom_hline(yintercept=4.791667, linetype="dashed", 
             color = "red", size=1)+
  # geom_hline(yintercept=5.319444+2.968503, linetype="dashed",
  #            color = "red", size=1)+
  # geom_hline(yintercept=5.319444-2.968503, linetype="dashed",
  #            color = "red", size=1)+
  scale_x_discrete(breaks=c("10","25","50","75","90","100"),
                   labels=c("10%", "25%", "50%", "75%", "90%", "100%"))+
  xlab("Proportion of plots surveyed")

#notes at end of day friday
#need to repeat this like 100x to get at variation in plots surveyed; mean and SE rather than SD

#ok so i did the repeat 100x and have boxplots getting at mean for more homogeneous and more clustered
#how to keep number of burrows EXACTLY the same while changing clusteredness



#DISCARDED
N <- 7
r <- 5
theta <- 80
x <- y <- vector()

for(n in 1:N){
  x[n] = r * cos(2*pi*n/N + theta) #+ x_centre
  y[n] = r * sin(2*pi*n/N + theta) #+ y_centre
}
plot(x, y)
df <- as.data.frame(cbind(x,y))
library(sf)
library(tidyverse)
polygon <- df %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
plot(polygon)
