library(sf)
library(ggplot2)

#creating single point w center 100, 100
pt.df   <- data.frame(pt = 1, x = 100, y = 100)
#convert to sf
pt.sf   <- st_as_sf(pt.df, coords = c("x", "y")) 
#create area of 50-m radius
island <- st_buffer(pt.sf, dist = 50)
plot(island, col="white")

#create buffer of "suitable" habitat that is 30 m from perimeter
suitable_buffer <- st_buffer(island, dist = -30)

ggplot() +
  geom_sf(data = island, color = "black", fill = "white", size=2) +
  geom_sf(data=suitable_buffer, color = "darkgreen", size=1)#+

#calculate the difference of these two polygons to get the suitable habitat
suitable_habitat <- st_difference(island, suitable_buffer)
plot(suitable_habitat[1], col="lightgreen")

#let's get regular points spaced apart by 5 m using package terra
library(terra)

#create raster w desired res
rast <- rast(suitable_habitat, resolution = c(5,5), crs=NA)

# Fake values, for demo only
values(rast) <- 1:ncell(rast)

#convert to points, mask to habitat
points <- as.points(rast, na.rm = TRUE)
sf_points <- sf::st_as_sf(points)
sf_points <- st_intersection(sf_points, suitable_habitat)

ggplot() +
  geom_sf(data = suitable_habitat, color = "black", fill = "white", size=1) +
  #geom_sf(data=th, color = "darkgreen", size=1)+
  geom_sf(data=sf_points, color="purple")

#let's build 2.5-m plots across island
plots <- st_buffer(sf_points, dist = 2.5)
#let's only include those completely within polygon
plots <- st_join(plots, suitable_habitat, join = st_within, left=FALSE)

#let's see the suitable habitat w circular plots
ggplot() +
  geom_sf(data = suitable_habitat, color = "black", fill = "white", size=1) +
  geom_sf(data=plots, color="purple")

#https://keen-swartz-3146c4.netlify.app/pointpatterns.html#spatial-sampling-and-simulating-a-point-process
#kappa represents the intensity of the poisson process of cluster centers
#mu is mean number of points per cluster
#scale is standard deviation of random displacement (along each coordinate axis) of a point from the cluster center
#first one is homogeneous, second is not homogeneous

#a resource for spatstat
# http://spatstat.org/Melb2018/solutions/solution09.html


#goes from more homogeneous to more clustered
#real data says mean of 5.41 and sd of 3.72
kappa = 1825 / st_area(suitable_habitat) # intensity
kappa2 = (1825/3.6) / st_area(suitable_habitat) # intensity
#i don't think this one is realistic (max count of 28?)
#kappa3 = (2000/8) / st_area(suitable_habitat) # intensity

#setting seed and creating clustered point processes
#calls spatstat.random::rThomas

th <- list()
th2 <- list()

set.seed(23)
nsim <- 25
npp <- 25
for(i in 1:nsim){
th[[i]] = st_sample(suitable_habitat, kappa = kappa, mu = 1, scale = 1, type = "Thomas")
th2[[i]] = st_sample(suitable_habitat, kappa = kappa2, mu = 3.6, scale = 1, type = "Thomas")
}

#i don't think this one is realistic (max count of 28?)
#th3 = st_sample(suitable_habitat, kappa = kappa3, mu = 8.2, scale = 1, type = "Thomas") 

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

#let's get burrows per plot for MORE HOMOGENEOUS

burrow_count <- list()

for(i in 1:npp){
burrow_count[[i]] <- lengths(st_intersects(plots, th[[i]]))
}
burrows_all <- unlist(burrow_count)
mean(burrows_all) #5.41, this is mean including variance by process and sampling variance
sd(burrows_all) #2.906465
hist(burrows_all)

#let's get burrows per plot for MORE CLUSTERED

burrow_count <- list()

for(i in 1:npp){
  burrow_count[[i]] <- lengths(st_intersects(plots, th2[[i]]))
}
burrows_all <- unlist(burrow_count)
mean(burrows_all) #5.403889, this is mean including variance by process and sampling variance
sd(burrows_all) #4.02539

#ok but now we have to exclude habitat that cannot be visited
?st_buffer
nogo_buffer <- st_buffer(island, dist = -15)
nogo_habitat <- st_difference(island, nogo_buffer)
survey_habitat <- st_difference(suitable_habitat, nogo_habitat)

ggplot() +
  geom_sf(data = island, color = "black", fill = "white", size=1) +
  geom_sf(data = suitable_habitat, color = "black", fill = "black", size=1) +
  geom_sf(data = survey_habitat, color = "black", fill = "darkgreen", size=1)

#let's see what plots are in surveyable areas
#NOT CLUSTERED
plots_survey <- st_join(plots, survey_habitat, join = st_within, left=FALSE)
clust1 <- ggplot() +
  geom_sf(data = island, color = "black", fill = "white", size=1) +
  geom_sf(data = suitable_habitat, color = "white", fill = "darkgrey", size=1) +
  geom_sf(data=plots, color = "blue", fill=NA, size=1)+
  geom_sf(data=th[[1]], color = "yellow", size=0.5)

#CLUSTERED
#plots_survey <- st_join(plots, survey_habitat, join = st_within, left=FALSE)
clust2 <- ggplot() +
  geom_sf(data = island, color = "black", fill = "white", size=1) +
  geom_sf(data = suitable_habitat, color = "white", fill = "darkgrey", size=1) +
  geom_sf(data=plots, color = "blue", fill=NA, size=1)+
  geom_sf(data=th2[[1]], color = "yellow", size=0.5)

#let's see what plots are in surveyable vs non-surveyable areas
#NOT CLUSTERED
#plots_survey <- st_join(plots, survey_habitat, join = st_within, left=FALSE)
clust3 <- ggplot() +
  geom_sf(data = island, color = "black", fill = "white", size=1) +
  geom_sf(data = suitable_habitat, color = "black", fill = "darkgrey", size=1) +
  geom_sf(data = survey_habitat, color = "darkgrey", fill = "lightgrey", size=1)+
  geom_sf(data=plots_survey, color = "blue", fill=NA, size=1)+
  geom_sf(data=th[[1]], color = "yellow", size=0.5)

#CLUSTERED
#plots_survey <- st_join(plots, survey_habitat, join = st_within, left=FALSE)
clust4 <- ggplot() +
  geom_sf(data = island, color = "black", fill = "white", size=1) +
  geom_sf(data = suitable_habitat, color = "black", fill = "darkgrey", size=1) +
  geom_sf(data = survey_habitat, color = "darkgrey", fill = "lightgrey", size=1)+
  geom_sf(data=plots_survey, color = "blue", fill=NA, size=1)+
  geom_sf(data=th2[[1]], color = "yellow", size=0.5)
  
  
library(cowplot)
plot_grid(
  clust1, clust2, clust3, clust4,
  labels=c('A', 'B', 'C', 'D'),
  # labels = c('little cluster, all surveyable', 'more cluster, all surveyable', 
  #            'little cluster, 15-m not surveyable', 'more cluster, 15-m not surveyable'),
  align="hv"
)
ggsave("G:/My Drive/Puffins/Figures/BurrowDensity_CirclePlots_Overview.jpg")

#true density of burrows in suitable habitat is
#nrow(th)/st_area(suitable_habitat) #0.323726/m2

library(tidyverse)

#what proportion of circles are sampled?
prop <- c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
nSamples <- round(nrow(plots)*prop) 

my.samples <- list()
mean <- sd <- array(data=NA, dim=c(6,nsim,npp))
temp <- list()

#we are repeating sims 100x to get at variability of sampling
for(i in 1:length(nSamples)){
  for(j in 1:nsim){
    #getting the same 6x100 plots to sample for each pp
    temp[[j]] <- sample(c(1:nrow(plots)),nSamples[i])
    for(k in 1:npp){
    mean[i,j,k] <- mean(lengths(st_intersects(plots[temp[[j]],], th[[k]])))
    sd[i,j,k] <- sd(lengths(st_intersects(plots[temp[[j]],], th[[k]])))
  }}}


#here is function to make mean and sd for this exercise
means_fct <- function(tidy) {
  means <- as_tibble(tidy) %>% mutate(prop = c("10", "25", "50", "75", "90", "100")) %>%
    pivot_longer(cols = starts_with("V"),
                 values_to = "mean",
                 values_drop_na = TRUE) %>%

    return(means)
}

# sds <-   sd %>% pivot_longer(
#   cols = starts_with("V"),
#   values_to = "sd",
#   values_drop_na = TRUE
# )

dim(mean)
test <- as_tibble(mean)
mean_summ <- means_fct(mean)
#sd <- as.data.frame(sd)
mean_summ$prop <- factor(mean_summ$prop, levels = c("10", "25", "50", "75", "90", "100"))


y_int = mean(mean)
#sd$prop <- c("10", "25", "50", "75", "90", "100")

library(ggplot2)

#there is a small diff w truth bc 15 m radius of suitable habitat is not surveyable
test1 <- ggplot(mean_summ, aes(x=prop, y=mean)) +
  geom_boxplot()+
  # geom_bar(position=position_dodge(), stat="identity",
  #          colour='black') +
  # geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2)+
  geom_hline(yintercept=5.4, linetype="dashed",
             color = "red", size=1)+
  # geom_hline(yintercept=5.319444+2.968503, linetype="dashed",
  #            color = "red", size=1)+
  # geom_hline(yintercept=5.319444-2.968503, linetype="dashed",
  #            color = "red", size=1)+
  scale_x_discrete(breaks=c("10","25","50","75","90","100"),
                   labels=c("10%", "25%", "50%", "75%", "90%", "100%"))+
  xlab("Proportion of plots surveyed")+ ylab("Mean # burrows per plot") + ylim(0,13)
test1

####---- MOVING ON TO THE MORE CLUSTERED ONE ----####

my.samples <- list()
mean <- sd <- array(data=NA, dim=c(6,nsim,npp))
temp <- list()

for(i in 1:length(nSamples)){
  for(j in 1:nsim){
    #getting the same 6x100 plots to sample for each pp
    temp[[j]] <- sample(c(1:nrow(plots)),nSamples[i])
    for(k in 1:npp){
      mean[i,j,k] <- mean(lengths(st_intersects(plots[temp[[j]],], th2[[k]])))
      sd[i,j,k] <- sd(lengths(st_intersects(plots[temp[[j]],], th2[[k]])))
    }}}

mean_summ <- means_fct(mean)
#sd <- as.data.frame(sd)
mean_summ$prop <- factor(mean_summ$prop, levels = c("10", "25", "50", "75", "90", "100"))

#sd$prop <- c("10", "25", "50", "75", "90", "100")

library(ggplot2)

#there is a diff w truth bc 15 m radius of suitable habitat is not surveyable
test2 <- ggplot(mean_summ, aes(x=prop, y=mean)) +
  geom_boxplot()+
  # geom_bar(position=position_dodge(), stat="identity",
  #          colour='black') +
  # geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2)+
  geom_hline(yintercept=5.4, linetype="dashed", 
             color = "red", size=1)+
  # geom_hline(yintercept=5.319444+2.968503, linetype="dashed",
  #            color = "red", size=1)+
  # geom_hline(yintercept=5.319444-2.968503, linetype="dashed",
  #            color = "red", size=1)+
  scale_x_discrete(breaks=c("10","25","50","75","90","100"),
                   labels=c("10%", "25%", "50%", "75%", "90%", "100%"))+
  xlab("Proportion of plots surveyed")+ ylab("Mean # burrows per plot") + ylim(0,13)
test2

####---- MOVING ON TO LESS CLUSTERED, SMALLER AREA SURVEYABLE ----####

nSamples <- round(nrow(plots_survey)*prop)

my.samples <- list()
mean <- sd <- array(data=NA, dim=c(6,nsim,npp))
temp <- list()

#we are repeating sims 100x to get at variability of sampling
for(i in 1:length(nSamples)){
  for(j in 1:nsim){
    #getting the same 6x100 plots to sample for each pp
    temp[[j]] <- sample(c(1:nrow(plots)),nSamples[i])
    for(k in 1:npp){
      mean[i,j,k] <- mean(lengths(st_intersects(plots[temp[[j]],], th[[k]])))
      sd[i,j,k] <- sd(lengths(st_intersects(plots[temp[[j]],], th[[k]])))
    }}}

mean_summ <- means_fct(mean)
#sd <- as.data.frame(sd)
mean_summ$prop <- factor(mean_summ$prop, levels = c("10", "25", "50", "75", "90", "100"))

#sd$prop <- c("10", "25", "50", "75", "90", "100")

library(ggplot2)

#there is a diff w truth bc 15 m radius of suitable habitat is not surveyable
test3 <- ggplot(mean_summ, aes(x=prop, y=mean)) +
  geom_boxplot()+
  # geom_bar(position=position_dodge(), stat="identity",
  #          colour='black') +
  # geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2)+
  geom_hline(yintercept=5.4, linetype="dashed", 
             color = "red", size=1)+
  # geom_hline(yintercept=5.319444+2.968503, linetype="dashed",
  #            color = "red", size=1)+
  # geom_hline(yintercept=5.319444-2.968503, linetype="dashed",
  #            color = "red", size=1)+
  scale_x_discrete(breaks=c("10","25","50","75","90","100"),
                   labels=c("10%", "25%", "50%", "75%", "90%", "100%"))+
  xlab("Proportion of plots surveyed")+ ylab("Mean # burrows per plot") + ylim(0,13)
test3

####---- MOVING ON TO MORE CLUSTERED, SMALLER AREA SURVEYABLE ----####

my.samples <- list()
mean <- sd <- array(data=NA, dim=c(6,nsim,npp))
temp <- list()

#we are repeating sims 100x to get at variability of sampling
for(i in 1:length(nSamples)){
  for(j in 1:nsim){
    #getting the same 6x100 plots to sample for each pp
    temp[[j]] <- sample(c(1:nrow(plots)),nSamples[i])
    for(k in 1:npp){
      mean[i,j,k] <- mean(lengths(st_intersects(plots[temp[[j]],], th2[[k]])))
      sd[i,j,k] <- sd(lengths(st_intersects(plots[temp[[j]],], th2[[k]])))
    }}}

mean_summ <- means_fct(mean)
#sd <- as.data.frame(sd)
mean_summ$prop <- factor(mean_summ$prop, levels = c("10", "25", "50", "75", "90", "100"))

#sd$prop <- c("10", "25", "50", "75", "90", "100")

library(ggplot2)

#there is a diff w truth bc 15 m radius of suitable habitat is not surveyable
test4 <- ggplot(mean_summ, aes(x=prop, y=mean)) +
  geom_boxplot()+
  # geom_bar(position=position_dodge(), stat="identity",
  #          colour='black') +
  # geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2)+
  geom_hline(yintercept=5.4, linetype="dashed", 
             color = "red", size=1)+
  # geom_hline(yintercept=5.319444+2.968503, linetype="dashed",
  #            color = "red", size=1)+
  # geom_hline(yintercept=5.319444-2.968503, linetype="dashed",
  #            color = "red", size=1)+
  scale_x_discrete(breaks=c("10","25","50","75","90","100"),
                   labels=c("10%", "25%", "50%", "75%", "90%", "100%"))+
  xlab("Proportion of plots surveyed")+ ylab("Mean # burrows per plot") + ylim(0,13)
test4

library(cowplot)
plot_grid(
  test1, test2, test3, test4,
  labels=c('A', 'B', 'C', 'D'),
  # labels = c('little cluster, all surveyable', 'more cluster, all surveyable', 
  #            'little cluster, 15-m not surveyable', 'more cluster, 15-m not surveyable'),
  align="hv"
)
ggsave("G:/My Drive/Puffins/Figures/BurrowDensity_CirclePlots.jpg")

#need to assign occupancy to all of these burrows -- should use real data, but let's use 0.75 for now


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
