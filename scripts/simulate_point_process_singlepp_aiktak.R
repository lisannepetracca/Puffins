#---
#"Simulating Point Process for Island Resembling Aiktak"
#"Lisanne Petracca"
#"Nov 2022"
#---

#This is script summarizing the creation of a clustered point process on an island of similar size to Aiktak
#We adjust the number of burrows sampled (50, 100, 250, 500, 1000) and estimate bias and RMSE

library(sf)
library(ggplot2)
library(tidyverse)

#creating single point w center 750, 750; this will allow for size of destruction island (WA) and aiktak island (AK)
pt.df   <- data.frame(pt = 1, x = 750, y = 750)
#convert to sf
pt.sf   <- st_as_sf(pt.df, coords = c("x", "y")) 
#create area of 700-m radius to match destruction/aiktak 
island <- st_buffer(pt.sf, dist = 700)
plot(island, col="white")

#create buffer of "suitable" habitat that is 30 m from perimeter
#this reflects fact that TUPU habitat is often along cliff edge
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

#let's build 2.5-m radius plots across island
plots <- st_buffer(sf_points, dist = 2.5)
#let's only include those completely within polygon
plots <- st_join(plots, suitable_habitat, join = st_within, left=FALSE)

#let's see the suitable habitat w circular plots
ggplot() +
  geom_sf(data = suitable_habitat, color = "black", fill = "white", size=1) +
  geom_sf(data=plots, color="purple")

#here's some stuff on generating clustered poisson process

#https://keen-swartz-3146c4.netlify.app/pointpatterns.html#spatial-sampling-and-simulating-a-point-process
#kappa represents the intensity of the poisson process of cluster centers
#mu is mean number of points per cluster
#scale is standard deviation of random displacement (along each coordinate axis) of a point from the cluster center
#first one is homogeneous, second is not homogeneous

#a resource for spatstat
# http://spatstat.org/Melb2018/solutions/solution09.html

th <- list()

library(sf)
library(spatstat)

#these are parameters chosen to represent actual burrow densities from our aleutians field trip
set.seed(23)
kappa <- 0.0785
mu <- 3.51
nsim <- 100

for(i in 1:nsim){
  th[[i]] = st_sample(suitable_habitat, kappa = kappa, mu = mu, scale = 1, type = "Thomas") #3.6 scale
}


#?rThomas
#The help function obtained by ?rThomas details the meaning of the parameters kappa, mu and scale. 
#Simulating point processes means that the intensity is given, not the sample size. 
#The sample size within the observation window obtained this way is a random variable.

#?rThomas
#spdep package??

burrow_count <- list()

#this is the number of burrows intersecting the circular plots
for(i in 1:nsim){
  burrow_count[[i]] <- lengths(st_intersects(plots, th[[i]]))
}

#getting mean and sd of burrow count over those sims
mean_tot <- mean_sim <- list()
sd_tot <- sd_sim <- list()

(mean_tot <- mean(unlist(lapply(burrow_count, mean))))
(sd_tot <- mean(unlist(lapply(burrow_count, sd))))

#this is mean and sd by sim
mean_sim <- unlist(lapply(burrow_count, mean))
sd_sim <- unlist(lapply(burrow_count, sd))

#here is where we have to assign occupancy to each burrow 
#in circle plots, 71 of 126 burrows occupied #0.56
occu <- function(pp) {
  pp$occu <- rbinom(nrow(pp),1,0.56)
  return(pp)
}

#this adds occu (1 or 0) to each burrow
length(th)
th <- lapply(th,occu)

ggplot() +
  geom_sf(data = suitable_habitat, color = "black", fill = "white", size=1) +
  geom_sf(data=th, color = "darkgreen", size=1)
ggplot() +
  geom_sf(data = suitable_habitat, color = "black", fill = "white", size=1) +
  geom_sf(data=th2, color = "darkgreen", size=1)

#ok but now we have to exclude habitat that cannot be visited
#we are pretending that outer 15 m is a cliff face (which in the future may have a higher burrow density)
nogo_buffer <- st_buffer(island, dist = -15)
nogo_habitat <- st_difference(island, nogo_buffer)
survey_habitat <- st_difference(suitable_habitat, nogo_habitat)

#let's see what plots are in surveyable areas
plots_survey <- st_join(plots, survey_habitat, join = st_within, left=FALSE)

ggplot() +
  geom_sf(data = island, color = "black", fill = "white", size=1) +
  geom_sf(data = suitable_habitat, color = "black", fill = "black", size=1) +
  geom_sf(data = survey_habitat, color = "black", fill = "darkgreen", size=1)

#plots_survey stuff
clust1 <- ggplot() +
  geom_sf(data = island, color = "black", fill = "white", size=1) +
  geom_sf(data = suitable_habitat, color = "white", fill = "darkgrey", size=1) +
  geom_sf(data=plots, color = "blue", fill=NA, size=1)+
  geom_sf(data=th[[1]], color = "yellow", size=0.5)

clust2 <- ggplot() +
  geom_sf(data = island, color = "black", fill = "white", size=1) +
  geom_sf(data = suitable_habitat, color = "black", fill = "darkgrey", size=1) +
  geom_sf(data = survey_habitat, color = "darkgrey", fill = "lightgrey", size=1)+
  geom_sf(data=plots_survey, color = "blue", fill=NA, size=1)+
  geom_sf(data=th[[1]], color = "yellow", size=0.5)

library(cowplot)
plot_grid(
  clust1, clust2,
  labels=c('A', 'B'),
  # labels = c('all surveyable', 
  #            '15-m not surveyable'),
  align="hv"
)
#ggsave("G:/My Drive/Puffins/Figures/BurrowDensity_CirclePlots_Overview_SinglePP.jpg")


####---- HERE IS WORK WITH PROPORTION OF CIRCLES AND BURROW DENSITY ----####

#number of burrows sampled
nSamples <- c(50, 100, 250, 500, 1000) 

abundance <- sd <- bias <- rmse <- matrix(NA, nrow=5,ncol=nsim)
temp <- temp2 <- var <- list()
#occu_vec <- vector()

#let's do the negative binomial thing
library(MASS)
plot.area <- pi*2.5^2
total.area <- area(suitable_habitat)
M <- total.area/plot.area #this is the number of potential sampling units

#nplot <- length(nplots)

#we are repeating sims 100x to get at variability of sampling

for(i in 1:length(nSamples)){ #six types of sampling
  for(j in 1:nsim){ #number of repeats on point process
    temp <- sample(c(1:nrow(plots)),nSamples[i])#these are selected plots
    #this is number of burrows for each plot intersecting point process
    temp2 <- lengths(st_intersects(plots[temp,], th[[j]]))
    
    #this is the model of the mean
    mod.mn <- glm.nb(temp2 ~ 1)
    
    #note, log link 
    abundance[i,j] <- (exp(mod.mn$coefficients)*M) #total.area/plot.area is big M
    
    #empirical variance of the estimate (plot-level abundance) is: 
    var <- exp(mod.mn$coefficients) + exp(mod.mn$coefficients)^2/summary(mod.mn)$theta #this is s squared
    
    #SD(abundance) = 
    sd[i,j] <- sqrt(M^2 * (var / nSamples[i]) * (1-(nSamples[i]/M))) 
    bias[i,j] <- abundance[i,j] - nrow(th[[j]])
    rmse[i,j] <- (var + bias[i,j]^2)^1/2
  }}

sum(lapply(nrow(th)))

#do this at two time periods
#ratio of abundance is trend (Nt-N(t+10))/N(t)

#then would calculate variance around this
#bootstrap?

test <- as_tibble(bias)
#here is function to make mean and sd for this exercise
process_fct <- function(tidy) {
  term <- as_tibble(tidy) %>% mutate(nplot = c("50", "100", "250", "500", "1000")) %>%
    pivot_longer(cols = starts_with("V"),
                 values_to = "mean",
                 values_drop_na = TRUE) %>%
    
    return(term)
}

dim(mean)
bias_dup <- bias
rmse_dup <- rmse
bias_tib <- process_fct(bias_dup)
rmse_tib <- process_fct(rmse_dup)
# mean_real <- as.data.frame(matrix(mean_sim, nrow=6, ncol=nsim, byrow=T))
# mean_diff <- mean_dup - mean_real
# mean_summ1 <- means_fct(mean_diff)

#sd <- as.data.frame(sd)
bias_tib$nplot <- factor(bias_tib$nplot, levels = c("50", "100", "250", "500", "1000"))
rmse_tib$nplot <- factor(rmse_tib$nplot, levels = c("50", "100", "250", "500", "1000"))
#sd$prop <- c("10", "25", "50", "75", "90", "100")

test1 <- ggplot(bias_tib, aes(x=nplot, y=mean)) +
  geom_boxplot(fill="lightblue")+
  geom_hline(yintercept=0, linetype="dashed",
             color = "plum", size=1)+
  # scale_x_discrete(breaks=c("50", "100", "250", "500", "1000"),
  #                  labels=c("10%", "25%", "50%", "75%", "90%", "100%"))+
  xlab("Number of plots surveyed")+ theme_classic() + ylab("Bias") #+ ylim(-1,1)
test1

test2 <- ggplot(rmse_tib, aes(x=nplot, y=mean)) +
  geom_boxplot(fill="lightblue")+
  geom_hline(yintercept=0, linetype="dashed",
             color = "plum", size=1)+
  # scale_x_discrete(breaks=c("50", "100", "250", "500", "1000"),
  #                  labels=c("10%", "25%", "50%", "75%", "90%", "100%"))+
  xlab("Number of plots surveyed")+ theme_classic() + ylab("RMSE") #+ ylim(-1,1)
test2

library(cowplot)
plot_grid(
  test1, test2,
  labels=c('A', 'B'),
  # labels = c('all surveyable', 
  #            '15-m not surveyable'),
  align="hv"
)
ggsave("G:/My Drive/Puffins/Figures/BurrowDensity_CirclePlots_SinglePP_Aiktak_Bias_RMSE.jpg")