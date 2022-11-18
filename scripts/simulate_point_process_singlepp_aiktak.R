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

th <- list()

library(sf)
library(spatstat)

set.seed(23)
kappa <- 0.0785
mu <- 3.51
nsim <- 100

for(i in 1:nsim){
  th[[i]] = st_sample(suitable_habitat, kappa = kappa, mu = mu, scale = 1, type = "Thomas") #3.6 scale
}

burrow_count <- list()

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

#here is where we have to assign occupancy to each burrow (0.75)
#in circle plots, 71 1's of 126 burrows #0.56
occu <- function(pp) {
  pp$occu <- rbinom(nrow(pp),1,0.56)
  return(pp)
}

#this adds occu (1 or 0) to each burrow
length(th)
th <- lapply(th,occu)

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

#ok but now we have to exclude habitat that cannot be visited
?st_buffer
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

#what proportion of circles are sampled?
prop <- c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
#one strategy
#nSamples <- round(nrow(plots)*prop) 
#second strategy
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

library(ggplot2)

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

####---- PLOTS FOR OCCUPANCY ----####

occu_summ1 <- means_fct(occu_mean)
max(occu_summ1$mean)

#sd <- as.data.frame(sd)
occu_summ1$prop <- factor(occu_summ1$prop, levels = c("10", "25", "50", "75", "90", "100"))

occu1 <- ggplot(occu_summ1, aes(x=prop, y=mean)) +
  geom_boxplot()+
  geom_hline(yintercept=0.56, linetype="dashed",
             color = "red", size=1)+
  scale_x_discrete(breaks=c("10","25","50","75","90","100"),
                   labels=c("10%", "25%", "50%", "75%", "90%", "100%"))+
  xlab("Proportion of plots surveyed")+ ylab("Estimated occupancy") + ylim(0.4,0.7)
occu1

####---- MOVING ON TO SMALLER AREA SURVEYABLE ----####

nSamples <- round(nrow(plots_survey)*prop)

my.samples <- list()
mean <- occu_mean <- sd <- matrix(NA, nrow=6,ncol=nsim)
temp <- temp2 <- temp3 <- list()
occu_vec <- vector()

#we are repeating sims 100x to get at variability of sampling

for(i in 1:length(nSamples)){ #six types of sampling
  for(j in 1:nsim){ #number of repeats on point process
    temp <- sample(c(1:nrow(plots_survey)),nSamples[i])#these are selected plots
    #this is number of burrows for plots intersecting point process
    temp2 <- lengths(st_intersects(plots_survey[temp,], th[[j]]))
    #this provides id's of burrows within those intersecting plots
    temp3 <- st_intersects(plots_survey[temp,], th[[j]])
    #this is the number of circular plots
    for(m in 1:length(temp2)){
      if (temp2[m]==0) {
        occu_vec[m] <- NA 
      }
      else {
        x <- sample(c(1:temp2[m]),1)
        occu_vec[m] <- th[[j]][temp3[[m]][x],]$occu
      }
    }
    mean[i,j] <- mean(temp2)
    occu_mean[i,j] <- mean(occu_vec, na.rm=T)
  }}

dim(mean)
mean_dup <- mean
mean_real <- as.data.frame(matrix(mean_sim, nrow=6, ncol=nsim, byrow=T))
mean_diff <- mean_dup - mean_real
mean_summ2 <- means_fct(mean_diff)
#sd <- as.data.frame(sd)
mean_summ2$prop <- factor(mean_summ2$prop, levels = c("10", "25", "50", "75", "90", "100"))
#sd$prop <- c("10", "25", "50", "75", "90", "100")

library(ggplot2)

test2 <- ggplot(mean_summ2, aes(x=prop, y=mean)) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed",
             color = "red", size=1)+
  scale_x_discrete(breaks=c("10","25","50","75","90","100"),
                   labels=c("10%", "25%", "50%", "75%", "90%", "100%"))+
  xlab("Proportion of plots surveyed")+ ylab("Deviation of mean burrow density from known mean") + ylim(-1,1)
test2

####---- PLOTS FOR OCCUPANCY ----####

occu_summ2 <- means_fct(occu_mean)
max(occu_summ2$mean)

#sd <- as.data.frame(sd)
occu_summ2$prop <- factor(occu_summ2$prop, levels = c("10", "25", "50", "75", "90", "100"))

occu2 <- ggplot(occu_summ2, aes(x=prop, y=mean)) +
  geom_boxplot()+
  geom_hline(yintercept=0.56, linetype="dashed",
             color = "red", size=1)+
  scale_x_discrete(breaks=c("10","25","50","75","90","100"),
                   labels=c("10%", "25%", "50%", "75%", "90%", "100%"))+
  xlab("Proportion of plots surveyed")+ ylab("Estimated occupancy") + ylim(0.4,0.7)
occu2


####---- ok let's try to code sampling two burrows as opposed to 1 ----####

#what proportion of circles are sampled?
prop <- c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
nSamples <- round(nrow(plots)*prop) 

my.samples <- list()
mean <- occu_mean <- sd <- matrix(NA, nrow=6,ncol=nsim)
temp <- temp2 <- temp3 <- list()
occu_vec <- vector()

#we are repeating sims 100x to get at variability of sampling

for(i in 1:length(nSamples)){ #six types of sampling
  for(j in 1:nsim){ #number of repeats on point process
    temp <- sample(c(1:nrow(plots)),nSamples[i])#these are selected plots
    #this is number of burrows for plots intersecting point process
    temp2 <- lengths(st_intersects(plots[temp,], th[[j]]))
    #this provides id's of burrows within those intersecting plots
    temp3 <- st_intersects(plots[temp,], th[[j]])
    occu_vec <- vector()
    #this is the number of circular plots
    for(m in 1:length(temp2)){
      if (temp2[m]==0) {
        occu_vec <- c(occu_vec, NA) 
      }
      else if (temp2[m]==1){
        x <- sample(c(1:temp2[m]),1)
        occu_vec <- c(occu_vec, th[[j]][temp3[[m]][x],]$occu)
      } else {
        x <- sample(c(1:temp2[m]),2)
        occu_vec <- c(occu_vec, th[[j]][temp3[[m]][x[1]],]$occu, th[[j]][temp3[[m]][x[2]],]$occu)
      }
    }
    mean[i,j] <- mean(temp2)
    occu_mean[i,j] <- mean(occu_vec, na.rm=T)
  }}

occu_summ3 <- means_fct(occu_mean)
max(occu_summ3$mean)

#sd <- as.data.frame(sd)
occu_summ3$prop <- factor(occu_summ3$prop, levels = c("10", "25", "50", "75", "90", "100"))

occu3 <- ggplot(occu_summ3, aes(x=prop, y=mean)) +
  geom_boxplot()+
  geom_hline(yintercept=0.56, linetype="dashed",
             color = "red", size=1)+
  scale_x_discrete(breaks=c("10","25","50","75","90","100"),
                   labels=c("10%", "25%", "50%", "75%", "90%", "100%"))+
  xlab("Proportion of plots surveyed")+ ylab("Estimated occupancy") + ylim(0.4,0.7)
occu3

#what proportion of circles are sampled?
prop <- c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
nSamples <- round(nrow(plots_survey)*prop) 

my.samples <- list()
mean <- occu_mean <- sd <- matrix(NA, nrow=6,ncol=nsim)
temp <- temp2 <- temp3 <- list()
occu_vec <- vector()

#we are repeating sims 100x to get at variability of sampling

for(i in 1:length(nSamples)){ #six types of sampling
  for(j in 1:nsim){ #number of repeats on point process
    temp <- sample(c(1:nrow(plots_survey)),nSamples[i])#these are selected plots
    #this is number of burrows for plots intersecting point process
    temp2 <- lengths(st_intersects(plots_survey[temp,], th[[j]]))
    #this provides id's of burrows within those intersecting plots
    temp3 <- st_intersects(plots_survey[temp,], th[[j]])
    #this is the number of circular plots
    occu_vec <- vector()
    for(m in 1:length(temp2)){
      if (temp2[m]==0) {
        occu_vec <- c(occu_vec, NA) 
      }
      else if (temp2[m]==1){
        x <- sample(c(1:temp2[m]),1)
        occu_vec <- c(occu_vec, th[[j]][temp3[[m]][x],]$occu)
      } else {
        x <- sample(c(1:temp2[m]),2)
        occu_vec <- c(occu_vec, th[[j]][temp3[[m]][x[1]],]$occu, th[[j]][temp3[[m]][x[2]],]$occu)
      }
    }
    mean[i,j] <- mean(temp2)
    occu_mean[i,j] <- mean(occu_vec, na.rm=T)
  }}

occu_summ4 <- means_fct(occu_mean)
max(occu_summ4$mean)

#sd <- as.data.frame(sd)
occu_summ4$prop <- factor(occu_summ4$prop, levels = c("10", "25", "50", "75", "90", "100"))

occu4 <- ggplot(occu_summ4, aes(x=prop, y=mean)) +
  geom_boxplot()+
  geom_hline(yintercept=0.56, linetype="dashed",
             color = "red", size=1)+
  scale_x_discrete(breaks=c("10","25","50","75","90","100"),
                   labels=c("10%", "25%", "50%", "75%", "90%", "100%"))+
  xlab("Proportion of plots surveyed")+ ylab("Estimated occupancy") + ylim(0.4,0.7)
occu4




library(cowplot)
plot_grid(
  test1, test2,
  labels=c('A', 'B'),
  # labels = c('all surveyable', 
  #            '15-m not surveyable'),
  align="hv"
)
ggsave("G:/My Drive/Puffins/Figures/BurrowDensity_CirclePlots_SinglePP_Aiktak.jpg")

plot_grid(
  occu1, occu2,
  labels=c('A', 'B'),
  # labels = c('all surveyable', 
  #            '15-m not surveyable'),
  align="hv"
)
ggsave("G:/My Drive/Puffins/Figures/Occu_SingleBurrow_CirclePlots_SinglePP_Aiktak.jpg")

dev.off()
plot_grid(
  occu1, occu2, occu3, occu4,
  labels=c('A', 'B', 'C', 'D'))
# labels = c('all surveyable, single burrow', 
#            '15-m not surveyable, single burrow',
#              'all surveyable, two burrows', 
#            '15-m not surveyable, two burrows'),
ggsave("G:/My Drive/Puffins/Figures/Occu_OneAndTwoBurrow_CirclePlots_SinglePP_Aiktak.jpg")

#need to assign occupancy to all of these burrows -- should use real data, but let's use 0.75 for now


#notes at end of day friday
#need to repeat this like 100x to get at variation in plots surveyed; mean and SE rather than SD

#ok so i did the repeat 100x and have boxplots getting at mean for more homogeneous and more clustered
#how to keep number of burrows EXACTLY the same while changing clusteredness



#GRID SEARCH FOR KAPPA AND MU

# library(sf)
# library(spatstat)
# #here is code for testing via grid search of kappa and mu; the latter would be provided as vectors
# kappa <- 0.0785
# mu <- 3.51
# c <- expand.grid(kappa, mu)
# test <- rep(list(list()), nrow(c))
# #nsim <- 20
#   for(i in 1:nrow(c)){
#     for(j in 1:15){
#     temp = st_sample(suitable_habitat, kappa = c$Var1[i], mu = c$Var2[i], scale = 1, type = "Thomas") #3.6 scale
#     test[[i]][[j]] <- temp
#     }}
# 
# burrow_count <- rep(list(list()), nrow(c))
# 
# for(i in 1:nrow(c)){
#   for(j in 1:15){
#   burrow_count[[i]][[j]] <- lengths(st_intersects(plots, test[[i]][[j]]))
#   }}
#  
# #getting mean and sd of burrow count over those sims
# test <- list() 
# for(i in 1:nrow(c)){
# test[[i]] <- unlist(lapply(burrow_count[[i]], mean))
# }
# (test <- unlist(lapply(test, mean)))
# test <- list() 
# for(i in 1:nrow(c)){
#   test[[i]] <- unlist(lapply(burrow_count[[i]], sd))
# }
# (test <- unlist(lapply(test, mean)))
