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

#let's build 2.5-m plots across island
plots <- st_buffer(sf_points, dist = 2.5)
#let's only include those completely within polygon
plots <- st_join(plots, suitable_habitat, join = st_within, left=FALSE)

th <- list()

library(sf)
library(spatstat)
#here is code for testing via grid search of kappa and mu; the latter would be provided as vectors
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

#this is grand mean and sd
(mean_tot <- mean(unlist(lapply(burrow_count, mean))))
(sd_tot <- mean(unlist(lapply(burrow_count, sd))))

#this is mean and sd by sim
mean_sim <- unlist(lapply(burrow_count, mean))
sd_sim <- unlist(lapply(burrow_count, sd))

#here is where we have to assign occupancy to each burrow (0.75)
#in circle plots, 71 1's of 126 burrows #0.56
# occu <- function(pp) {
#   pp$occu <- rbinom(nrow(pp),1,0.56)
#   return(pp)
# }

# #this adds occu (1 or 0) to each burrow
# length(th)
# th <- lapply(th,occu)

#ok but now we have to exclude habitat that cannot be visited
nogo_buffer <- st_buffer(island, dist = -15)
nogo_habitat <- st_difference(island, nogo_buffer)
survey_habitat <- st_difference(suitable_habitat, nogo_habitat)

#let's see what plots are in surveyable areas
plots_survey <- st_join(plots, survey_habitat, join = st_within, left=FALSE)
dim(plots) #there are 4312 total plots
dim(plots_survey) #1688 are surveyable

#true density of burrows in suitable habitat is
#nrow(th)/st_area(suitable_habitat) #0.323726/m2


####---- HERE IS WORK WITH POPULATION TREND AND THEN PROPORTION OF CIRCLES SAMPLED ----####

#create 10%, 25%, and 50% loss each 5-yr period for 10-yr period at burrow level
#these are % decline each 5-yr period 
34833 * 0.9 * 0.9
34833 * 0.75 * 0.75
34833 * 0.5 * 0.5

growth <- c(1, 0.9, 0.75, 0.5)
ids_100 <- ids_90 <- ids_75 <- ids_50 <- list()

nrow(th[[1]])

for(i in 1:100){
  ids_100[[i]] <- sample(c(1:nrow(th[[i]])),nrow(th[[i]])*growth[1]*growth[1])
  ids_90[[i]] <- sample(c(1:nrow(th[[i]])),nrow(th[[i]])*growth[2]*growth[2])
  ids_75[[i]] <- sample(c(1:nrow(th[[i]])),nrow(th[[i]])*growth[3]*growth[3])
  ids_50[[i]] <- sample(c(1:nrow(th[[i]])),nrow(th[[i]])*growth[4]*growth[4])
}

length(ids_100[[1]])
length(ids_90[[1]])
length(ids_75[[1]])
length(ids_50[[1]]) #ok, these all check out as to remaining ids

#so this is a list of ids at the end of 10 years for no change & 90%, 75%, 50% change every 5 years
all_ids <- list(ids_100, ids_90, ids_75, ids_50)
all_ids[[1]][[1]]
#what proportion of circles are sampled?
prop <- c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
nSamples <- round(nrow(plots)*prop) 

mean <- array(NA, dim =c(6,100,4))
temp <- temp2 <- list()
occu_vec <- vector()

#we are repeating sims 100x to get at variability of sampling

for(i in 1:length(nSamples)){ #six types of sampling
  for(j in 1:nsim){ #number of repeats on point process
    for(k in 1:length(growth)){
    temp <- sample(c(1:nrow(plots)),nSamples[i])#these are selected plots (this won't change)
    #this is number of burrows for plots intersecting point process
    temp2 <- lengths(st_intersects(plots[temp,], th[[j]][all_ids[[k]][[j]],]))
    mean[i,j,k] <- mean(temp2)
  }}}

library(dplyr)


#here is function to make mean and sd for this exercise
# means_fct <- function(tidy) {
#   means <- as_tibble(tidy) %>% mutate(prop = c("10", "25", "50", "75", "90", "100")) %>%
#     pivot_longer(cols = starts_with("V"),
#                  values_to = "mean",
#                  values_drop_na = TRUE) %>%
#     
#     return(means)
# }

dim(meanz)
mean_dup <- mean
mean_real <- matrix(mean_sim, nrow=4, ncol=nsim, byrow=T)
mean_real_t <- t(mean_real)
dim(mean_real_t)
mean_real_array <- array(data=NA, dim=c(6,100,4))
for(i in 1:6){
  mean_real_array[i,,] <- mean_real_t
}

mean_real_array[,,1]

meanz <- mean
dim(meanz)
dim(mean_real_array)
trend <- (1 - (mean / mean_real_array)) *100
trend_matrix <- as.data.frame.table(trend)
trend_matrix$Var1 <- recode(trend_matrix$Var1, A = "10%", B = "25%", C = "50%",
                    D= "75%", E = "90%", F="100%")
trend_matrix$Var3 <- recode(trend_matrix$Var3, A = "No Decline", B = "19% Decline", C = "44% Decline",
                    D= "75% Decline")
hline_dat = data.frame(Var3=c("No Decline", "19% Decline", "44% Decline",
                              "75% Decline"),
                       threshold=c(0, 19, 43.75, 75))


library(ggplot2)

test1 <- ggplot(trend_matrix, aes(x=Var1, y=Freq)) +
  geom_boxplot()+
  geom_hline(data=hline_dat, aes(yintercept=threshold), colour="salmon", size=1, lty=2) +
  scale_x_discrete(breaks=c("10%","25%","50%","75%","90%","100%"),
                   labels=c("10%", "25%", "50%", "75%", "90%", "100%"))+
  xlab("Proportion of plots surveyed")+ ylab("Estimated Population Decline (%)") + #ylim(-1,1)+
  facet_grid(~factor(Var3, levels=c("No Decline", "19% Decline", "44% Decline",
                                     "75% Decline")))
test1
ggsave("G:/My Drive/Puffins/Figures/BurrowDensity_CirclePlots_SinglePP_Aiktak_PopDecline.jpg")

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

