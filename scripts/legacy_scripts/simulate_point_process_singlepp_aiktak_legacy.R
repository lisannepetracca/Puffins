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

####---- CHOSEN METHOD FOR MAKING AUTOCORRELATED DATA ----####

library(raster)
library(gstat)

#https://search.r-project.org/CRAN/refmans/raster/html/rasterFromXYZ.html

#ok so first we create  basic x-y raster by row and column
#then assign those values to raster in our coord system
r <- raster(nrow=200, ncol=100, xmn=0, xmx=200, ymn=0, ymx=100, crs="")
raster::values(r) <- runif(20000,0,1)
#don't like it, let's go back to yesterday
#here test is an empty raster
test <- as.data.frame(r, row.names=NULL, optional=FALSE,
                      xy=TRUE, na.rm=T)
head(test)

#define the gstat object (spatial model)
#ok so range definitely controls the autocorrelation; lower (1) means less autocorrelated compared to higher (10)

#beta seems to control mean of values, when psill is 10

#when psill is low (0.02), there seems to be more of the low values, more difficult to get yellows
#when high (50), lots of yellow

#formula specifies dependent variable as linear model of independent variables; z~1 for ordinary and simple kriging
#beta (vector with trend coefficients) also needs to be defined for simple kriging
#nmax is number of nearest observations that should be used for a kriging prediction or simulation
#dummy means to only consider for unconditional simulation
#model specifies the variogram
set.seed(67)
g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, model=vgm(psill=10,model="Exp",range=300), nmax=20)
yy <- predict(g.dummy, newdata=test, nsim=1) #was nsim=4

# show one realization
gridded(yy) = ~x+y
spplot(yy[1]) 

head(yy)

#?vgm() was really helpful in generating sample variograms

#assigning those values to raster?
# xmin       ymin       xmax       ymax 
# -2168649.7  -308459.1  2471350.3  2171540.9 
grid_raster <- raster::raster(nrow=200, ncol=100, xmn=-2168649.7, xmx=2471350.3, ymn=-308459.1, ymx=2171540.9, crs=CRS("+init=epsg:3338"))

raster::values(grid_raster) <- yy$sim1
plot(grid_raster)

#ok so grid_raster has the autocorrelated covariate values

#scale it

grid_scale <- (grid_raster-cellStats(grid_raster, "mean"))/cellStats(grid_raster, "sd")
plot(grid_scale)

grid_plot <- as.data.frame(grid_scale, xy=TRUE)
names(grid_plot) <- c("x", "y", "habitat_suitability")

#ok let's get centroids and extract cov information
centroids <- st_centroid(TUPU_grid_1600km2)

#now we can plot as we did for the vector data, but take note of "geom_raster" argument
#also, note that we are taking the third column of "elev_df" as our color fill
#as this is the column that has our raster values
ggplot() +
  geom_raster(data = grid_plot , aes(x = x, y = y, fill = habitat_suitability)) +
  scale_fill_viridis_c() +
  geom_sf(data = centroids, fill=NA, color="black", size = .1) +
  coord_sf()
ggsave("data/a_figures/1600km2_grid_w_spatial_cov.jpg")

ggplot() +
  geom_raster(data = grid_plot , aes(x = x, y = y, fill = habitat_suitability)) +
  scale_fill_viridis_c() +
  geom_sf(data = TUPU_proj, fill=NA, color="black", size = .1) +
  coord_sf()
ggsave("data/a_figures/colonies_w_spatial_cov.jpg")

#seeing something ab psi
TUPU_grid_1600km2_sf <- st_as_sf(TUPU_grid_1600km2)
TUPU_grid_1600km2_sf$psi <- simModel$psi
ggplot() +
  geom_raster(data = grid_plot , aes(x = x, y = y, fill = habitat_suitability)) +
  scale_fill_viridis_c() +
  geom_sf(data = centroids, color="black", size = .1) +
  coord_sf()
ggsave("data/a_figures/grid_w_spatial_cov.jpg")


