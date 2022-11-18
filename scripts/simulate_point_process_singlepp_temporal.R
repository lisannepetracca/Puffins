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

growth <- c(1, 0.9, 0.75, 0.5, 0.25)
th_t2_100perc <- th_t2_90perc <- th_t2_75perc <- th_t2_50perc <- th_t2_25perc <- list()
th_t3_100perc <- th_t3_90perc <- th_t3_75perc <- th_t3_50perc <- th_t3_25perc <- list()

nrow(th[[1]])

#generate t=2
for(i in 1:100){
  th_t2_100perc[[i]] <- sample(c(1:nrow(th[[i]])),nrow(th[[i]])*sqrt(growth[1]))
  th_t2_90perc[[i]] <- sample(c(1:nrow(th[[i]])),nrow(th[[i]])*sqrt(growth[2]))
  th_t2_75perc[[i]] <- sample(c(1:nrow(th[[i]])),nrow(th[[i]])*sqrt(growth[3]))
  th_t2_50perc[[i]] <- sample(c(1:nrow(th[[i]])),nrow(th[[i]])*sqrt(growth[4]))
  th_t2_25perc[[i]] <- sample(c(1:nrow(th[[i]])),nrow(th[[i]])*sqrt(growth[5]))
}

#generate t=3
for(i in 1:100){
  th_t3_100perc[[i]] <- sample(c(1:length(th_t2_100perc[[i]])),length(th_t2_100perc[[i]])*sqrt(growth[1]))
  th_t3_90perc[[i]] <- sample(c(1:length(th_t2_90perc[[i]])),length(th_t2_90perc[[i]])*sqrt(growth[2]))
  th_t3_75perc[[i]] <- sample(c(1:length(th_t2_75perc[[i]])),length(th_t2_75perc[[i]])*sqrt(growth[3]))
  th_t3_50perc[[i]] <- sample(c(1:length(th_t2_50perc[[i]])),length(th_t2_50perc[[i]])*sqrt(growth[4]))
  th_t3_25perc[[i]] <- sample(c(1:length(th_t2_25perc[[i]])),length(th_t2_25perc[[i]])*sqrt(growth[5]))
}

length(th_t3_25perc[[1]])

#so this is a list of ids at the end of 10 years for no change & 90%, 75%, 50% change every 5 years
all_ids <- list(th_t2_100perc, th_t2_90perc, th_t2_75perc, th_t2_50perc, th_t2_25perc, 
                th_t3_100perc, th_t3_90perc, th_t3_75perc, th_t3_50perc, th_t3_25perc)

all_ids[[1]][[1]]

#what proportion of circles are sampled?
# prop <- c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
# nSamples <- round(nrow(plots)*prop) 

nSamples <- c(50, 100, 250, 500, 1000) 

abundance_est <- abundance_true <- sd <- bias <- rmse <- lambda <- array(NA, dim=c(length(nSamples), length(all_ids), nsim))
temp <- temp2 <- var <- list()
#occu_vec <- vector()

#let's do the negative binomial thing
library(MASS)
plot.area <- pi*2.5^2
total.area <- st_area(suitable_habitat)
M <- total.area/plot.area #this is the number of potential sampling units

#getting N1 abundance in the right place
for(i in 1:nsim){
  abundance_true[,,i] <- nrow(th[[i]])
}

mean(abundance_true)
abundance_true[,,4]

#nplot <- length(nplots)

#we are repeating sims 100x to get at variability of sampling

for(i in 1:length(nSamples)){ #six types of sampling
  for(j in 1:length(all_ids))
    for(k in 1:nsim){ #number of repeats on point process
    temp <- sample(c(1:nrow(plots)),nSamples[i])#these are selected plots
    #this is number of burrows for each plot intersecting point process
    temp2 <- lengths(st_intersects(plots[temp,], th[[k]][all_ids[[j]][[k]],]))
    
    #this is the model of the mean
    mod.mn <- glm.nb(temp2 ~ 1)
    
    #note, log link 
    abundance_est[i,j,k] <- (exp(mod.mn$coefficients)*M) #total.area/plot.area is big M
    
    #empirical variance of the estimate (plot-level abundance) is: 
    var <- exp(mod.mn$coefficients) + exp(mod.mn$coefficients)^2/summary(mod.mn)$theta #this is s squared
    
    #SD(abundance) = 
    sd[i,j,k] <- sqrt(M^2 * (var / nSamples[i]) * (1-(nSamples[i]/M))) 
    bias[i,j,k] <- abundance_est[i,j,k] - nrow(th[[k]])
    rmse[i,j,k] <- (var + bias[i,j,k]^2)^1/2
    
    #lambda
    lambda[i,j,k] <- (abundance_true[i,j,k] - abundance_est[i,j,k])/abundance_true[i,j,k]
    }}

#here is function to make mean and sd for this exercise

lam_matrix <- as.data.frame.table(lambda)
length(unique(lam_matrix$Var1))
lam_matrix$Var1 <- recode(lam_matrix$Var1, A = "50", B = "100", C = "250",
                            D= "500", E = "1000")
length(unique(lam_matrix$Var2))
lam_matrix$Var2 <- recode(lam_matrix$Var2, A = "T2, 0%", B = "T2, 10%", 
                           C = "T2, 25%", D = "T2, 50%", E = "T2, 75%",
                           F = "T3, 0%", G = "T3, 10%", 
                           H = "T3, 25%", I = "T3, 50%", J = "T3, 75%")
lam_matrix <- lam_matrix %>% separate(Var2, c("A", "B"))
lam_matrix <- lam_matrix %>% filter(A=="T3")

head(lam_matrix)

library(ggplot2)

hline_dat = data.frame(B=c("0", "10", "25",
                              "50", "75"),
                       threshold=c(0, 0.1, 0.25, 0.5, 0.75))
hum_names <- as_labeller(
  c(`0` = "0% Decline", `10` = "10% Decline",`25` = "25% Decline", 
    `50` = "50% Decline",`75` = "75% Decline"))

test1 <- ggplot(lam_matrix, aes(x=Var1, y=Freq)) +
  geom_boxplot(aes(fill=B))+
  geom_hline(data=hline_dat, aes(yintercept=threshold), colour="plum", size=1, lty=2) +
  # scale_x_discrete(breaks=c("50", "100", "250", "500", "1000"),
  #                  labels=c("10%", "25%", "50%", "75%", "90%", "100%"))+
  xlab("Number of plots surveyed")+ theme_bw() + ylab("Estimated decline") +
  facet_grid(~B, labeller = hum_names)+#+ ylim(-1,1)+
  scale_fill_manual(values = c('#edf8fb','#b3cde3','#8c96c6','#8856a7','#810f7c'))+
  theme(legend.position = "none")
test1
ggsave("G:/My Drive/Puffins/Figures/BurrowDensity_CirclePlots_SinglePP_Aiktak_Trend_diffcol.jpg")

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

