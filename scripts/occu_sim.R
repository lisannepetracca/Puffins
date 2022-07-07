library(here)
library(jagsUI)

#there seem to be a total of 960 unique colony names (data_colony nrows)

set.seed(81)
all_colonies <- 960
max_visit <- 10
occu_probs <- runif(all_colonies)

#simulated true occupancy data for all colonies
occu_sim <- rbinom(all_colonies,1,occu_probs)
sum(occu_sim)
#truth is 480

sum(occu_sim[my.sites[[1]]]) #27
sum(occu_sim[my.sites[[2]]]) #130
sum(occu_sim[my.sites[[3]]]) #237
sum(occu_sim[my.sites[[4]]]) #356
sum(occu_sim[my.sites[[5]]]) #480

#simulated observation data for x number of visits

p <- 0.7
y <- matrix(data=NA, nrow=all_colonies, ncol=max_visit)

#here is a simulated dataset for 960 rows and 10 visits
for(i in 1:all_colonies){
    y[i,] <- rbinom(max_visit,1,occu_sim[i]*p)
    }

#now we make subsamples based on proportion sampled and number of visits
prop <- c(0.05, 0.25, 0.50, 0.75, 1)
visits <- c(.2,.5,1)

y_sim <- my.sites <- my.visits <- list()
for(i in 1:length(prop)){
my.sites[[i]] <- sample(c(1:all_colonies),all_colonies*prop[i])}

for(i in 1:length(visits)){
my.visits[[i]] <- sample(c(1:max_visit), max_visit*visits[i])}

y_sim <- list()

my.sites.all <- rep(my.sites,each=3)
my.visits.all <- rep(my.visits,5)

for(i in 1:15){
  y_sim[[i]] <- matrix(data=NA, nrow=960, ncol=length(my.visits.all[[i]]))
  y_sim[[i]][my.sites.all[[i]],] <- y[my.sites.all[[i]], my.visits.all[[i]]]
}

sink("puffin_sim.txt")
cat("
    model {
    
    p ~ dbeta(1,1)
    for(i in 1:colonies){
    psi[i] ~ dbeta(1,1)
    }
    
  # likelihood 
for(i in 1:colonies){
for(j in 1:visits){
    y[i,j] ~ dbin(p * z[i], 1)
    }
    z[i] ~ dbern(psi[i])
}

#estimated number of occupied colonies
N <- sum(z)
  }",fill=TRUE)
sink()

win.data <- list()
for(i in 1:15){
win.data[[i]] <- list(y=y_sim[[i]], colonies=nrow(y_sim[[i]]), visits=ncol(y_sim[[i]]))
}

#provide initial values
inits <- function(){list(
    p = runif(1,0,1),
    z=rep(1,960))}


nb <- 100 #100000 #100 #30000 #1000 #2000 #15000 #change iterations to low, burn in low, spit out
ni <- 1000 #300000 #1000 #150000 #100 #2000 #10000 #40000  
nt <- 1 #10 #3
nc <- 3 #3

# Parameters to estimate
#params <- "psi" 
params <- c("p", "N")

out <- list()
for(i in 1:15){
  out[[i]] <- jags(win.data[[i]], inits, params, "puffin_sim.txt", n.adapt=100, n.chains=nc, n.iter=ni, n.burn = nb, n.thin=nt, parallel=T)
}
#print(out,n=3)
print(out)

getwd()
saveRDS(out, file = "results/basic_occu_sim.rds")

readRDS("results/basic_occu_sim.rds")

#let's get those estimates and plot against truth
N <- p <- list()
for(i in 1:15){
N[[i]] <- 480 - out[[i]]$sims.list$N
p[[i]] <- 0.7 - out[[i]]$sims.list$p
}

library(reshape2)

N_bias_df <- data.frame(matrix(unlist(N), nrow=length(N), byrow=TRUE))
p_bias_df <- data.frame(matrix(unlist(p), nrow=length(p), byrow=TRUE))

N_bias_df <- N_bias_df %>% mutate(percent_sites = rep(c(5,25,50,75,100),each=3)) %>%
  mutate(visits = rep(c(2,5,10), 5)) %>% melt(id = c('percent_sites', 'visits'), value.name = 'total')  

p_bias_df <- p_bias_df %>% mutate(percent_sites = rep(c(5,25,50,75,100),each=3)) %>%
  mutate(visits = rep(c(2,5,10), 5)) %>% melt(id = c('percent_sites', 'visits'), value.name = 'total')

library(ggplot2)
library(tidybayes)

#labeler for facets

facet_names <- c(
  `2` = "Two visits per colony",
  `5` = "Five visits per colony",
  `10` = "Ten visits per colony")

A <- N_bias_df %>%
  ggplot(aes(x = percent_sites, y = total, color=as.factor(percent_sites))) +
  facet_grid(cols = vars(visits), labeller = as_labeller(facet_names)) +
  stat_pointinterval(aes(color = as.factor(percent_sites)), alpha = 0.7, .width = c(0.5, 0.95)) +
  scale_color_manual(name="Percent of colonies visited", labels=c("5%", "25%", "50%", "75%", "100%"),
                     values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00'))+
  theme(#panel.spacing = unit(0, "lines"), 
  #       strip.background = element_blank(),
  #       strip.placement = "outside",
  #       axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
  axis.title.x=element_blank(),legend.position="bottom")+ylab("Bias in # Colonies Occupied") +
  geom_hline(yintercept=0,linetype=2)+
  xlab("")+
  guides(color=guide_legend(nrow=2,byrow=TRUE))
ggsave("results/figures/bias_N.jpg")

B <- p_bias_df %>%
  ggplot(aes(x = percent_sites, y = total, color=as.factor(percent_sites))) +
  facet_grid(cols = vars(visits), labeller = as_labeller(facet_names)) +
  stat_pointinterval(aes(color = as.factor(percent_sites)), alpha = 0.7, .width = c(0.5, 0.95)) +
  scale_color_manual(name="Percent of colonies visited", labels=c("5%", "25%", "50%", "75%", "100%"),
                     values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00'))+
  theme(#panel.spacing = unit(0, "lines"), 
    #       strip.background = element_blank(),
    #       strip.placement = "outside",
    #       axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),legend.position="bottom")+ylab("Bias in detection probability (p)") +
  geom_hline(yintercept=0,linetype=2)+
  xlab("")+
  guides(color=guide_legend(nrow=2,byrow=TRUE))
ggsave("results/figures/bias_p.jpg")


# write.csv(as.mcmc(out$summary),"Outputs/a_composite_model/jags_removals_ddirch_June24_2022_epsB.csv")
# saveRDS(out, file = "Outputs/a_composite_model/jags_removals_ddirch_June24_2022_epsB.rds")

scenarios_recov <- list()
for(i in 1:length(file_names)){
  load(file_names[[i]])
  scenarios_recov[[i]] <- list(NSite_state.mean, NSite_EWash.mean, NSite_NCasc.mean, NSite_SCasc.mean)
}

