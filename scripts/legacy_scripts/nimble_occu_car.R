#simulation stuff: https://r-nimble.org/nimbleExamples/simulation_from_model.html
#car stuff: https://r-nimble.org/nimbleExamples/CAR.html
#spatial with K Kellner https://kenkellner.com/blog/ubms-spatial.html

# Create BUGS model code with nimbleCode() describing the model
# Build the model with nimbleModel() providing constants and initial values for parameters needed for simulation
# Identify the nodes to simulate
# Run model$simulate()

#bringing in 1600 km2 grid from visualize.colonies.R

library(spdep)
library(raster)
TUPU_grid_1600km2

#let's make that grid smaller
grid_raster <- raster::raster(nrow=30, ncol=30, xmn=-2168649.7, xmx=2471350.3, ymn=-308459.1, ymx=2171540.9, crs=CRS("+init=epsg:3338"))

raster::values(grid_raster) <- 1
plot(grid_raster)
#ok let's get centroids and extract cov information
polygon <- rasterToPolygons(grid_raster, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)
polygon <- st_as_sf(polygon)

centroids <- st_centroid(polygon)
centroids_sp <- as(centroids,"Spatial")
values <- extract(grid_scale, centroids_sp, df=T)

C.nb <- poly2nb(polygon, row.names =  NULL)
## Determine neighborhood/adjacency information needed for neighborhood-based CAR model
nbInfo <- nb2WB(C.nb)

# A vector of indices indicating which regions are neighbors of which.
nbInfo$adj

# A vector of weights. In this case, all weights are 1.
head(nbInfo$weights)

# A vector of length N. num[n] indicates how many neighbors region n contains.
# This helps map the adj vector to the starting region.
nbInfo$num

#now we are ready to use dcar_normal in nimble
nregions <- nrow(as.data.frame(polygon))

#let's get covariate values for hab_suitablity given our grid
#simulate spp autocorrelated data
#http://www.eroubenoff.net/2021-03-04-spatial_sim/

library(nimble, warn.conflicts = FALSE)

simCode <- nimbleCode({
  
  # latent process (this is the CAR part!)
  #s[1:N] ~ dcar_normal(adj[1:L], weights[1:L], num[1:N], tau, zero_mean = 0) 
  
  #other stuff for car part
  #sigma ~ dunif(0, 100)   # prior for variance components based on Gelman (2006)
  #tau <- 1 / sigma^2
  
  #prior for trend term theta
  #for(a in 1:area){
    theta ~ dlogis(0,1)
  #}
  
  #prior for p
  p ~ dbeta(1,1)
  
  #prior for psi intercept


  mean.psi ~ dbeta(1,1) # prior for mean occupancy hypothetical wolf packs 
  logit.int <- log(mean.psi / (1-mean.psi))    # Logit transformation
  
  #beta on psi associated with spatial cov
  beta.cov ~ dnorm(0,sd=sigma.beta.cov)
  sigma.beta.cov ~ dexp(1)
  
  for (site in 1:N) {
    logit(psi[site,1]) <- logit.int + (beta.cov * cov[site]) 
    z[site,1] ~ dbern(psi[site,1]) # True occupancy status
    #s[site]
    y[site,1] ~ dbinom(z[site,1] * p, J) # Observed data
    
  for(year in 2:nyear){
    #
    # latent state model, has theta term now.
    #
    logit(psi[site,year]) <- logit.int + (beta.cov * cov[site]) + 
      #s[site] + 
      theta * z[site,year-1] #theta[index[site]]
    z[site,year] ~ dbern(psi[site,year])
    #
    # data model
    #
      y[site,year] ~ dbinom(z[site,year] * p, J) # Observed data
  }}
})


simModel <- nimbleModel(code = simCode,
                          constants = list(#area=3, 
                                           nyear=5, #index=c(rep(1,300),rep(2,300),rep(3,300)),
                                           N = nregions, J = 5, #L = length(nbInfo$adj), 
                                           #adj = nbInfo$adj, weights = nbInfo$weights, num = nbInfo$num),
                                           cov = values$layer),
                          inits = list(#s= rep(0, 900),
                                       p=0.7, 
                                       #sigma=0.5, 
                                       mean.psi=0.5, 
                                       beta.cov=2.5, sigma.beta.cov=1, 
                                       theta=0.75))

#The model$simulate() function allows you to specify which nodes you want to simulate. 
#Naively, we may want to go ahead and simulate the y nodes with simModel$simulate("y").
#However, since y depends on z, which has not been simulated, we’ll just get back a bunch of NAs again.

#We could get around this by simulating z before y. 
#In more complicated models, it can be useful to use the NIMBLE function model$getDependencies() 
#to gather all nodes needing to be simulated given the parameters we provided.

nodesToSim <- simModel$getDependencies(c("psi", "p", "theta"), #beta.cov
                                       self = T, downstream = T)

#By setting self = FALSE we specified that we didn’t want to see psi or p in the return vector. 
#By setting downstream = TRUE we recursively looked at the dependencies of nodes, rather than just the nodes 
#directly dependent on psi and p.

#We successfully retrieved all z and y nodes, as well as an internal set of nodes NIMBLE created to handle 
#the operation z[i] * p.

simModel$simulate(nodesToSim)

saveRDS(simModel, file = "data/simModel.rds")

simModel$p
simModel$beta.cov
simModel$psi
simModel$y #omg this is awesome
simModel$z
simModel$mean.psi

#ok now we create 15 sets of data: 

#now we make subsamples based on proportion sampled and number of visits
#5%, 25%, 50%, 75%, 100%
nsites <- c(45, 225, 450, 675, 900)
nvisits <- list(c(1,2),c(1,2,3),c(1:5)) 

y <- simModel$y
dim(y)

head(y)
head(y_sim[[15]])

y_sim <- my.sites <- my.visits <- list()
for(i in 1:length(nsites)){
  my.sites[[i]] <- sort(sample(c(1:900),nsites[i]))}

y_sim <- list()

my.sites.all <- rep(my.sites,each=3)
my.visits.all <- rep(nvisits,5)

for(i in 1:15){
  y_sim[[i]] <- matrix(NA, nrow=900, ncol=5) #should all have same dimension, just fill w NAs
  y_sim[[i]][my.sites.all[[i]],my.visits.all[[i]]] <- y[my.sites.all[[i]], my.visits.all[[i]]]
}

my.sites.all[[15]]
my.visits.all[[15]]
y_sim[[1]]
y_sim[[15]]

dim(y_sim[[1]])
y_sim[[1]][,5,]

z_init <- list()
for(i in 1:15){
  z_init[[i]] <- apply(y_sim[[i]], 1, 
              function(x) 
                ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))) 
z_init[[i]][z_init[[i]]>=0] <- 10
z_init[[i]][is.na(z_init[[i]])] <- 1
z_init[[i]][z_init[[i]]==10] <- NA
}

z_init[[15]]
y_sim[[13]]
z_init[[1]]


#running the simulated data
# simulatedYs <- CsimModel$y
# CsimModel$setData(list(y = simulatedYs))
# 
# simMCMC <- buildMCMC(CsimModel)
# 
# CsimMCMC <- compileNimble(simMCMC, project = simModel)
# 
# samples <- runMCMC(CsimMCMC, niter = 10000)
# 
# plot(samples[ , 'psi'], type = 'l', xlab = 'iteration',  ylab = expression(psi))
# 
# plot(samples[ , 'p'], type = 'l', xlab = 'iteration', ylab = expression(p))


