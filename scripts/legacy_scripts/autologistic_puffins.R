library(nimble)
library(MCMCvis)

# Step 1. Simulate the data.

# General bookkeeping
nsite <- 900
nyear <- 5
ncov <- 2
nvisits <- 5

# for covariates
X <- matrix(
  NA,
  ncol = ncov,
  nrow = nsite
)

set.seed(24)
# Create covariates
X <- cbind(1,apply(X, 2, function(x) rnorm(nsite)))

# Occupancy coefficients, +1 for intercept
psi_betas <- rnorm(ncov + 1)

# auto-logistic term
theta[1] <- 0.75
theta[2] <- 0
theta[3] <- -0.75
index <- c(rep(1,300),rep(2,300),rep(3,300))

# Detection coefficients, decreasing magnitude here
rho_betas <- rnorm(ncov + 1, 0, 0.5)

# latent state, give same dimensions as X
z <- matrix(NA, ncol = nyear, nrow = nsite)

# Do first year occupancy
psi <- plogis(X %*% psi_betas)
z[,1] <- rbinom(nsite, 1, psi)


# And then the rest, which also uses the theta term
for(year in 2:nyear){
  psi <- plogis(X %*% psi_betas + theta[index] * z[,year-1])
  z[,year] <- rbinom(nsite,1,psi)
}

# Add imperfect detection, make it a matrix with
#  the same dimensions as z. Then multiply by z.
rho <- matrix(
  plogis(X %*% rho_betas),
  ncol = nyear,
  nrow = nsite
) * z

# Create the observed data. Again, same dimensions as z.
y <- matrix(
  rbinom(
    length(rho),
    nvisits,
    rho
  ),
  ncol = nyear,
  nrow = nsite
)

#And now we have our simulated data y and covariates X that we can use to fit out model. 

simCode <- nimbleCode({
  for(site in 1:nsite){
    # This is for the first year
    #
    # latent state model
    #
    logit(psi[site,1]) <- inprod(psi_beta[1:3], X[site,])
    z[site,1] ~ dbern(psi[site,1])
    #
    # data model
    #
    logit(rho[site,1]) <- inprod(rho_beta[1:3], X[site,])
    y[site,1] ~ dbin(rho[site,1] * z[site,1], J)
    #
    # For remaining years of sampling
    #
    for(year in 2:nyear){
      #
      # latent state model, has theta term now.
      #
      logit(psi[site,year]) <- inprod(psi_beta[1:3], X[site,]) +
        theta[index[1:900]] * z[site,year-1]
      z[site,year] ~ dbern(psi[site,year])
      #
      # data model
      #
      logit(rho[site,year]) <- inprod(rho_beta[1:3], X[site,])
      y[site,year] ~ dbin(rho[site,year] * z[site,year], J)
    }
  }
  #
  # Priors
  #
  # Intercept and slope terms
  for(covar in 1:ncov){
    psi_beta[covar] ~ dlogis(0,1)
    rho_beta[covar] ~ dlogis(0,1)
  }
  # First-order autoregressive term
  for(i in 1:3){
    theta[i] ~ dlogis(0,1)}
})

# Fitting the model

simModel <- nimbleModel(code = simCode,
                        constants = list(#area=3, 
                          nsite=nsite, X=X, ncov= ncov+1, nyear=nyear,
                          #N = nregions, 
                          J = nvisits), #L = length(nbInfo$adj), 
                          #adj = nbInfo$adj, weights = nbInfo$weights, num = nbInfo$num),
                          #cov = values$layer),
                        data = list(y=y),
                        inits = list(#s= rep(0, 900),
                            z = matrix(1, ncol = nyear, nrow = nsite),
                            psi_beta = rnorm(3),
                            rho_beta = rnorm(3),
                            theta = rnorm(3)))                          
                          # p=0.7, 
                          # #sigma=0.5, 
                          # mean.psi=0.5, 
                          # beta.cov=2.5, sigma.beta.cov=1, 
                          # theta=0.75))

conf <- configureMCMC(simModel, monitors = c("theta", "psi_beta", "rho_beta"))
Rmcmc <- buildMCMC(conf)
Cmodel <- compileNimble(simModel)
Cmcmc <- compileNimble(Rmcmc, project = simModel)

samples.first <- runMCMC(Cmcmc, niter=3000, nburnin=100, nchains=2)#, nburnin=2000)
MCMCsummary(samples.first, round = 2)
