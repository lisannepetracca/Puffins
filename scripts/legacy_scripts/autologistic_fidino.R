# Step 1. Simulate the data.

# General bookkeeping
nsite <- 25
nyear <- 8
ncovar <- 3
nrepeats <- 5

# for covariates
X <- matrix(
  NA,
  ncol = ncovar,
  nrow = nsite
)

set.seed(333)
# Create covariates
X <- cbind(1,apply(X, 2, function(x) rnorm(nsite)))

# Occupancy coefficients, +1 for intercept
psi_betas <- rnorm(ncovar + 1)

# auto-logistic term
theta <- 0.75

# Detection coefficients, decreasing magnitude here
rho_betas <- rnorm(ncovar + 1, 0, 0.5)

# latent state, give same dimensions as X
z <- matrix(NA, ncol = nyear, nrow = nsite)

# Do first year occupancy
psi <- plogis(X %*% psi_betas)
z[,1] <- rbinom(nsite, 1, psi)

# And then the rest, which also uses the theta term
for(year in 2:nyear){
  psi <- plogis(X %*% psi_betas + theta * z[,year-1])
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
    nrepeats,
    rho
  ),
  ncol = nyear,
  nrow = nsite
)

#And now we have our simulated data y and covariates X that we can use to fit out model. In JAGS, the auto-logistic model can be written as:
  
  model{
    for(site in 1:nsite){
      # This is for the first year
      #
      # latent state model
      #
      logit(psi[site,1]) <- inprod(psi_beta, X[site,])
      z[site,1] ~ dbern(psi[site,1])
      #
      # data model
      #
      logit(rho[site,1]) <- inprod(rho_beta, X[site,])
      y[site,1] ~ dbin(rho[site,1] * z[site,1], J)
      #
      # For remaining years of sampling
      #
      for(year in 2:nyear){
        #
        # latent state model, has theta term now.
        #
        logit(psi[site,year]) <- inprod(psi_beta, X[site,]) +
          theta * z[site,year-1]
        z[site,year] ~ dbern(psi[site,year])
        #
        # data model
        #
        logit(rho[site,year]) <- inprod(rho_beta, X[site,])
        y[site,year] ~ dbin(rho[site,year] * z[site,year], J)
      }
    }
    #
    # Priors
    #
    # Intercept and slope terms
    for(covar in 1:ncovar){
      psi_beta[covar] ~ dlogis(0,1)
      rho_beta[covar] ~ dlogis(0,1)
    }
    # First-order autoregressive term
    theta ~ dlogis(0,1)
  }

# which, on the GitHub repository for this post can be found at ./jags/autologistic_model.R.
# 
# Fitting the model
# Now, we can fit the model in JAGS via the runjags package.

# data list for model
data_list <- list(
  J = nrepeats,
  nsite = nsite,
  nyear = nyear,
  X = X,
  ncovar = ncovar + 1, # for intercept
  y = y
)

# initial values
my_inits <- function(chain){
  gen_list <- function(chain = chain){
    list(
      z = matrix(1, ncol = data_list$nyear, nrow = data_list$nsite),
      psi_beta = rnorm(data_list$ncovar),
      rho_beta = rnorm(data_list$ncovar),
      theta = rnorm(1),
      .RNG.name = switch(chain,
                         "1" = "base::Wichmann-Hill",
                         "2" = "base::Wichmann-Hill",
                         "3" = "base::Super-Duper",
                         "4" = "base::Mersenne-Twister",
                         "5" = "base::Wichmann-Hill",
                         "6" = "base::Marsaglia-Multicarry",
                         "7" = "base::Super-Duper",
                         "8" = "base::Mersenne-Twister"),
      .RNG.seed = sample(1:1e+06, 1)
    )
  }
  return(switch(chain,
                "1" = gen_list(chain),
                "2" = gen_list(chain),
                "3" = gen_list(chain),
                "4" = gen_list(chain),
                "5" = gen_list(chain),
                "6" = gen_list(chain),
                "7" = gen_list(chain),
                "8" = gen_list(chain)
  )
  )
}

# fit the model
my_mod <- runjags::run.jags(
  model = "./jags/autologistic_model.R",
  monitor = c("theta", "psi_beta", "rho_beta"),
  data = data_list,
  inits = my_inits,
  n.chains = 4,
  adapt = 1000,
  burnin = 10000,
  sample = 10000,
  method = "parallel"
)

# model summary
my_sum <- summary(my_mod)