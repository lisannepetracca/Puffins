#run NIMBLE!
Rmodel <- nimbleModel(code = simCode, 
                      constants = list(#area=3, 
                                       nyear=5, #index=c(rep(1,300),rep(2,300),rep(3,300)),
                                       N = nregions, nvisit = 5, #L = length(nbInfo$adj), 
                                       #adj = nbInfo$adj, weights = nbInfo$weights, num = nbInfo$num),
                                       cov = values$layer),
                      data = list(y=y),#,              
                      inits = list(#s= rep(0, 900), 
                        z=matrix(1, nrow=900, ncol=5), beta.cov=0, p=0.5, mean.psi=0.25, theta=0.6, sigma.beta.cov=0.5))
#Rmodel$calculate()

conf <- configureMCMC(Rmodel, monitors = c("p", "theta"))
Rmcmc <- buildMCMC(conf)
Cmodel <- compileNimble(Rmodel)
Cmcmc <- compileNimble(Rmcmc, project = Rmodel)

samples.first <- runMCMC(Cmcmc, niter=500, nburnin=100, nchains=2)#, nburnin=2000)
samplesSummary(samples.first)

library(MCMCvis)
MCMCsummary(samples.first, round = 2)



Cmodel$setData(y = y_sim[[15]])

#Cmodel$setInits(list(s= rep(0, 900), z=z_init[[15]]))

samples.second <- runMCMC(Cmcmc, niter=500, nburnin=100)
samplesSummary(samples.second)

y_sim[[14]]
