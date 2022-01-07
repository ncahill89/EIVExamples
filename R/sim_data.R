
# Simulate from an EIV linear regression --------------------------------


sim_slr <- function(n_sim = 50,
                    alpha = 0,
                    beta = 1,
                    sigma = 0.1,
                    age_err = 50,
                    y_err = 0.1)
{

  sdobs <- age_err/1000
  taux <- 1 / (sdobs * sdobs)
  truex <- runif(n_sim, 0, 2)
  obsx <- rnorm(n_sim,truex,sdobs)
  errory <- rnorm(n_sim, 0, y_err)
  obsy <- alpha + beta*truex + errory
  parms <- data.frame(alpha, beta)

  dat = tibble(age = obsx*1000,
               age_err = age_err,
               y = obsy,
               y_err = y_err)
}


# Simulate from an EIV-GP regression --------------------------------

sim_gp <- function(n_sim = 50,
                   alpha = 0,
                   sigma_g = 2,
                   phi = 2,
                   sigma = 0.1,
                   age_err = 50,
                   y_err = 0.1)
{

true_year = sort(sample(0:2000, size = n_sim))

sim_eiv <- '
data{
  gp ~ dmnorm(mu,Sigma.inv)
  Sigma.inv <- inverse(Sigma)

  for(i in 1:n_sim)
  {
    mu[i] <- alpha
    Sigma[i,i] <- sigma_g^2 + 0.0001
    for(j in (i+1):n_sim) {
    Sigma[i,j] <- sigma_g^2*exp(-(phi^2)*((mu_x[i]-mu_x[j])^2))
    Sigma[j,i] <- Sigma[i,j]
    }

    y[i]~dnorm(gp[i],(sigma + y_err)^-2)
    x[i] ~ dnorm(mu_x[i],x_err^-2)

  }
  }
model{
fake <- 0
}
'

data<-list(n_sim = n_sim,
           mu_x = true_year/1000,
           alpha = alpha,
           sigma = sigma,
           sigma_g = sigma_g,
           phi = phi,
           x_err = age_err/1000,
           y_err = y_err)

out <- run.jags(sim_eiv,
                data = data,
                monitor=c("y","x","gp"),
                sample=1,
                n.chains=1,
                summarise=FALSE)


sim_dat <- coda::as.mcmc(out)

age <- as.vector(sim_dat)[(n_sim+1):(n_sim*2)]
y <- as.vector(sim_dat)[1:n_sim]
n_pred <- 50
n_obs <- n_sim
x_star <- age

###Predictive distribution for the GP
Sigma <-  sigma*2 * diag(n_obs) + sigma_g^2*exp(-(phi^2)*rdist(true_year/1000,true_year/1000)^2)
Sigma_star <- sigma_g^2*exp(-(phi^2)*rdist(x_star,true_year/1000)^2)
Sigma_star_star <- sigma_g^2*exp(-(phi^2)*rdist(x_star,x_star)^2)
pred_mean <- Sigma_star %*% solve(Sigma, y)

dat = tibble(age = age*1000,
             age_err = age_err,
             y = y,
             y_err = y_err,
             true_gp = c(pred_mean))
}
