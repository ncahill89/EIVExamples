
# Simple Linear Regression Model ------------------------------------------


reg_model <-
  'model{
  ## Data model loop
  for(j in 1:n_obs)
  {
  y[j]~dnorm(mu_y[j],(sigma + y_err[j])^-2)
  mu_y[j] <- alpha + beta*(x[j])
  } # end j loop

 ## Priors
 alpha ~ dnorm(0.0,0.01)
 beta ~ dnorm(0.0,0.01)
 sigma ~ dt(0,4^-2,1)T(0,)

 for(i in 1:n_pred) {mu_pred[i] = alpha + beta*x_pred[i]}

 sl_pred ~ dnorm(alpha + beta*2, sigma^-2)
}# end
'

# EIV Linear Regression Model ------------------------------------------

eiv_reg_model <-
  'model{
  ## Data model loop
  for(j in 1:n_obs)
  {
  y[j]~dnorm(mu_y[j],(sigma + y_err[j])^-2)
  x[j] ~ dnorm(mu_x[j],x_err[j]^-2)
  mu_x[j] ~ dnorm(1,0.5^-2)
  mu_y[j] <- alpha + beta*(mu_x[j])
  } # end j loop

 ## Priors
 alpha ~ dnorm(0.0,0.01)
 beta ~ dnorm(0.0,0.01)
 sigma ~ dt(0,4^-2,1)T(0,)

 for(i in 1:n_pred) {mu_pred[i] = alpha + beta*x_pred[i]}

 sl_pred ~ dnorm(alpha + beta*2, sigma^-2)
}# end
'

# Gaussian Process Regression Model ------------------------------------------


gp_model <- '
model{

  gp ~ dmnorm(mu,Sigma.inv)
  Sigma.inv <- inverse(Sigma)
  mu_x <- x
  for(i in 1:n_obs)
  {
    mu[i] <- alpha
    Sigma[i,i] <- sigma_g^2 + 0.0001
    for(j in (i+1):n_obs) {
    Sigma[i,j] <- sigma_g^2*exp(-(phi^2)*((x[i]-x[j])^2))
    Sigma[j,i] <- Sigma[i,j]
    }

    y[i]~dnorm(gp[i],(sigma + y_err[i])^-2)

  }


  sigma_g ~ dt(0,10^-2,1)T(0,)
  phi ~ dt(0,4^-2,1)T(0,)
  sigma ~ dt(0,10^-2,1)T(0,)
  alpha ~ dnorm(0,0.01)
}

'

# EIV Gaussian Process Regression Model ------------------------------------------

eiv_gp_model <- '
model{

  gp ~ dmnorm(mu,Sigma.inv)
  Sigma.inv <- inverse(Sigma)

  for(i in 1:n_obs)
  {
    mu[i] <- alpha
    Sigma[i,i] <- sigma_g^2 + 0.0001
    for(j in (i+1):n_obs) {
    Sigma[i,j] <- sigma_g^2*exp(-(phi^2)*((mu_x[i]-mu_x[j])^2))
    Sigma[j,i] <- Sigma[i,j]
    }

    y[i]~dnorm(gp[i],(sigma + y_err[i])^-2)
    x[i] ~ dnorm(mu_x[i],x_err[i]^-2)
    mu_x[i] ~ dnorm(1,0.5^-2)

  }


  sigma_g ~ dt(0,10^-2,1)T(0,)
  phi ~ dt(0,4^-2,1)T(0,)
  sigma ~ dt(0,10^-2,1)T(0,)
  alpha ~ dnorm(0,0.01)
}

'
