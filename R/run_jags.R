run_jags <- function(dat,
                     model = reg_model,
                     iter = 15000,
                     burnin = 5000,
                     thin = 5){

  ###The required data
  jags_data <- list(y = dat$y,
                    y_err = dat$y_err,
                    x = dat$age/1000,
                    x_err = dat$age_err/1000,
                    n_obs = nrow(dat),
                    n_pred = 50,
                    x_pred = seq(min(dat$age/1000), max(dat$age/1000),length.out = 50))

  ###Parameters to save
  if(model == gp_model|model == eiv_gp_model){
    jags_pars <-  c("alpha",
                    "phi",
                    "sigma_g",
                    "sigma",
                    "mu_x")}
  if(model == reg_model|model == eiv_reg_model){
    jags_pars <-  c("mu_pred",
                    "sl_pred",
                    "beta",
                    "alpha",
                    "sigma")}

  ###Run the model
  mod <- suppressWarnings(jags(data = jags_data,
              parameters.to.save=jags_pars,
              model.file = textConnection(model),
              n.iter = iter,
              n.burnin = burnin,
              n.thin = thin))


  ###Create an object containing the posterior samples
  m <- mod$BUGSoutput$sims.matrix


  return(list(m=m,
              sims_list = mod$BUGSoutput$sims.list,
              jags_data = jags_data))

}

reg_est <- function(mod)
{

  par_dat <- mod$m %>% spread_draws(alpha,beta) %>% median_qi(alpha, beta)

  return(par_dat)

}

gp_est <- function(mod)

{
  m <- mod$m

  sims_list <- mod$sims_list
  jags_data <- mod$jags_data

  n_pred <- 50
  n_obs <- jags_data$n_obs
  index <- 1:n_obs
  x_star <- seq(min(jags_data$x), max(jags_data$x), length.out = n_pred)
  par_dat <- m %>% spread_draws(sigma_g,phi, sigma,mu_x[index]) # posterior samples
  par_est<- par_dat %>%
    mean_qi(sigma_g, phi, sigma,mu_x) # posterior estimate for pars

  ###Predicitive distribution for the GP
  Sigma <-  unique(par_est$sigma)*2 * diag(n_obs) + unique(par_est$sigma_g)^2*exp(-(unique(par_est$phi)^2)*rdist(par_est$mu_x,par_est$mu_x)^2)
  Sigma_star <- unique(par_est$sigma_g)^2*exp(-(unique(par_est$phi)^2)*rdist(x_star,par_est$mu_x)^2)
  Sigma_star_star <- unique(par_est$sigma_g)^2*exp(-(unique(par_est$phi)^2)*rdist(x_star,x_star)^2)


  pred_mean <- Sigma_star %*% solve(Sigma, dat$y)
  pred_var <- Sigma_star_star - Sigma_star %*% solve(Sigma, t(Sigma_star))

  ###Store results
  pred_res <- tibble(pred_mean = c(pred_mean),
                     age = x_star*1000,
                     lwr_95 = pred_mean - 1.96 * sqrt(diag(pred_var)),
                     upr_95 = pred_mean + 1.96 * sqrt(diag(pred_var)))

return(pred_res)
}
