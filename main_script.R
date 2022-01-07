
devtools::load_all()

# Linear regression -------------------------------------------------------

### simulate Data
dat <- sim_slr(n_sim = 30,
               alpha = 0,
               beta = 1,
               age_err = 200)

### plot the data + true regression line
plot_dat(dat,
         alpha = 0,
         beta = 1) # exclude alpha & beta arguments if you don't want to plot the true line

### run the simple regression model
mod1 <- run_jags(dat,
                 model = reg_model)

### get model estimates
reg_est(mod = mod1)

### plot regression line + truth
plot_results(mod1,
             dat = dat,
             alpha = 0,
             beta = 1)

### run the EIV-regression model
mod2 <- run_jags(dat,
                 model = eiv_reg_model)

### get model estimates
reg_est(mod = mod2)

### plot regression line + truth
plot_results(mod2,
             dat = dat,
             alpha = 0,
             beta = 1)

### compare reg with EIV-reg
plot_results(mod1,
             mod2,
             dat,
             alpha = 0,
             beta = 1)


# Gaussian Processes ------------------------------------------------------


### simulate data
dat <- sim_gp(n_sim = 10,
              phi = 4,
              sigma_g = 2,
              age_err = 200)

### plot the data and true underlying process
p <- plot_dat(dat)
p + geom_line(data = dat, aes(x = age, y = true_gp),color="green", size=1.5, linetype="dashed")


### run the GP model
mod1 <- run_jags(dat,
         model = gp_model)

### get model estimates
pred_res1 <- gp_est(mod = mod1)

### plot GP results + truth
p1 <- plot_results_gp(mod1,
                     dat = dat)
p1 + geom_line(data = dat, aes(x = age, y = true_gp),color="green", size=1.5, linetype="dashed")

### run the EIV-GP model
mod2 <- run_jags(dat,
               model = eiv_gp_model)

### get model estimates
pred_res2 <- gp_est(mod = mod1)

### plot EIV-GP results + Truth
p2 <- plot_results_gp(mod2,
                     dat = dat)
p2 + geom_line(data = dat, aes(x = age, y = true_gp),color="green", size=1.5, linetype="dashed")


## compare GP with EIV-GP
p_compare <- plot_results_gp(mod1,
                     mod2,
                     dat = dat)
p_compare + geom_line(data = dat, aes(x = age, y = true_gp),color="green", size=1.5, linetype="dashed")

