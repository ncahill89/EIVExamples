plot_results <- function(mod1,
                         mod2 = NULL,
                         dat,
                         alpha = NULL,
                         beta = NULL)
{

  ### format data for plotting with errors
  data_to_plot <- dat_to_plot(dat)

### get model estimates
muy_dat1 <- mod1$m %>%
  spread_draws(mu_pred[1:m1$jags_data$n_pred]) %>%
  median_qi(mu_pred) %>%
  mutate(age = mod1$jags_data$x_pred*1000) %>%
  mutate(mu_y = mu_pred)

muy_dat1$mod <- ""
muy_dat <- muy_dat1

### same set up if second model is included
if(!is.null(mod2))
{
muy_dat2 <- mod2$m %>%
  spread_draws(mu_pred[1:m2$jags_data$n_pred]) %>%
  median_qi(mu_pred) %>%
  mutate(age = mod2$jags_data$x_pred*1000) %>%
  mutate(mu_y = mu_pred)

muy_dat1$mod <- "SLR"
muy_dat2$mod <- "EIV-SLR"

muy_dat <- full_join(muy_dat1,muy_dat2)

}

### plot the results
p <- ggplot(data = muy_dat) +
  geom_line(aes(x = age, y = mu_y, colour = mod)) +
  geom_ribbon(aes(x = age, ymin = .lower, ymax = .upper, fill = mod), alpha = 0.4) +
  geom_point(data = dat, aes(x = age, y = y), alpha = 0.2) +
  geom_polygon(aes(x = x, y = y,group = obs_index),data = data_to_plot,alpha = 0.2) +
  ylab("y") +
  xlab("Age") +
  labs(fill = "", colour = "") +
  theme_classic()

### add true line if indicated
if(!is.null(alpha))
  return(p +  geom_abline(aes(intercept=alpha, slope=beta/1000), color="green", size=1.5, linetype="dashed"))

if(is.null(alpha)) return(p)

}

plot_results_gp <- function(mod1,
                            mod2 = NULL,
                            dat)
{
  ### format data for plotting with errors
  data_to_plot <- dat_to_plot(dat)

  ## get model based estimates
  pred_res <- gp_est(mod1)
  pred_res$mod <- ""

  ### same set up if second model is included
  if(!is.null(mod2))
  {
  pred_res1 <- pred_res
  pred_res2 <- gp_est(mod2)
  pred_res1$mod <- "GP"
  pred_res2$mod <- "EIV-GP"
  pred_res <- full_join(pred_res1, pred_res2)
  }

  ### plot the results
  ggplot(pred_res, aes(x = age, y = pred_mean)) +
    geom_line(aes(colour = mod)) +
    geom_ribbon(aes(ymin = lwr_95, ymax = upr_95, fill = mod), alpha = 0.4) +
    geom_point(data = dat, aes(x = age, y = y),colour = "black", alpha = 0.2) +
    geom_polygon(aes(x = x, y = y,group = obs_index),data = data_to_plot,alpha = 0.2) +
    labs(y = "y", colour = "", fill = "") +
    theme_classic()


}



