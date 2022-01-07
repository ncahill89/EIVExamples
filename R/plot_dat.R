dat_to_plot <- function(dat){

  dat = dat %>%
    mutate(y_1_lwr = y - y_err,
           y_2_lwr = y - y_err,
           y_3_upr = y + y_err,
           y_4_upr = y + y_err,
           x_1_upr = age + age_err,
           x_2_lwr = age - age_err,
           x_3_lwr = age - age_err,
           x_4_upr = age + age_err)


get_bounds <- dat %>%
  select(y_1_lwr:x_4_upr) %>%
  mutate(obs_index = 1:n()) %>%
  pivot_longer(cols = y_1_lwr:x_4_upr,
               names_to = "bounds",
               values_to = "value") %>%
  mutate(bounds = replace(bounds, bounds %in% c("y_1_lwr","y_2_lwr","y_3_upr","y_4_upr"), "y"),
         bounds = replace(bounds, bounds %in% c("x_1_upr","x_2_lwr","x_3_lwr","x_4_upr"), "x"))

x_bounds <- get_bounds %>%
  filter(bounds == "x")

y_bounds <- get_bounds %>%
  filter(bounds == "y")

data_to_plot <- tibble(obs_index = x_bounds$obs_index,
                       x = x_bounds$value,
                       y = y_bounds$value)

return(data_to_plot)
}

plot_dat <- function(dat,
                     alpha = NULL,
                     beta = NULL)
{

  data_to_plot <- dat_to_plot(dat)
  p <- ggplot(data_to_plot, aes(x = x, y = y))+
    geom_polygon(aes(group = obs_index, fill = "1-sigma error"),alpha = 0.3) +
    geom_point(data = dat, aes(x = age, y = y), alpha = 0.6, pch = 1) +
    xlab("Age") + ylab("Y") +
    scale_fill_manual(values="gray",name = "") +
    theme_classic()


  if(!is.null(alpha))
 return(p +  geom_abline(aes(intercept=alpha, slope=beta/1000), color="green", size=1.5, linetype="dashed"))

  if(is.null(alpha)) return(p)
}
