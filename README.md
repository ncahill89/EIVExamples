# EIVModels
This repository contains code to simulate from and run errors-in-variables simple regression and Gaussian process models (using JAGS). This code was written for the IGCP 725 Radiocarbon workshop. 

You will need to install the JAGS software in order for the code to run. You will can download from [here](https://sourceforge.net/projects/mcmc-jags/).

Download the repo and open the `EIV.Rproj` file in Rstudio. Then open the `main_script.R` to run the code. 

Required R packages are: 

  - R2jags
  - runjags
  - tidyverse
  - tidybayes
  - fields
  - mvtnorm

I have also created a shiny app for exploring EIV with simple linear regresion which you can find [here](https://niamhcahill.shinyapps.io/ExploreEIV/?_ga=2.38566883.1578472151.1642023320-1680552844.1641500119)
