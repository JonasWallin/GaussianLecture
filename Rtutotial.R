##
# exercises
# Fitting the lidar data using INLA
# https://github.com/timcdlucas/INLAutils
#
#
##
library(INLA)
library(SemiPar)
library(ggplot2)
data(lidar)
xx <- seq(720, 750, by = 5)
# Add data for prediction
new.data <- cbind(range = xx, logratio = NA)
new.data <- rbind(lidar, new.data)
lidar <- new.data
###
# using a rw1 model
# A non-stationary Gaussian proceses
# x_{t_A} \sim N(x_{t_B}, (t_A-t_B)/tau )
###
m.rw1 <- inla(logratio ~ -1 + f(range, model = "rw1", constr = FALSE),
              data = new.data, control.predictor = list(compute = TRUE))
##
# 
##
lidar$postmean  <- m.rw1$summary.fitted.values$mean
lidar$postLower <- m.rw1$summary.fitted.values$`0.025quant`
lidar$postUpper <- m.rw1$summary.fitted.values$`0.975quant`
p <- ggplot(data=lidar, aes(x = range, y = logratio)) + 
  geom_point() +
  geom_line(aes(x = range, y=postmean))+
    geom_ribbon(aes(x=range, ymin=postLower, ymax=postUpper), alpha=0.2)



####
# tutorial Binomial
# or Gaussian classifcation
####

## Load the data
data(Tokyo)

Tokyo.fit <- inla(y ~ -1 + f(time, model = "rw2", constr = FALSE), Ntrials = n,
                data = Tokyo, family ="binomial",
                control.predictor = list(compute = TRUE))


logit <- function(x){exp(x)/(1+exp(x))}
Tokyo$y_ratio   <- Tokyo$y/Tokyo$n
Tokyo$postmean  <- logit(Tokyo.fit$summary.linear.predictor$mean)
Tokyo$postLower <- logit(Tokyo.fit$summary.linear.predictor$`0.025quant`)
Tokyo$postUpper <- logit(Tokyo.fit$summary.linear.predictor$`0.975quant`)
p <- ggplot(data=Tokyo, aes(x = time, y = y_ratio)) + 
  geom_point() +
  geom_line(aes(x = time, y=postmean))+
  geom_ribbon(aes(x=time, ymin=postLower, ymax=postUpper), alpha=0.2)

print(p)