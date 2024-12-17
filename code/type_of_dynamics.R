set.seed(1)
#source("check_persistence.R")
#source("plot_dynamics.R")
library(deSolve)
library(tidyverse)
THRESH <- 10^-36

glv <- function(t, x, pars){
  x[x < THRESH] <- 0
  dx <- x * (pars$r + as.vector(pars$A %*% x))
  return(list(dx))
}

integration_time <- seq(0, 1, by = 0.01)
# single species
pars <- list(r = 1, A = matrix(-3))
out1.1 <- ode(y = 0.1, times = integration_time * 20, func = glv, parms = pars, method = "ode45")
out1.2 <- ode(y = 0.4, times = integration_time * 20, func = glv, parms = pars, method = "ode45")
pl1 <- plot_dynamics(out1.1, out1.2)
#show(pl1)

# two species
pars <- list(r = c(1,2), A = -matrix(c(1,4,5,1), 2,2 ))
out2.1 <- ode(y = c(0.4, 0.6), times = integration_time * 7, func = glv, parms = pars, method = "ode45")
out2.2 <- ode(y = c(0.7, 0.1), times = integration_time * 7, func = glv, parms = pars, method = "ode45")
pl2 <- plot_dynamics(out2.1, out2.2)
#show(pl2)

set.seed(1)
# find pars at random
tmp <- find_persistent(3)
integration_time <- seq(0, 1, by = 0.01)
pars <- list(r = tmp$r, A = tmp$A)
out3.1 <- ode(y = c(0.1, 0.1, 0.1), times = integration_time * 400, func = glv, parms = pars, method = "ode45")
out3.2 <- ode(y = c(0.1, 0.2, 0.3), times = integration_time * 400, func = glv, parms = pars, method = "ode45")
pl3 <- plot_dynamics(out3.1, out3.2)
#show(pl3)

# find pars at random
set.seed(130)
#tmp <- find_persistent(4)
#pars <- list(r = 10 * tmp$r, A = 10 * tmp$A)
r <- c(2.009609, 3.988181,15.071364, 5.699945)
A <- matrix(c(-5.287503,  -3.334398, -4.865576, -14.008396,
              -11.736001,  -9.391500, -11.943589, -3.101972,
              -1.013097,  -4.598932, -21.824857,  -5.745490,
              -2.183506,  -6.652185, -6.761757,-4.853617
),4,4)
integration_time <- seq(0, 500, by = 0.01)
pars <- list(r = r, A = A)
out4.1 <- ode(y = c(0.1, 0.1, 0.1, 0.1), times = integration_time, func = glv, parms = pars, method = "ode45")
out4.2 <- ode(y = c(0.1, 0.11, 0.11, 0.1), times = integration_time, func = glv, parms = pars, method = "ode45")
pl4 <- plot_dynamics(out4.1, out4.2)
#show(pl4)
#diff <- rowSums((out4.1 - out4.2)^2)
#plot(diff, type = "l")
# 
# ggsave(filename = "one.png", plot = pl1, width = 4, height = 3)
# ggsave(filename = "two.png", plot = pl2, width = 4, height = 3)
# ggsave(filename = "three.png", plot = pl3, width = 4, height = 3)
# ggsave(filename = "four.png", plot = pl4, width = 4, height = 3)
