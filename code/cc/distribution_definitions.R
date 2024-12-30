# Email: zrmiller@illinois.edu
# March 2022

### Distributions to sample from 

# Uniform with lower limit = m and upper limit = m + 1
sample_uniform <- function(n, m){
  return(runif(n, min = m, max = m + 1))
}

# Exponential with rate = 1, truncated (shifted) at m
sample_exponential <- function(n, m){
  return(rexp(n, rate = 1) + m)
}

# Pareto with alpha = 2 and lower limit = m
sample_pareto <- function(n, m){
  # sample using inverse transform sampling
  raw <- runif(n)
  transformed <- m / (raw^(1/2))
  return(transformed)
}

# Triangular distribution with mode and upper limit at m + 1
sample_triangle <- function(n, m){
  # sample using inverse transform sampling
  raw <- runif(n)
  transformed <- sqrt(raw) + m
  return(transformed)
}