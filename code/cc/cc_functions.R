# Email: zrmiller@illinois.edu
# March 2022

### Core simulation functions 

get_attractor <- function(c.vec, # vector of colonization rates
                          m, # common local extinction rate
                          ordered = FALSE, # are the colonization rates ordered increasing? (sort them by default)
                          return_bounds = FALSE # also return the sequence of l threshold values
                          ){
  
  # get colonization rates corresponding to species in the attractor using
  # recursive formula c_i > l_i = c_(i-1)^2 / l_(i-1)
  
  if(!ordered) c.vec <- sort(c.vec) # order c's if necessary
  c.vec <- c.vec[c.vec > m] # remove species with colonization rates less than m
  
  n <- length(c.vec) # maximum number of potential survivors
  survivors <- vector(length = n) # initialize vector of survivors
  counter <- 1 # initialize counter
  l <- m # bound for invasion
  
  if(return_bounds) l_vec <- l # initialize a vector of threshold (l) values if requested
  
  for(i in 1:n){ # loop over all species in the pool
    if(c.vec[i] > l){ # if c_i > l_i, species i can invade
      c <- c.vec[i] 
      survivors[counter] <- c # add to vector of survivors
      l <- (c^2) / l # update threshold
      
      if(return_bounds) l_vec <- append(l_vec, l) # add to vector of l values
      
      counter <- counter + 1 # update counter
    }
  }
  
  survivors <- survivors[survivors > 0] # remove any extra slots preallocated
  if(return_bounds) return(list(c = survivors, l = l_vec)) else return(survivors)
}


find_eq_occupancies <- function(c.vec, # vector of colonization rates
                                m, # common local extinction rate
                                ordered = FALSE # are the colonization rates ordered increasing? (sort them by default)
                                ){
  
  # get equilibrium occupancies using Eq. 7 from Tilman (1994)
  # note: this function can return negative values corresponding to unfeasible equilibrium
  #       values are only meaningful if c.vec is a coexisting set (e.g. output of get_attractor)
  
  if(!ordered) c.vec <- sort(c.vec) # order c's if necessary
  
  n <- length(c.vec) # number of species
  p <- rep(0, n) # initialize vector of frequencies
  
  for(i in 1:n){ # apply recursive formula for equilibrium occupancies
    p[i] <- 1 - sum(p) - m / c.vec[i] - sum(c.vec * p) / c.vec[i]
  }
  
  return(p)
}


get_approximate_attractor <- function(c.vec, # vector of colonization rates
                                      m, # common local extinction rate
                                      ordered = FALSE, # are the colonization rates ordered increasing? (sort them by default)
                                      return_bounds = FALSE # also return the sequence of l values
){
  
  # exactly as "get_attractor", except using the linear niche shadow approximation for lower bounds, l 
  
  if(!ordered) c.vec <- sort(c.vec) # order c's if necessary
  c.vec <- c.vec[c.vec > m] # remove species with colonization rates less than m
  
  n <- length(c.vec) # maximum number of potential survivors
  survivors <- vector(length = n) # initialize vector of survivors
  counter <- 1 # initialize counter
  l <- m # bound for invasion
  
  if(return_bounds) l_vec <- l # initialize a vector of threshold (l) values if requested
  
  for(i in 1:n){ # loop over all species in the pool
    if(c.vec[i] > l){ # if c_i > l_i, species i can invade
      c <- c.vec[i] 
      survivors[counter] <- c # add to vector of survivors
      l <- l + 2 * (c - l) # update threshold
      
      if(return_bounds) l_vec <- append(l_vec, l)  # add to vector of l values
      
      counter <- counter + 1 # update counter
    }
  }
  
  survivors <- survivors[survivors > 0] # remove any extra slots preallocated
  if(return_bounds) return(list(c = survivors, l = l_vec)) else return(survivors)
}