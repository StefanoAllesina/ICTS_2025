n <- 5
k <- sort(runif(n), decreasing = TRUE)
A <- (outer(rep(1, n), k) + outer(k, rep(1, n)))
A[upper.tri(A)] <- 0
diag(A) <- k


one <- outer(rep(1, n), k)
two <- outer(k, rep(1,n))
three <- outer(k, k)
four <- outer(k, 1/k)
five <- outer(1/k, k)
six <- outer(rep(1, n), 1/k)
seven <- outer(rep(1, n), 1/k)

findmatch <- function(tmp){
  Z <- A + one * tmp[1] + two * tmp[2] + three * tmp[3] + four * tmp[4]
  return(sum(Z^2))
}

optim(runif(4), findmatch, method = "BFGS")

tominimize <- function(pars){
  pars <- abs(pars)
  pars <- pars / max(pars)
  G <- diag(pars) %*% A
  G <- G + t(G)
  return(-min(eigen(G, symmetric = TRUE, only.values = TRUE)$values))
}

tmp <- list(par = runif(n))
tmp <- optim(tmp$par, fn = tominimize, method = "Nelder-Mead", control = list(maxit = 5000, trace = TRUE))
tmp <- optim(tmp$par, fn = tominimize, method = "BFGS", control = list(maxit = 5000, trace = TRUE))
sol <- abs(tmp$par)
sol <- sol / max(sol)
