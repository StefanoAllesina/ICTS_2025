n <- 50
k <- sort(runif(n), decreasing = TRUE)
A <- (outer(rep(1, n), k) + outer(k, rep(1, n)))
A[upper.tri(A)] <- 0
diag(A) <- k

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
