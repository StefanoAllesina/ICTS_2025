set.seed(1)
n <- 3

success <- FALSE
while(!success){
  A <- matrix(sample(-5:5, n * n, replace = TRUE), n, n)
  diag(A) <- -abs(diag(A))
  eA <- eigen(A, only.values = TRUE, symmetric = FALSE)$values
  if (max(Re(eA)) < -0.01){
    for (i in 1:100){
      x <- sample(1:4, n)
      B <- diag(x) %*% A
      eB <- eigen(B, only.values = TRUE, symmetric = FALSE)$values
      if (max(Re(eB)) > 0.01){
        success <- TRUE
        break
      }
    }
  }
}