library(tidyverse)
source("cc_functions.R")
source("distribution_definitions.R")
nsim <- 500000

results <- tibble()
for (n in c(5, 10, 100, 1000, 2500)){
  for (dist in c("uniform", "exponential")){
    print(c(n, dist))
    tmp <- tibble("n" = n, "dist" = dist, k = rep(0, nsim))
    for (i in 1:nsim){
      if (i %% 1000 == 0) print(i)
      if (dist == "uniform") tmp$k[i] <- length(get_attractor(sort(sample_uniform(n = n, m = 1), decreasing = TRUE), m = 1))
      if (dist == "exponential") tmp$k[i] <- length(get_attractor(sort(sample_exponential(n = n, m = 1), decreasing = TRUE), m = 1))
    }
    tmp <- tmp %>% group_by(k, dist, n) %>% summarise(freq = n() / nsim, .groups = "drop")
    results <- rbind(results, tmp)
  }
}

results <- results %>% mutate(expected = exp(lchoose(n-1, k-1) - (n-1) * log(2)))

write_csv(results, file = "res_CC.csv")
