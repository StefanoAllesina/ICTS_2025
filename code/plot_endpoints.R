library(tidyverse)
library(ggpubr)
dt <- read_csv(file = "data/Sunderhauf_fig4a.csv")

plot_endpoint_file <- function(fn, label = "test") {
  # add community
  cm <- apply(dt %>% as.matrix() > 0, 1,
              function(x)
                paste(colnames(dt)[x], collapse = ""))
  dt <-
    dt %>% add_column(community = cm) %>% 
    mutate(experiment = row_number())
  dt %>%
    pivot_longer(
      names_to = "species",
      values_to = "n",
      cols = -c(community, experiment)
    ) %>%
    filter(n > 0) %>% ggplot(aes(x = species, y = n, fill = species)) +
    geom_point() + geom_violin(alpha = 0.5) +
    facet_wrap( ~ community) + scale_y_log10() +
    theme_bw() + theme(legend.position = "bottom") +
    ggtitle(label)
}

plot_meanvar <- function(fn, label = "test") {
  # add community
  cm <- apply(dt %>% as.matrix() > 0, 1,
              function(x)
                paste(colnames(dt)[x], collapse = ""))
  dt <-
    dt %>% add_column(community = cm) %>% 
    mutate(experiment = row_number())
  dt %>%
    pivot_longer(
      names_to = "species",
      values_to = "n",
      cols = -c(community, experiment)
    ) %>%
    filter(n > 0) %>% group_by(community, species) %>% 
    summarise(mean = mean(n), sd = var(n)) %>% 
    ggplot(aes(x = mean, y = sd, colour = species)) + 
    geom_point() + geom_smooth(method = "lm") + scale_x_log10() + scale_y_log10() + 
    facet_wrap(~species, scales = "free") + 
    stat_cor(method = "pearson",
             label.x.npc = "left",
             label.y = 0) + 
    theme_bw()
}

