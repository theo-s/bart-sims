library(Rforestry)
library(ranger)
library(glmnet)
library(grf)
library(tidyverse)
library(reshape)
library(Cubist)
library(caret)
library(clustermq)
library(foreach)
library(dplyr)
library(doParallel)
library(grid)
library(gridExtra)

# Now do same plots but for split distributions ================================
sim_results <- read.csv(file = "~/Desktop/bart-sims/results/high_splits.csv")
npost = 20e3


xlims <- list("PAPER_10d" = c(.3,.8),
              "XOR_10d" = c(.0,.2),
              "Friedman_1" = c(2.45, 2.9))

ylims <- list("PAPER_10d" = c(0,300),
              "XOR_10d" = c(0,150),
              "Friedman_1" = c(0, 400))

for (dataset in unique(sim_results$Dataset)) {
  print(dataset)
  output_file = paste0("~/Desktop/bart-sims/figures/split_hists/high_splits",dataset,".png")

  sim_results %>%
    filter(Dataset == dataset) %>%
    mutate(Iteration = rep(1:npost, max(Chain))) %>%
    mutate(Chain = as.factor(Chain)) %>%
    filter(Iteration > 15e3) %>%
    ggplot(aes(x = first_split_var, color = Chain, fill = Chain))+
    geom_bar(alpha = .3)+
    labs(x = "First Feature Tree Splits on", y = "Number of MCMC Samples after burn in")+
    # xlim(xlims[[dataset]][1],xlims[[dataset]][2])+
    # ylim(ylims[[dataset]][1],ylims[[dataset]][2])+
    theme_bw() -> p1


  ggsave(p1, filename = output_file, width = 6,height = 8)

}



sim_results <- read.csv(file = "~/Desktop/bart-sims/results/full_high_splits.csv")
for (dataset in unique(sim_results$Dataset)) {
  print(dataset)
  output_file = paste0("~/Desktop/bart-sims/figures/split_hists/full_high_splits",dataset,".png")

  sim_results %>%
    filter(Dataset == dataset) %>%
    mutate(Iteration = rep(1:npost, max(Chain))) %>%
    mutate(Chain = as.factor(Chain)) %>%
    filter(Iteration > 15e3) %>%
    ggplot(aes(x = first_split_var, color = Chain, fill = Chain))+
    geom_bar(alpha = .3,position="stack")+
    labs(x = "First Feature Tree Splits on", y = "Number of MCMC Samples after burn in")+
    # xlim(xlims[[dataset]][1],xlims[[dataset]][2])+
    # ylim(ylims[[dataset]][1],ylims[[dataset]][2])+
    theme_bw() -> p1


  ggsave(p1, filename = output_file, width = 6,height = 8)

}

