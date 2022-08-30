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

sim_results <- read.csv(file = "~/Desktop/bart-sims/results/coverage.csv")
npost = 5e3


xlims <- list("PAPER" = c(.25,.325),
              "XOR" = c(.0,.05),
              "Friedman_1" = c(1.8, 2.5))

ylims <- list("PAPER" = c(0,400),
              "XOR" = c(0,250),
              "Friedman_1" = c(0, 800))

for (dataset in unique(sim_results$Dataset)) {
  output_file = paste0("~/Desktop/bart-sims/figures/sig_hists/sig_",dataset,".png")

  sim_results %>%
    filter(Dataset == dataset) %>%
    mutate(Iteration = rep(1:npost, max(Chain))) %>%
    mutate(Chain = as.factor(Chain)) %>%
    ggplot(aes(x = sigma, color = Chain))+
    geom_histogram(alpha = .3, bins = 150, position="identity")+
    labs(x = "Posterior Variance Estimate")+
    # xlim(xlims[[dataset]][1],xlims[[dataset]][2])+
    # ylim(ylims[[dataset]][1],ylims[[dataset]][2])+
    theme_bw() -> p1


  ggsave(p1, filename = output_file, width = 6,height = 8)

}

for (dataset in unique(sim_results$Dataset)) {
  output_file = paste0("~/Desktop/bart-sims/figures/sig_hists/rmse_",dataset,".png")

  sim_results %>%
    filter(Dataset == dataset) %>%
    mutate(Iteration = rep(1:npost, max(Chain))) %>%
    mutate(Chain = as.factor(Chain)) %>%
    ggplot(aes(x = RMSE, color = Chain))+
    geom_histogram(alpha = .3, bins = 150, position="identity")+
    labs(x = "Test Set RMSE")+
    # xlim(xlims[[dataset]][1],xlims[[dataset]][2])+
    # ylim(ylims[[dataset]][1],ylims[[dataset]][2])+
    theme_bw() -> p1


  ggsave(p1, filename = output_file, width = 6,height = 8)

}

