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

sim_results <- read.csv(file = "~/Desktop/bart-sims/results/paper_high.csv")
npost = 2e3


xlims <- list("PAPER_10d" = c(.3,.8),
              "XOR_10d" = c(.0,.2),
              "Friedman_1" = c(2.45, 2.9))

ylims <- list("PAPER_10d" = c(0,300),
              "XOR_10d" = c(0,150),
              "Friedman_1" = c(0, 400))

for (dataset in unique(sim_results$Dataset)) {
  print(dataset)
  output_file = paste0("~/Desktop/bart-sims/figures/high_hists/paper_high",dataset,".png")

  sim_results %>%
    filter(Dataset == dataset) %>%
    mutate(Iteration = rep(1:npost, max(Chain))) %>%
    mutate(Chain = as.factor(Chain)) %>%
    filter(Iteration > 15e3) %>%
    ggplot(aes(x = RMSE, color = Chain))+
    geom_histogram(alpha = .3, bins = 150, position="identity")+
    labs(x = "Test Set RMSE")+
    xlim(xlims[[dataset]][1],xlims[[dataset]][2])+
    ylim(ylims[[dataset]][1],ylims[[dataset]][2])+
    theme_bw() -> p1


  ggsave(p1, filename = output_file, width = 6,height = 8)

}



sim_results <- read.csv(file = "~/Desktop/bart-sims/results/full_paper_high.csv")
for (dataset in unique(sim_results$Dataset)) {
  print(dataset)
  output_file = paste0("~/Desktop/bart-sims/figures/high_hists/full_paper_high",dataset,".png")

  sim_results %>%
    filter(Dataset == dataset) %>%
    mutate(Iteration = rep(1:npost, max(Chain))) %>%
    mutate(Chain = as.factor(Chain)) %>%
    filter(Iteration > 15e3) %>%
    ggplot(aes(x = RMSE, color = Chain))+
    geom_histogram(alpha = .3, bins = 150, position="identity")+
    labs(x = "Test Set RMSE")+
    xlim(xlims[[dataset]][1],xlims[[dataset]][2])+
    ylim(ylims[[dataset]][1],ylims[[dataset]][2])+
    theme_bw() -> p1


  ggsave(p1, filename = output_file, width = 6,height = 8)

}

# Now do same plots but for # Leaves ===========================================
sim_results <- read.csv(file = "~/Desktop/bart-sims/results/paper_high.csv")
npost = 20e3


xlims <- list("PAPER_10d" = c(.3,.8),
              "XOR_10d" = c(.0,.2),
              "Friedman_1" = c(2.45, 2.9))

ylims <- list("PAPER_10d" = c(0,300),
              "XOR_10d" = c(0,150),
              "Friedman_1" = c(0, 400))

for (dataset in unique(sim_results$Dataset)) {
  print(dataset)
  output_file = paste0("~/Desktop/bart-sims/figures/high_hists/paper_high_leaves",dataset,".png")

  n_bins <-   sim_results %>%
    filter(Dataset == dataset) %>%
    mutate(Iteration = rep(1:npost, max(Chain))) %>%
    mutate(Chain = as.factor(Chain)) %>%
    filter(Iteration > 15e3) %>%
    summarise(min = min(leaf_counts),max= max(leaf_counts))

  sim_results %>%
    filter(Dataset == dataset) %>%
    mutate(Iteration = rep(1:npost, max(Chain))) %>%
    mutate(Chain = as.factor(Chain)) %>%
    filter(Iteration > 15e3) %>%
    ggplot(aes(x = leaf_counts, color = Chain, fill = Chain))+
    geom_histogram(alpha = .3, position="stack", bins = n_bins$max-n_bins$min+2)+
    labs(x = "Number of Leaves")+
    # xlim(xlims[[dataset]][1],xlims[[dataset]][2])+
    # ylim(ylims[[dataset]][1],ylims[[dataset]][2])+
    theme_bw() -> p1


  ggsave(p1, filename = output_file, width = 6,height = 8)

}



sim_results <- read.csv(file = "~/Desktop/bart-sims/results/full_paper_high.csv")
for (dataset in unique(sim_results$Dataset)) {
  print(dataset)
  output_file = paste0("~/Desktop/bart-sims/figures/high_hists/full_paper_high_leaves",dataset,".png")

  n_bins <-   sim_results %>%
    filter(Dataset == dataset) %>%
    mutate(Iteration = rep(1:npost, max(Chain))) %>%
    mutate(Chain = as.factor(Chain)) %>%
    filter(Iteration > 15e3) %>%
    summarise(min = min(leaf_counts),max= max(leaf_counts))

  sim_results %>%
    filter(Dataset == dataset) %>%
    mutate(Iteration = rep(1:npost, max(Chain))) %>%
    mutate(Chain = as.factor(Chain)) %>%
    filter(Iteration > 15e3) %>%
    ggplot(aes(x = leaf_counts, color = Chain, fill = Chain))+
    geom_histogram(alpha = .3,position="stack", bins = n_bins$max-n_bins$min+2)+
    labs(x = "Number of Leaves")+
    # xlim(xlims[[dataset]][1],xlims[[dataset]][2])+
    # ylim(ylims[[dataset]][1],ylims[[dataset]][2])+
    theme_bw() -> p1


  ggsave(p1, filename = output_file, width = 6,height = 8)

}



# for (dataset in unique(sim_results$Dataset)) {
#   # output_file = paste0("~/Desktop/bart-sims/figures/new/grow_prune_bart_paper_",dataset,".pdf")
#   output_file = paste0("~/Desktop/bart-sims/figures/new/full_bart_paper_",dataset,".pdf")
#
#   sim_results %>%
#     filter(Dataset == dataset) %>%
#     mutate(Iteration = rep(1:npost, max(Chain))) %>%
#     mutate(Chain = as.factor(Chain)) %>%
#     ggplot(aes(x = Iteration, y = RMSE, color = Chain))+
#     geom_line(alpha = .3)+
#     labs(x = "Iteration", y = "Test Set RMSE")+
#     theme_bw() -> p1
#
#   sim_results %>%
#     filter(Dataset == dataset) %>%
#     mutate(Iteration = rep(1:npost, max(Chain))) %>%
#     mutate(Chain = as.factor(Chain)) %>%
#     ggplot(aes(x = Iteration, y = leaf_counts, color = Chain))+
#     geom_line(alpha = .3)+
#     labs(x = "Iteration", y ="Average Number of Leaves")+
#     theme_bw() -> p2
#
#   sim_results %>%
#     filter(Dataset == dataset) %>%
#     mutate(Iteration = rep(1:npost, max(Chain))) %>%
#     mutate(Chain = as.factor(Chain)) %>%
#     ggplot(aes(x = Iteration, y = pred1, color = Chain))+
#     geom_line(alpha = .3)+
#     labs(x = "Iteration", y ="Point Prediction at X_1 (Test set)")+
#     theme_bw() -> p3
#
#   sim_results %>%
#     filter(Dataset == dataset) %>%
#     mutate(Iteration = rep(1:npost, max(Chain))) %>%
#     mutate(Chain = as.factor(Chain)) %>%
#     ggplot(aes(x = Iteration, y = var_prop1, color = Chain))+
#     geom_line(alpha = .3)+
#     labs(x = "Iteration", y ="Proportion of Splits on Feature 1")+
#     theme_bw() -> p4
#   gridExtra::grid.arrange(p1,p2,p3,p4, ncol = 2, nrow = 2,
#                           top = textGrob(paste0("Data set: ",dataset),gp=gpar(fontsize=20,font=3))) -> p5
#
#   ggsave(p5, filename = output_file, width = 8,height = 10)
#
# }
