library(dbarts)
library(dplyr)
library(ggplot2)
library(Metrics)
library(tidyverse)
library(optparse)
library(coda)
library(gridExtra)
library(ggridges)
library(latex2exp) 
library(scales)
library(stringr)

source("code/bart_helpers.R")
source("code/ds_reg.R")
source("code/sim_helpers.R")

TEXT_SIZE  <- 20


first_split_df <- function(ds_name, n,p, n_tree, nskip, ndpost, nchain, synthetic, run, restricted){
  data_all <- get_data(ds_name = ds_name, n = n, p = p, synthetic=synthetic, seed = 42)
  data_train <- data_all[["train"]]
  data_test <- data_all[["test"]]
  y.train <- get.labels(data_train)
  if (n == Inf) {
    n_label = "All data points"
  } else{
    n_label <- paste0(length(y.train), " data points")
  }
  n <- length(y.train)
  dir_data <- .get_dir_data(n_tree, ds_name, restricted)
  dir_fig <- .get_dir_fig(n_tree, ds_name, restricted)
  
  fname <- .get_fname(nskip = nskip,ndpost = ndpost, n=n, nchain = nchain, synthetic = synthetic,p=p, run=run)

  plots_data <- .get_plots_data(data_train, data_test,n_tree, nskip, ndpost, nchain, fname, dir_data, run,restricted)
  
  split_data <- plots_data$split_mat

  split_data$Chain <- factor(split_data$Chain)
  split_data$var[split_data$var == -1] <- "Empty Tree" 
  
  split_data$Feature <- factor(split_data$var, levels = c(1:9, "Empty Tree"))
  split_data$Dataset <- .get.label.name(ds_name)
  split_data$n_obs <- n_label

  return(split_data)
}

plot_first_splits <- function(ds_names, ns, p, n_tree, nskip, ndpost, nchain,
                              synthetic, restricted, runs = NULL,
                              legend_pos = "top", save = TRUE) {

  n_dfs <- length(ds_names) * length(ns)
  dfs <- vector(mode = "list", length = n_dfs)
  i <- 1

  for (ds_name in ds_names) {
    for (n in ns) {
      if (is.null(runs)) {
        runs <- list(
          "california_housing" = 3,
          "breast_tumor" = 2
        )
      }
      if (ds_name %in% names(runs)) {
        run <- runs[[ds_name]]
      } else {
        stop(paste("specify what run to use", ds_name, "in runs arg"))
      }

      dfs[[i]] <- first_split_df(ds_name = ds_name, n = n,
                                 p = p, n_tree = n_tree,
                                 nskip = nskip, ndpost = ndpost,
                                 nchain = nchain,
                                 synthetic = synthetic,
                                 run = run,
                                 restricted = restricted)
      i <- i + 1
    }
  }

  split_data <- bind_rows(dfs)

  gg <- ggplot(split_data, aes(x=Chain, fill=Feature)) +
    geom_histogram(stat="count") + ylab(y_lab) +
    xlab("Chain Number") +
    facet_grid(rows = vars(Dataset), cols = vars(n_obs)) +
    scale_fill_brewer(palette = "Pastel1") +
    theme_minimal() +
    theme(text = element_text(size = TEXT_SIZE),
          legend.position = legend_pos,
          legend.text = element_text(size = TEXT_SIZE - 6),
          axis.text.x = element_text(size=TEXT_SIZE - 6),
          axis.text.y = element_text(size=TEXT_SIZE - 6))

  if (save) {

    dir_split <- file.path("results","figures","first_split")
    .check_create(dir_split)

    fname_pre <- paste0(ds_names, collapse = "_")
    fname <- paste(fname_pre, n, "tree", n_tree,"split", sep="_")
    if (restricted){
      fname <- paste(fname, "restricted", sep="_")
    }

    fname_suf <- paste0(fname, ".pdf")
    print(file.path(dir_split,fname_suf))
    ggsave(file.path(dir_split,fname_suf), plot = gg,
           device = "pdf", width = 6, height = 6,
           dpi=300, bg = "white")
  }

  return(gg)
}

ds_names <- c("california_housing", "breast_tumor")
runs <- list("california_housing" = 3,
             "breast_tumor" = 2)
ns <- c(200, Inf)
p <- 1
n_tree <- 1
nskip <- 5000
ndpost <- 1000
nchain <- 8
legend_pos <- "top"
y_lab <- "Count"
save <- TRUE

## restricted = FALSE
plot_first_splits(ds_names = ds_names, ns = ns, p = p, n_tree = n_tree,
                  nskip = nskip, ndpost = ndpost, nchain = nchain,
                  synthetic = FALSE, restricted = FALSE, runs = runs,
                  legend_pos = legend_pos, save = save)

## restricted = TRUE
plot_first_splits(ds_names = ds_names, ns = ns, p = p, n_tree = n_tree,
                  nskip = nskip, ndpost = ndpost, nchain = nchain,
                  synthetic = FALSE, restricted = TRUE, runs = runs,
                  legend_pos = legend_pos, save = save)
