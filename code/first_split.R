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


TEXT_SIZE  <- 22
DATASETS <- c("california_housing", "breast_tumor", "echo_months", "satellite_image")
DATASETS_MAIN <- c("california_housing", "breast_tumor")
DATASETS_APDX <- c("echo_months", "satellite_image")
COLORS <- c( "violet","royalblue", "forestgreen", "yellow3")
COLORS_MAIN <- c("violet", "royalblue")
COLORS_APDX <- c("forestgreen", "yellow3")


.get_ds_clr <- function(ds_name){
  if (ds_name =="california_housing"){
    return("Blues")
  }
  if (ds_name == "breast_tumor"){
    return("Purples")
  }
  if (ds_name == "echo_months"){
    return("Greens")
  }
  if (ds_name == "satellite_image"){
    return("YlOrBr")
  }
}
#DATASETS <- c("breast_tumor", "echo_months", "satellite_image")#, "breast_tumor")


# ds_name <- "california_housing"
# n <- 200
# # p <- 6
# n_tree <- 1
# nskip <- 5000
# ndpost <- 1000
# nchain <- 8
# p <- 1
# run <- 1
# # alpha <- 0.05
# # plot_type <- "rmse"
# synthetic <- FALSE
# restricted <- F
cumsum_rmse_plot <- function(ds_name, n,p, n_tree, nskip, ndpost, nchain,add_legend,y_lab, synthetic, run, restricted, plot=FALSE){
  data_all <- get_data(ds_name = ds_name, n = n, p = p, synthetic=synthetic, seed = 42)
  data_train <- data_all[["train"]]
  data_test <- data_all[["test"]]
  y.train <- get.labels(data_train)
  n <- length(y.train)
  dir_data <- .get_dir_data(n_tree, ds_name, restricted)
  dir_fig <- .get_dir_fig(n_tree, ds_name, restricted)
  
  fname <- .get_fname(nskip = nskip,ndpost = ndpost, n=n, nchain = nchain, synthetic = synthetic, p=p, run=run)
  
  plots_data <- .get_plots_data(data_train, data_test,n_tree, nskip, ndpost,
                                nchain, fname, dir_data, run=run, restricted=restricted)
  rmse_mat <- plots_data$rmse_mat
  rmse_mat[, 1:nchain] <- sapply(rmse_mat[, 1:nchain], function(x) cumsum(scale(x, scale=FALSE)))
  
  colnames(rmse_mat) <- c(sprintf("%01d", seq(1,ncol(rmse_mat)-1)), "Sample")
  rmse_mat_wide <- as.data.frame(rmse_mat) %>% pivot_longer(sprintf("%01d", seq(1,ncol(rmse_mat)-1)), names_to = "Chain", values_to = "RMSE")
  
  x_lab <- paste("Sample")
  pos <- ifelse(add_legend, "right", "none")
  gg <-   ggplot(rmse_mat_wide, aes(x = Sample, y = RMSE, color = Chain)) + geom_line() +
    xlab(x_lab)+ theme_classic()   +
    theme(text = element_text(size = TEXT_SIZE),axis.text.x= element_text(size=TEXT_SIZE),
          axis.text.y= element_text(size=TEXT_SIZE), legend.position = pos) +
    ggtitle(paste0("Dataset: ", .get.label.name(ds_name), "\nn:", n)) + ylab(y_lab)
  
  if (!is.null(fname)){
    dir_fig <- file.path("results", "figures")
    dir_rmse <- file.path(dir_fig, "cusum")
    .check_create(dir_rmse)
    fname <- paste(ds_name, n, "tree", n_tree, sep="_")
    fname_suf <- paste0(fname, ".png")
    print(file.path(dir_rmse,fname_suf))
    ggsave(file.path(dir_rmse,fname_suf), plot = gg, dpi=300, bg = "white")
  }
}

coverage_plot <- function(ds_name, n,p, n_tree, nskip, ndpost, nchain,add_legend,y_lab, synthetic, run, restricted, plot=FALSE){
  data_all <- get_data(ds_name = ds_name, n = n, p = p, synthetic=synthetic, seed = 42)
  data_train <- data_all[["train"]]
  data_test <- data_all[["test"]]
  y.train <- get.labels(data_train)
  n <- length(y.train)
  add_legend <- F
  # y_lab <- "Density"
  dir_data <- .get_dir_data(n_tree, ds_name, restricted)
  dir_fig <- .get_dir_fig(n_tree, ds_name, restricted)
  fname <- .get_fname(nskip = nskip,ndpost = ndpost, n=n,
                      nchain = nchain, synthetic = synthetic, p=p, run=run)
  
  
  coverage_data <- .get_coverage_data(data_train, data_test,n_tree, nskip,
                                      ndpost, nchain, fname, dir_data, run=run,
                                      restricted=restricted)
  if (!plot){
    fname <- NULL}
  
  if (!is.null(fname)){
    gg <- coverage_data  %>%  ggplot(aes(x=RMSE, y=Coverage, size=Length)) + geom_point() + theme_minimal() +
      ggtitle(paste0("Dataset: ", .get.label.name(ds_name), "\nn: ", n))
    
    dir_fig <- file.path("results", "figures")
    dir_coverage <- file.path(dir_fig, "coverage")
    .check_create(dir_coverage)
    fname <- paste(ds_name, n, "tree", n_tree, sep="_")
    fname_suf <- paste0(fname, ".png")
    print(file.path(dir_coverage,fname_suf))
    ggsave(file.path(dir_coverage,fname_suf), plot = gg, dpi=300, bg = "white")
  }
  
  
}

rmse_plot <- function(ds_name, n,p, n_tree, nskip, ndpost, nchain,add_legend,y_lab, synthetic, run, restricted, plot=FALSE){
  data_all <- get_data(ds_name = ds_name, n = n, p = p, synthetic=synthetic, seed = 42)
  data_train <- data_all[["train"]]
  data_test <- data_all[["test"]]
  y.train <- get.labels(data_train)
  n <- length(y.train)
  add_legend <- F
  # y_lab <- "Density"
  dir_data <- .get_dir_data(n_tree, ds_name, restricted)
  dir_fig <- .get_dir_fig(n_tree, ds_name, restricted)
  fname <- .get_fname(nskip = nskip,ndpost = ndpost, n=n,
                      nchain = nchain, synthetic = synthetic, p=p, run=run)
  
  
  plots_data <- .get_plots_data(data_train, data_test,n_tree, nskip,
                                ndpost, nchain, fname, dir_data, run=run,
                                restricted=restricted)
  if (!plot){
    fname <- NULL}
  rmse_mat <- plots_data$rmse_mat
  
  gelman <- .get_gelman_rubin(rmse_mat[, 1:nchain])[[1]][1]
  if (!is.null(fname)){
    colnames(rmse_mat) <- c(sprintf("%01d", seq(1,ncol(rmse_mat)-1)), "Sample")
    rmse_mat_wide <- as.data.frame(rmse_mat) %>% pivot_longer(sprintf("%01d", seq(1,ncol(rmse_mat)-1)), names_to = "Chain", values_to = "RMSE")
    rmse_mean <- mean(rmse_mat_wide$RMSE)
    # rmse_mat_wide$RMSE <- (rmse_mat_wide$RMSE/rmse_mean - 1) * 100
    # x_lab <- paste("% Change to Mean RMSE")
    x_lab <- "RMSE"
    colors <- c()
    x_lim <- quantile(rmse_mat_wide$RMSE, c(0.01, 0.99))
    pos <- ifelse(add_legend, "right", "none")
    gg <-   ggplot(rmse_mat_wide, aes(x = RMSE, y = Chain, fill = Chain)) +
      geom_density_ridges() +
      #
      # geom_density_ridges(aes(point_color = "grey27", point_fill = "grey27"),
      #                     alpha = .2, point_alpha = 1, jittered_points = TRUE)+
      scale_fill_brewer(palette = .get_ds_clr(ds_name))+
      # scale_fill_manual(values = c(
      #  "grey",
      #   "grey",
      #   "black",
      #    "grey",
      #    "grey",
      #    "grey",
      #    "black",
      #    "grey"
      # ))+
      xlab(x_lab) + expand_limits(y=0)+ theme_classic()   +
      theme(text = element_text(size = TEXT_SIZE),  axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),axis.text.x= element_text(size=18), legend.position = pos) +
      ggtitle(paste0("Dataset: ", .get.label.name(ds_name), "\nn: ", n,
                     "\nGelman Rubin: ", round(gelman, 4))) + ylab(y_lab) +xlim(x_lim[1], x_lim[2])
    # ggtitle(paste0("Dataset: ", .get.label.name(ds_name), " (n=", n, ")",
    #                "\nGelman Rubin: ", round(gelman, 4),
    #                "\nMean RMSE: ", round(rmse_mean, 2))) + ylab(y_lab) +xlim(x_lim[1], x_lim[2])
    
    dir_fig <- file.path("results", "figures")
    dir_rmse <- file.path(dir_fig, "rmse")
    .check_create(dir_rmse)
    fname <- paste(ds_name, n, "tree", n_tree, sep="_")
    fname_suf <- paste0(fname, ".png")
    print(file.path(dir_rmse,fname_suf))
    ggsave(file.path(dir_rmse,fname_suf), plot = gg, dpi=300, bg = "white")
  }
  return(list(gelman=gelman, n=n))
}

first_split_plot <- function(ds_name, n,p, n_tree, nskip, ndpost, nchain,add_legend,y_lab,synthetic, run, restricted, plot=FALSE){
  data_all <- get_data(ds_name = ds_name, n = n, p = p, synthetic=synthetic, seed = 42)
  data_train <- data_all[["train"]]
  data_test <- data_all[["test"]]
  y.train <- get.labels(data_train)
  n <- length(y.train)
  dir_data <- .get_dir_data(n_tree, ds_name, restricted)
  dir_fig <- .get_dir_fig(n_tree, ds_name, restricted)
  
  
  fname <- .get_fname(nskip = nskip,ndpost = ndpost, n=n, nchain = nchain, synthetic = synthetic,p=p, run=run)
  
  plots_data <- .get_plots_data(data_train, data_test,n_tree, nskip, ndpost, nchain, fname, dir_data, run,restricted)
  if (!plot){
    fname <- NULL}
  
  split_data <- plots_data$split_mat
  
  
  pos <- ifelse(add_legend, "right", "none")
  
  n_samples <- nskip+ndpost
  
  chain_flips <- c()
  
  for (c in 1:nchain){
    data_chain <- split_data %>% filter(Chain == c)
    
    var_i <- data_chain$var[1:(n_samples-1)]
    var_i_p_1 <- data_chain$var[2:n_samples]
    chain_flips <- c(chain_flips, mean(var_i_p_1 != var_i))
    
  }
  if (!is.null(fname)){
    
    split_data$Chain <- factor(split_data$Chain)
    split_data$var[split_data$var == -1] <- "Empty Tree" 
    
    split_data$Feature <- factor(split_data$var)
    gg <- ggplot(split_data, aes(x=Chain, fill=Feature)) +
      geom_histogram(stat="count") + ylab(y_lab) +
      xlab("Chain Number")+
      ggtitle(paste0("Dataset: ", .get.label.name(ds_name), "\nn: ", n))+
      scale_fill_brewer(palette = "Pastel1")+
      theme_minimal() + theme(text = element_text(size = TEXT_SIZE),
                              legend.position = pos, axis.text.x= element_text(size=TEXT_SIZE),
                              axis.text.y= element_text(size=TEXT_SIZE)) 
    
    # gg <- split_data %>% 
    #   group_by(chain, var) %>% 
    #   summarise(Count = n())%>%
    #   mutate(freq = Count / sum(Count)) %>% ggplot(aes(x=chain, y=var, size=freq, color=freq)) + geom_point() +
    #   ylab(y_lab) + xlab("Chain") + 
    # theme_minimal() + theme(text = element_text(size = TEXT_SIZE),
    #                         legend.position = pos, axis.text.x= element_text(size=TEXT_SIZE),
    #                         axis.text.y= element_text(size=TEXT_SIZE)) +
    #   guides(size=guide_legend(title="Frequency"), color="none") +
    #   scale_colour_gradient2(low='white', mid="grey", high="black")+ 
    #   scale_size(range = c(0,7), limits = c(0,1))+
    #   ggtitle(paste0("Dataset: ", .get.label.name(ds_name), " (n=", n, ")"))
    # 
    dir_split <- file.path("results","figures","first_split")
    # dir_split <- file.path(dir_fig, "first_split")
    .check_create(dir_split)
    fname <- paste(ds_name, n, "tree", n_tree,"split", sep="_")
    if (restricted){
      fname <- paste(fname, "restricted", sep="_")
    }
    
    fname_suf <- paste0(fname, ".png")
    print(file.path(dir_split,fname_suf))
    ggsave(file.path(dir_split,fname_suf), plot = gg, dpi=300, bg = "white")
  }
  return(chain_flips)
}

.get_gr_df_dp <- function(args, n_tree, restricted){
  # n_tree <- args$n_tree
  nskip <- args$n_burn
  ndpost <- args$post
  nchain <- args$n_chains
  synthetic <- args$synthetic
  # restricted <- args$restricted
  
  p <- 1
  
  
  # datasets <- c("california_housing", "echo_months", "satellite_image")#, "breast_tumor")
  n_dps <- c(200, 2000, Inf)
  df <- data.frame(matrix(ncol = 4, nrow = 0))
  
  add_leg <- TRUE
  for (ds_name in DATASETS){
    
    data_fir <- .get_dir_data(n_tree = n_tree, ds_name = ds_name, restricted=restricted)
    for (n_dp in n_dps){
      n  <- .get.real.n(ds_name = ds_name, n = n_dp)
      
      gelman_vec <- c()
      for (run in 1:20){
        fname <-  file.path(data_fir, paste0(.get_fname(nskip, ndpost, n,nchain,p,run,  synthetic), "_rmse.csv"))
        if (file.exists(fname)){
          y_lab_mse <- ifelse(n==200, "Density", "")
          rmse_plt <- rmse_plot(ds_name = ds_name, n = n,p = p, n_tree = n_tree
                                , nskip = nskip, ndpost = ndpost, nchain=nchain,
                                add_legend = add_leg,y_lab = y_lab_mse, synthetic = FALSE, run=run, restricted=restricted)
          gelman_vec <- c(gelman_vec, rmse_plt$gelman)
          
          y_lab_split <- ifelse(n==200, "Root Split Variable", "")
          
          first_split_plot(ds_name = ds_name, n = n,p = p, n_tree = n_tree
                           , nskip = nskip, ndpost = ndpost, nchain=nchain,
                           add_legend = add_leg,y_lab = y_lab_split, synthetic = FALSE, run=run, restricted=restricted)
          add_leg <- FALSE
        }
        
      }
      sd_gr <- 1.96 * (sd(gelman_vec) / sqrt(length(gelman_vec)))
      
      df <- rbind(df, c(mean(gelman_vec), sd_gr, n, .get.label.name(ds_name)))
      
    }
  }
  
  
  colnames(df) <- c("GR", "sd", "data_points", "Dataset")
  df$GR <- as.numeric(df$GR)
  df$sd <- as.numeric(df$sd)
  
  df$data_points <- as.numeric(df$data_points)
  df$Dataset <- factor(df$Dataset)
  return(df)
  
}

.get_gr_df_features <- function(args, n_tree, restricted){
  # n_tree <- args$n_tree
  nskip <- args$n_burn
  ndpost <- args$post
  nchain <- args$n_chains
  synthetic <- args$synthetic
  # restricted <- args$restricted
  
  n_dp <- Inf
  
  
  # datasets <- c("california_housing", "echo_months", "satellite_image")#, "breast_tumor")
  ps <- c(1, 2, 5)
  df <- data.frame(matrix(ncol = 4, nrow = 0))
  
  add_leg <- TRUE
  for (ds_name in DATASETS){
    
    data_fir <- .get_dir_data(n_tree = n_tree, ds_name = ds_name, restricted=restricted)
    n  <- .get.real.n(ds_name = ds_name, n = n_dp)
    
    for (p in ps){
      
      gelman_vec <- c()
      for (run in 1:20){
        fname <-  file.path(data_fir, paste0(.get_fname(nskip, ndpost, n,nchain,p,run,  synthetic), "_rmse.csv"))
        if (file.exists(fname)){
          y_lab_mse <- ifelse(n==200, "Density", "")
          rmse_plt <- rmse_plot(ds_name = ds_name, n = n,p = p, n_tree = n_tree
                                , nskip = nskip, ndpost = ndpost, nchain=nchain,
                                add_legend = add_leg,y_lab = y_lab_mse, synthetic = FALSE, run=run, restricted=restricted)
          gelman_vec <- c(gelman_vec, rmse_plt$gelman)
          
          y_lab_split <- ifelse(n==200, "Root Split Variable", "")
          
          first_split_plot(ds_name = ds_name, n = n,p = p, n_tree = n_tree
                           , nskip = nskip, ndpost = ndpost, nchain=nchain,
                           add_legend = add_leg,y_lab = y_lab_split, synthetic = FALSE, run=run, restricted=restricted)
          add_leg <- FALSE
        }
        
      }
      sd_gr <- 1.96 * (sd(gelman_vec) / sqrt(length(gelman_vec)))
      df <- rbind(df, c(mean(gelman_vec), sd_gr, p, .get.label.name(ds_name)))
      
    }
  }
  
  
  colnames(df) <- c("GR", "sd", "features", "Dataset")
  df$GR <- as.numeric(df$GR)
  df$sd <- as.numeric(df$sd)
  
  df$features <- as.numeric(df$features)
  df$Dataset <- factor(df$Dataset)
  return(df)
  
} 

first_split_plot <- function(ds_name, n,p, n_tree, nskip, ndpost, nchain,add_legend,y_lab,synthetic, run, restricted, plot=FALSE){
  data_all <- get_data(ds_name = ds_name, n = n, p = p, synthetic=synthetic, seed = 42)
  data_train <- data_all[["train"]]
  data_test <- data_all[["test"]]
  y.train <- get.labels(data_train)
  n <- length(y.train)
  dir_data <- .get_dir_data(n_tree, ds_name, restricted)
  dir_fig <- .get_dir_fig(n_tree, ds_name, restricted)
  
  
  fname <- .get_fname(nskip = nskip,ndpost = ndpost, n=n, nchain = nchain, synthetic = synthetic,p=p, run=run)
  
  plots_data <- .get_plots_data(data_train, data_test,n_tree, nskip, ndpost, nchain, fname, dir_data, run,restricted)
  if (!plot){
    fname <- NULL}
  
  split_data <- plots_data$split_mat
  
  
  pos <- ifelse(add_legend, "right", "none")
  
  n_samples <- nskip+ndpost
  
  chain_flips <- c()
  
  for (c in 1:nchain){
    data_chain <- split_data %>% filter(Chain == c)
    
    var_i <- data_chain$var[1:(n_samples-1)]
    var_i_p_1 <- data_chain$var[2:n_samples]
    chain_flips <- c(chain_flips, mean(var_i_p_1 != var_i))
    
  }
  if (!is.null(fname)){
    
    split_data$Chain <- factor(split_data$Chain)
    split_data$var[split_data$var == -1] <- "Empty Tree" 
    
    split_data$Feature <- factor(split_data$var)
    gg <- ggplot(split_data, aes(x=Chain, fill=Feature)) +
      geom_histogram(stat="count") + ylab(y_lab) +
      xlab("Chain Number")+
      ggtitle(paste0("Dataset: ", .get.label.name(ds_name), "\nn: ", n))+
      scale_fill_brewer(palette = "Pastel1")+
      theme_minimal() + theme(text = element_text(size = TEXT_SIZE),
                              legend.position = pos, axis.text.x= element_text(size=TEXT_SIZE),
                              axis.text.y= element_text(size=TEXT_SIZE)) 
    
    # gg <- split_data %>% 
    #   group_by(chain, var) %>% 
    #   summarise(Count = n())%>%
    #   mutate(freq = Count / sum(Count)) %>% ggplot(aes(x=chain, y=var, size=freq, color=freq)) + geom_point() +
    #   ylab(y_lab) + xlab("Chain") + 
    # theme_minimal() + theme(text = element_text(size = TEXT_SIZE),
    #                         legend.position = pos, axis.text.x= element_text(size=TEXT_SIZE),
    #                         axis.text.y= element_text(size=TEXT_SIZE)) +
    #   guides(size=guide_legend(title="Frequency"), color="none") +
    #   scale_colour_gradient2(low='white', mid="grey", high="black")+ 
    #   scale_size(range = c(0,7), limits = c(0,1))+
    #   ggtitle(paste0("Dataset: ", .get.label.name(ds_name), " (n=", n, ")"))
    # 
    dir_split <- file.path("results","figures","first_split")
    # dir_split <- file.path(dir_fig, "first_split")
    .check_create(dir_split)
    fname <- paste(ds_name, n, "tree", n_tree,"split", sep="_")
    if (restricted){
      fname <- paste(fname, "restricted", sep="_")
    }
    
    fname_suf <- paste0(fname, ".png")
    print(file.path(dir_split,fname_suf))
    ggsave(file.path(dir_split,fname_suf), plot = gg, dpi=300, bg = "white")
  }
  return(chain_flips)
}

first_split_plot(ds_name = ds_name, n = n,p = p, n_tree = n_tree
                 , nskip = nskip, ndpost = ndpost, nchain=nchain,
                 add_legend = add_legend,y_lab = y_lab, synthetic = synthetic, run=run, restricted=restricted, plot=T)}