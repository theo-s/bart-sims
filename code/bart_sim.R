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



option_list = list(
  make_option(c("-d", "--dataset"), type="character", default="abalone",
              help="dataset name", metavar="character"),
  make_option(c("-t", "--plot_type"), type="character", default="all",
              help="plot type (rmse, converage or all)", metavar="character"),
  make_option(c( "--synthetic"), action="store_true",default=FALSE,
              help="make y synthetic", metavar="store_true"),
  make_option(c( "--restricted"), action="store_true",default=FALSE,
              help="use only grow and prune", metavar="store_true"),
  make_option(c("-n", "--n_d_p"), type="double", default=Inf,
              help="number of data points", metavar="double"),
  make_option(c("-p", "--n_features"), type="integer", default=1,
              help="number of feature", metavar="integer"),
  make_option(c("-b", "--n_burn"), type="integer", default=5000,
              help="number of burn-in", metavar="integer"),
  make_option(c("-c","--n_chains"), type="integer", default=8,
              help="number of chains", metavar="integer"),
  make_option(c("--post"), type="integer", default=1000,
              help="number of posterior samples", metavar="integer"),
  make_option(c("--n_tree"), type="integer", default=1,
              help="number of trees", metavar="integer"),
  make_option(c("-a", "--alpha"), type="double", default=0.05,
              help="alpha", metavar="double"),
  make_option(c("-r", "--run"), type="integer", default=1,
              help="run", metavar="integer")
);
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
    
    split_data$Variable <- factor(split_data$var)
    gg <- ggplot(split_data, aes(x=Chain, fill=Variable)) +
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

.plot_features <- function(df, leg_loc, fname, clrs, n_tree, y_lab=""){
  gg <- df %>% ggplot(aes(x=features, y=GR, color=Dataset))  +   geom_line(size=1.4) +
    geom_point(size=4) +   geom_errorbar(aes(ymin=GR-sd, ymax=GR+sd), width=0.2,size=1.2,
                                         position=position_dodge(0.05)) +
    theme_minimal() + xlab(TeX(r'($d/d_0$)')) + ylab(y_lab) +
    geom_hline(yintercept=1.1, linetype='dashed', col = 'grey27', size=1, alpha=0.7)+
    theme(text = element_text(size = TEXT_SIZE),  legend.position = leg_loc,
          axis.text.x= element_text(size=TEXT_SIZE),axis.text.y= element_text(size=TEXT_SIZE))  +
    scale_x_continuous(breaks = c(1, 2, 5)) +     scale_colour_manual(values=clrs)
  dir_gr <- file.path("results", "figures", paste("trees",n_tree, sep="_"))
  .check_create(dir_gr)
  ggsave(file.path(dir_gr,fname), plot = gg, dpi=300, bg ="white")
  
}


gr_plot_features <- function(args){
  n_tree <- args$n_tree
  df_fname <- file.path("results", paste0("gr_p_trees_",n_tree,".csv"))
  if (file.exists(df_fname)){
    df <- read.csv(df_fname)
  } else {
  df_bart <- .get_gr_df_features(args, 200, F)
  df_bart$Model <- "BART"
  
  df_smpl <- .get_gr_df_features(args, 1, T)
  df_smpl$Model <- "Simplified BART"
  
  df  <- rbind(df_smpl, df_bart)
  df$Model <- factor(df$Model)
  write.csv(df, df_fname)}
  

  df$ds_lower <- str_replace(tolower(df$Dataset), " ", "_")
  
  
  df_bart_main <- df %>% filter(Model=="BART", ds_lower %in% DATASETS_MAIN)
  .plot_features(df=df_bart_main, leg_loc = "none", fname = "gr_p_bart_main.png", clrs = COLORS_MAIN, n_tree=n_tree, y_lab="Gelman-Rubin")
  
  
  df_bart_apdx <- df %>% filter(Model=="BART", ds_lower %in% DATASETS_APDX)
  .plot_features(df=df_bart_apdx, leg_loc = "none", fname = "gr_p_bart_apdx.png", clrs = COLORS_APDX, n_tree=n_tree, y_lab="Gelman-Rubin")
  
  df_sbart_main <- df %>% filter(Model=="Simplified BART", ds_lower %in% DATASETS_MAIN)
  .plot_features(df=df_sbart_main, leg_loc = "none", fname = "gr_p_sbart_main.png", clrs = COLORS_MAIN, n_tree=n_tree)
  
  
  df_sbart_apdx <- df %>% filter(Model=="Simplified BART", ds_lower %in% DATASETS_APDX)
  .plot_features(df=df_sbart_apdx, leg_loc = "none", fname = "gr_p_sbart_apdx.png", clrs = COLORS_APDX, n_tree=n_tree)
  
  
  # gg <- df %>% ggplot(aes(x=features, y=GR, color=Dataset))  +   geom_line(aes(linetype=Model), size=1.2) +
  #   geom_point(size=4) +   geom_errorbar(aes(ymin=GR-sd, ymax=GR+sd), width=1,
  #                                        position=position_dodge(0.05)) +
  #   theme_minimal() + xlab(TeX(r'($d/d_0$)')) + ylab("Gelman-Rubin") +
  #   geom_hline(yintercept=1.1, linetype='solid', col = 'red4', size=1, alpha=0.3)+
  #   # annotate("text", x = 3.5, y = 1.11, label = "Convergence\nThreshold", vjust = -0.5)+
  #   theme(text = element_text(size = TEXT_SIZE),  legend.position = "none",
  #         axis.text.x= element_text(size=TEXT_SIZE),axis.text.y= element_text(size=TEXT_SIZE))  +
  #   scale_x_continuous(breaks = c(1, 2, 5)) +     scale_colour_manual(values=COLORS)
  # dir_gr <- file.path("results", "figures", paste("trees",n_tree, sep="_"))
  # .check_create(dir_gr)
  # ggsave(file.path(dir_gr,"gr_p.png"), plot = gg, dpi=300, bg ="white")



}


.plot_dp <- function(df, leg_loc, fname, clrs, n_tree, y_lab=""){
  gg <- df %>% ggplot(aes(x=data_points, y=GR, color=Dataset))  +   geom_line(size=1.4) +
    geom_point(size=4) +   geom_errorbar(aes(ymin=GR-sd, ymax=GR+sd), width=0.2,size=1.4,
                                         position=position_dodge(0.05)) +
    theme_minimal() + xlab("Number of Data Points") + ylab(y_lab) +
    geom_hline(yintercept=1.1, linetype='dashed', col = 'grey27', size=1, alpha=0.7)+
    theme(text = element_text(size = TEXT_SIZE), legend.position = leg_loc) + scale_colour_manual(values=clrs) +
    scale_x_continuous(trans = 'log10',
                       breaks = trans_breaks('log10', function(x) 10^x),
                       labels = trans_format('log10', math_format(10^.x)))
  dir_gr <- file.path("results", "figures", paste("trees",n_tree, sep="_"))
  .check_create(dir_gr)
  
  ggsave(file.path(dir_gr,fname), plot = gg, dpi=300, bg ="white")
  
}

gr_plot_data_points <- function(args){
  n_tree <- args$n_tree
  
  df_fname <- file.path("results", paste0("gr_n_trees_",n_tree,".csv"))
  if (file.exists(df_fname)){
    df <- read.csv(df_fname)
  } else {
  df_bart <- .get_gr_df_dp(args, 200, F)
  df_bart$Model <- "BART"
  
  df_smpl <- .get_gr_df_dp(args, 1, T)
  df_smpl$Model <- "Simplified BART"
  
  df  <- rbind(df_smpl, df_bart)
  df$Model <- factor(df$Model) 
  write.csv(df, df_fname)
  }
  
  df$ds_lower <- str_replace(tolower(df$Dataset), " ", "_")
  
  
  df_bart_main <- df %>% filter(Model=="BART", ds_lower %in% DATASETS)
  .plot_dp(df=df_bart_main, leg_loc = c(0.23, 0.85), fname = "gr_n_bart_main.png", clrs = COLORS, n_tree=n_tree, y_lab="Gelman-Rubin")
  
  
  df_bart_apdx <- df %>% filter(Model=="BART", ds_lower %in% DATASETS_APDX)
  .plot_dp(df=df_bart_apdx, leg_loc = c(0.23, 0.85), fname = "gr_n_bart_apdx.png", clrs = COLORS_APDX, n_tree=n_tree, y_lab="Gelman-Rubin")
  
  df_sbart_main <- df %>% filter(Model=="Simplified BART", ds_lower %in% DATASETS)
  .plot_dp(df=df_sbart_main, leg_loc = "none", fname = "gr_n_sbart_main.png", clrs = COLORS, n_tree=n_tree)
  
  
  df_sbart_apdx <- df %>% filter(Model=="Simplified BART", ds_lower %in% DATASETS_APDX)
  .plot_dp(df=df_sbart_apdx, leg_loc = "none", fname = "gr_n_sbart_apdx.png", clrs = COLORS_APDX, n_tree=n_tree)
  
  

  # df$data_points[df$data_points>2000] <- 3000
  # gg <- df %>% ggplot(aes(x=data_points, y=GR, color=Dataset))  +   geom_line(aes(linetype=Model), size=1.2) +
  #   geom_point(size=4) +   geom_errorbar(aes(ymin=GR-sd, ymax=GR+sd), width=1,
  #                                        position=position_dodge(0.05)) +
  #   theme_minimal() + xlab("Number of Data Points") + ylab("Gelman-Rubin") +
  #   geom_hline(yintercept=1.1, linetype='solid', col = 'red4', size=1, alpha=0.3)+
  #   # annotate("text", x = 11, y = 1.11, label = "Convergence\nThreshold", vjust = -0.5, size=8)+
  #   theme(text = element_text(size = TEXT_SIZE), legend.position = c(0.2, 0.8)) + scale_colour_manual(values=COLORS) +
  #   scale_x_continuous(trans = 'log10',
  #                        breaks = trans_breaks('log10', function(x) 10^x),
  #                        labels = trans_format('log10', math_format(10^.x)))

  # dir_gr <- file.path("results", "figures", paste("trees",n_tree, sep="_"))
  # .check_create(dir_gr)
  # 
  # ggsave(file.path(dir_gr,"gr_n.png"), plot = gg, dpi=300, bg ="white")



}

.plot_flips <- function(df, pos, y_lab, fname, n_tree){
  gg <- df %>% ggplot(aes(x=data_points, y=Change, color=Dataset))  +   geom_line(size=1.4) +
    geom_point(size=4) +   geom_errorbar(aes(ymin=Change-sd, ymax=Change+sd), width=0.2, size=1.2,
                                         position=position_dodge(0.0)) +
    theme_minimal() + xlab("Number of Data Points") + ylab(y_lab) +
    #geom_hline(yintercept=1.1, linetype='dotted', col = 'red', size=2)+
    # annotate("text", x = 3.5, y = 1.11, label = "Convergence\nThreshold", vjust = -0.5)+
    theme(text = element_text(size = TEXT_SIZE),  legend.position = pos) +
    scale_colour_manual(values=COLORS) + scale_x_continuous(trans = 'log10',
                                                            breaks = trans_breaks('log10', function(x) 10^x),
                                                            labels = trans_format('log10', math_format(10^.x)))
  dir_gr <- file.path("results","figures", paste("trees",n_tree, sep="_"))
  .check_create(dir_gr)

  ggsave(file.path(dir_gr,fname), plot = gg, dpi=300, bg ="white")
}

flips_plot <- function(args){
  n_tree <- 1
  nskip <- args$n_burn
  ndpost <- args$post
  nchain <- args$n_chains
  p <- args$n_features
  run <- args$run
  synthetic <- args$synthetic
  n_tree <- args$n_tree
  add_leg <- FALSE
  n_dps <- c(200, 2000, Inf)
  file_flips <- file.path("results", "flips.csv")
  if (file.exists(file_flips)){
    df <- read.csv(file_flips)
  } else {
  df <- data.frame(matrix(ncol = 4, nrow = 0))

  for (ds_name in DATASETS){
    for (n_dp in n_dps){
      data_fir <- .get_dir_data(n_tree = n_tree, ds_name = ds_name, restricted=F)
      data_fir_r <- .get_dir_data(n_tree = n_tree, ds_name = ds_name, restricted=T)
      
      n  <- .get.real.n(ds_name = ds_name, n = n_dp)




      y_lab_split <- ifelse(n==200, "First Split Variable", "")
      flips <- c()
      flips_restricted <- c()
      for (run in 1:20){
      fname <-  file.path(data_fir, paste0(.get_fname(nskip, ndpost, n,nchain,p,run,  synthetic), "_first_split.csv"))
      fname_r <-  file.path(data_fir_r, paste0(.get_fname(nskip, ndpost, n,nchain,p,run,  synthetic), "_first_split.csv"))
      
      if (file.exists(fname) & file.exists(fname_r)){

      flips <- c(flips, first_split_plot(ds_name = ds_name, n = n,p = p, n_tree = n_tree
        , nskip = nskip, ndpost = ndpost, nchain=nchain,
                       add_legend = F,y_lab = y_lab_split,
        synthetic = FALSE, run=run, restricted=F))
      flips_restricted <- c(flips_restricted,first_split_plot(ds_name = ds_name, n = n,p = p, n_tree = n_tree
                                             , nskip = nskip, ndpost = ndpost, nchain=nchain,
                                             add_legend = add_leg,y_lab = y_lab_split,
                                             synthetic = FALSE, run=run, restricted=T))
        }}
      
      
      sd_flips <- 1.96 *(sd(flips) / length(flips))
      
      df <- rbind(df, c(mean(flips), sd_flips, .get.label.name(ds_name),
                        .get.real.n(ds_name, n), "B-CART"))

      
      sd_flips_restricted <- 1.96 *(sd(flips_restricted) / length(flips_restricted))
      df <- rbind(df, c(mean(flips_restricted), sd_flips_restricted, .get.label.name(ds_name),
                        .get.real.n(ds_name, n), "Simplifed BART"))
      
      
      }

    }

  
  colnames(df) <- c("Change", "sd", "Dataset", "data_points", "Model")

  df$Change <- as.numeric(df$Change) * 6000
  df$sd <- as.numeric(df$sd)* 6000 
  df$Model <- factor(df$Model)
  df$data_points_raw <- as.numeric(df$data_points)
  
  # df$data_points <- log(as.numeric(df$data_points))
  df$data_points <- as.numeric(df$data_points)
  df$Dataset <- factor(df$Dataset)
    write.csv(df, file_flips)
  }
  df$ds_lower <- str_replace(tolower(df$Dataset), " ", "_")
  bart_flips <- df %>% filter(Model=="B-CART")
  .plot_flips(df=bart_flips, pos = c(0.7, 0.8), y_lab = "Number of Changes to the Root Split", fname = "bcart_flips.png", n_tree=n_tree)

  bart_flips <- df %>% filter(Model!="B-CART")
  .plot_flips(df=bart_flips, pos = "none", y_lab = "", fname = "sbart_flips.png", n_tree=n_tree)

  # df$data_points[df$data_points>2000] <- 3000


}

main <- function(args){
  ds_name <- args$dataset
  n <- args$n_d_p
  p <- args$n_features
  n_tree <- args$n_tree
  nskip <- args$n_burn
  ndpost <- args$post
  nchain <- args$n_chains
  run <- args$run
  plot_type <- args$plot_type
  synthetic <- args$synthetic
  restricted <- args$restricted
  if (is.na(synthetic)){
    synthetic <- FALSE
  }
  if (plot_type == "gr"){
    gr_plot_data_points(args)
    gr_plot_features(args)
  }
  if (plot_type == "flips"){
    flips_plot(args)

  }
  if (plot_type == "all"){
    add_legend <- TRUE
    y_lab <- ""
    first_split_plot(ds_name = ds_name, n = n,p = p, n_tree = n_tree
                     , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = add_legend,y_lab = y_lab, synthetic = synthetic, run=run, restricted=restricted, plot=T)
    rmse_plot(ds_name = ds_name, n = n,p = p, n_tree = n_tree
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
              add_legend = add_legend,y_lab = y_lab, synthetic = FALSE, run=run, restricted=restricted, plot=T)
    cumsum_rmse_plot(ds_name = ds_name, n = n,p = p, n_tree = n_tree
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
              add_legend = add_legend,y_lab = y_lab, synthetic = FALSE, run=run, restricted=restricted)}
  if (plot_type == "rmse"){
    add_legend <- TRUE
    y_lab <- "Density"
    rmse_plot(ds_name = ds_name, n = n,p = p, n_tree = n_tree
              , nskip = nskip, ndpost = ndpost, nchain=nchain,
              add_legend = add_legend,y_lab = y_lab, synthetic = FALSE, run=run, restricted=restricted, plot=T)}
  if (plot_type == "first_split"){
    add_legend <- FALSE
    y_lab <- ""#"First Split Variable"
    first_split_plot(ds_name = ds_name, n = n,p = p, n_tree = n_tree
                     , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = add_legend,y_lab = y_lab, synthetic = synthetic, run=run, restricted=restricted, plot=T)}

  if (plot_type == "cusum"){
    cumsum_rmse_plot(ds_name = "breast_tumor", n = 200,p = 1, n_tree = 200
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = F,y_lab = TeX(r'($S^{(j)}_t$)'), synthetic = FALSE, run=2, restricted=F)
    cumsum_rmse_plot(ds_name = "breast_tumor", n = Inf,p = 1, n_tree = 200
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = F,y_lab = "", synthetic = FALSE, run=2, restricted=F)
    cumsum_rmse_plot(ds_name = "california_housing", n = 200,p = 1, n_tree = 200
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = F,y_lab = TeX(r'($S^{(j)}_t$)'), synthetic = FALSE, run=3, restricted=F)
    cumsum_rmse_plot(ds_name = "california_housing", n = Inf,p = 1, n_tree = 200
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = F,y_lab = "", synthetic = FALSE, run=3, restricted=F)

    cumsum_rmse_plot(ds_name = "breast_tumor", n = 200,p = 1, n_tree = 1
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = F,y_lab = TeX(r'($S^{(j)}_t$)'), synthetic = FALSE, run=2, restricted=T)
    cumsum_rmse_plot(ds_name = "breast_tumor", n = Inf,p = 1, n_tree = 1
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = F,y_lab = "", synthetic = FALSE, run=2, restricted=T)
    cumsum_rmse_plot(ds_name = "california_housing", n = 200,p = 1, n_tree = 1
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = F,y_lab = TeX(r'($S^{(j)}_t$)'), synthetic = FALSE, run=3, restricted=T)
    cumsum_rmse_plot(ds_name = "california_housing", n = Inf,p = 1, n_tree = 1
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = F,y_lab = "", synthetic = FALSE, run=3, restricted=T)
  }
  if (plot_type == "rmse_paper"){
    y_lab  <- "Chains"
    rmse_plot(ds_name = "breast_tumor", n = 200,p = 1, n_tree = 200
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = T,y_lab = y_lab, synthetic = FALSE, run=2, restricted=F, plot = T)
    rmse_plot(ds_name = "breast_tumor", n = Inf,p = 1, n_tree = 200
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = F,y_lab = "", synthetic = FALSE, run=2, restricted=F, plot = T)
    rmse_plot(ds_name = "california_housing", n = 200,p = 1, n_tree = 200
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = F,y_lab = y_lab, synthetic = FALSE, run=3, restricted=F, plot = T)
    rmse_plot(ds_name = "california_housing", n = Inf,p = 1, n_tree = 200
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = F,y_lab = "", synthetic = FALSE, run=3, restricted=F, plot = T)

    rmse_plot(ds_name = "breast_tumor", n = 200,p = 1, n_tree = 1
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = T,y_lab = y_lab, synthetic = FALSE, run=2, restricted=T, plot = T)
    rmse_plot(ds_name = "breast_tumor", n = Inf,p = 1, n_tree = 1
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = F,y_lab = "", synthetic = FALSE, run=2, restricted=T, plot = T)
    rmse_plot(ds_name = "california_housing", n = 200,p = 1, n_tree = 1
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = F,y_lab = y_lab, synthetic = FALSE, run=3, restricted=T, plot = T)
    rmse_plot(ds_name = "california_housing", n = Inf,p = 1, n_tree = 1
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = F,y_lab = "", synthetic = FALSE, run=3, restricted=T, plot = T)
  }
  if (plot_type == "first_split_paper"){
    y_lab  <- "Count"
    first_split_plot(ds_name = "breast_tumor", n = 200,p = 1, n_tree = 200
              , nskip = nskip, ndpost = ndpost, nchain=nchain,
              add_legend = T,y_lab = y_lab, synthetic = FALSE, run=2, restricted=F, plot = T)
    first_split_plot(ds_name = "breast_tumor", n = Inf,p = 1, n_tree = 200
              , nskip = nskip, ndpost = ndpost, nchain=nchain,
              add_legend = F,y_lab = "", synthetic = FALSE, run=2, restricted=F, plot = T)
    first_split_plot(ds_name = "california_housing", n = 200,p = 1, n_tree = 200
              , nskip = nskip, ndpost = ndpost, nchain=nchain,
              add_legend = F,y_lab = y_lab, synthetic = FALSE, run=3, restricted=F, plot = T)
    first_split_plot(ds_name = "california_housing", n = Inf,p = 1, n_tree = 200
              , nskip = nskip, ndpost = ndpost, nchain=nchain,
              add_legend = F,y_lab = "", synthetic = FALSE, run=3, restricted=F, plot = T)
    
    first_split_plot(ds_name = "breast_tumor", n = 200,p = 1, n_tree = 1
              , nskip = nskip, ndpost = ndpost, nchain=nchain,
              add_legend = T,y_lab = y_lab, synthetic = FALSE, run=2, restricted=T, plot = T)
    first_split_plot(ds_name = "breast_tumor", n = Inf,p = 1, n_tree = 1
              , nskip = nskip, ndpost = ndpost, nchain=nchain,
              add_legend = F,y_lab = "", synthetic = FALSE, run=2, restricted=T, plot = T)
    first_split_plot(ds_name = "california_housing", n = 200,p = 1, n_tree = 1
              , nskip = nskip, ndpost = ndpost, nchain=nchain,
              add_legend = F,y_lab = y_lab, synthetic = FALSE, run=3, restricted=T, plot = T)
    first_split_plot(ds_name = "california_housing", n = Inf,p = 1, n_tree = 1
              , nskip = nskip, ndpost = ndpost, nchain=nchain,
              add_legend = F,y_lab = "", synthetic = FALSE, run=3, restricted=T, plot = T)

    first_split_plot(ds_name = "breast_tumor", n = 200,p = 1, n_tree = 1
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = T,y_lab = y_lab, synthetic = FALSE, run=2, restricted=F, plot = T)
    first_split_plot(ds_name = "breast_tumor", n = Inf,p = 1, n_tree = 1
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = F,y_lab = "", synthetic = FALSE, run=2, restricted=F, plot = T)
    first_split_plot(ds_name = "california_housing", n = 200,p = 1, n_tree = 1
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = F,y_lab = y_lab, synthetic = FALSE, run=3, restricted=F, plot = T)
    first_split_plot(ds_name = "california_housing", n = Inf,p = 1, n_tree = 1
      , nskip = nskip, ndpost = ndpost, nchain=nchain,
                     add_legend = F,y_lab = "", synthetic = FALSE, run=3, restricted=F, plot = T)
  }
}


if (getOption('run.main', default=TRUE)) {
  parser <- OptionParser(option_list=option_list);
  args <- parse_args(parser);

  main(args)
}


