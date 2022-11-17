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
library(MASS)
source("code/bart_helpers.R")
source("code/ds_reg.R")
source("code/sim_helpers.R")


get.overlap.pct <- function(ds_name, n, seed){
  p <- 1
  n_tree <- 200
  nskip <- 5000
  ndpost <- 1000
  alpha <- 0.01
  data_all <- get_data(ds_name = ds_name, n = n, p = p, synthetic=F, seed = seed)
  data_train <- data_all[["train"]]
  data_test <- data_all[["test"]]
  y.train <- get.labels(data_train)
  x.train <- get.features(data_train)
  
  probs <-c(0.5, 0.1, 0.4,0.5)
  
  names(probs) <- c("birth_death", "change","swap","birth")
  bart_fit <- bart(x.train = x.train,
                   y.train = y.train,
                
                   keeptrees = TRUE,
                   verbose = TRUE,
                   nskip = nskip,
                   keepevery = 1,
                   ntree = n_tree,
                   ndpost = ndpost,
                   nchain = 2,
                   seed=seed,
                   proposalprobs=probs)
  

  y.test <- get.labels(data_test)
  x.test <- get.features(data_test)
  
  pdctns <- predict(bart_fit, x.test, combineChains=F)
  
  chain_1 <- pdctns[1,1:1000, ]
  chain_2 <- pdctns[2,1:1000, ]
  quants <- c(alpha/2, 1-alpha/2)
  
  ci_1 <- apply( chain_1 , 2 , quantile , probs = quants , na.rm = TRUE )
  ci_2 <- apply( chain_2 , 2 , quantile , probs = quants , na.rm = TRUE )
  
  overlap <- pmin(ci_1[2, ], ci_2[2,])- pmax(ci_1[1, ], ci_2[1,])
  overlap_pct <- overlap/pmax(ci_1[2, ] - ci_1[1, ], ci_2[2, ] - ci_2[1, ])
  overlap_pct <- pmax(overlap_pct, 0)
  non_overlap <- sum(ci_1[1, ] > ci_2[2, ])+ sum(ci_2[1, ] > ci_1[2, ])
  
  return(list(pct=mean(overlap_pct), non=non_overlap))
}


coverage.data <- function(){
  df <- data.frame(matrix(ncol = 5, nrow = 0))
  
  for (ds in c("california_housing", "breast_tumor", "echo_months", "satellite_image")){

  for (n in c(100, 1000, 10000)){
    for (seed in c(1,2,3,4,5)){
    overlap <- get.overlap.pct(ds, n, seed)
    df <- rbind(df, c(ds, n,overlap$pct, overlap$non, seed))
    }
  }}
  colnames(df) <- c("dataset", "n", "overlap %", "non overlapping")
  write_csv(df, "results/coverage_99.csv")

}


plot.coverage <- function(){
  df <- read.csv("results/coverage.csv")
  colnames(df) <- c("dataset", "n", "overlap","nonoverlap", "run")
  plt.df <- df %>% group_by(dataset, n) %>%
    summarize(avg=mean(nonoverlap), n_rep=n(), sd=sd(nonoverlap), se=sd/sqrt(n_rep))
  p<- ggplot(plt.df, aes(x=n, y=avg, group=dataset, color=dataset)) + 
    geom_line() +
    geom_point()+
    geom_errorbar(aes(ymin=avg-2*se, ymax=avg+2*se), width=.2,
                  position=position_dodge(0.05))
  
  }

if (getOption('run.main', default=TRUE)) {

  coverage.data()
}
