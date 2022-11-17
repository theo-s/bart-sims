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


main <- function(){
  ds_name <- "echo_months"
  n <- 200
  p <- 1
  n_tree <- 200
  nskip <- 100
  ndpost <- 1000
  nchain <- 8
  data_all <- get_data(ds_name = ds_name, n = n, p = p, synthetic=F, seed = 42)
  data_train <- data_all[["train"]]
  data_test <- data_all[["test"]]
  y.train <- get.labels(data_train)
  names(probs) <- c("birth_death", "change","swap","birth")
  bart_fit <- bart(x.train = x.train,
                   y.train = y.train,
                   keeptrees = TRUE,
                   verbose = TRUE,
                   nskip = 0,
                   keepevery = 1,
                   ntree = n_tree,
                   ndpost = ndpost+nskip,
                   nchain = nchain,
                   seed=run,
                   proposalprobs=probs)

}


if (getOption('run.main', default=TRUE)) {

  main()
}
