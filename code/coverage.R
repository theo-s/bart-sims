library(dbarts)
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
library(rJava)

data_folder_name <- "results/"
options(java.parameters = "-Xmx20g")

source("code/generate_data_3.R")
source("code/generate_estimators.R")

set.seed(5387479)

outfile = "~/Desktop/bart-sims/results/coverage.csv"
use_all_moves=FALSE

# generate all the different jobs and save it ----------------------------------
# ds_names = c("Friedman_1","Friedman_3")
(ds_names <- names(datasets_grid))
(rep <- 1:5)

(all_jobs <- expand.grid(ds_names, rep) %>%
    dplyr::rename(Dataset = Var1, Chain = Var2)) %>%
  dplyr::arrange(Dataset,Chain)

all_jobs$Dataset <- as.character(all_jobs$Dataset)

npost = 5e3

results <- data.frame(matrix(NA,
                             ncol = 2,
                             nrow = npost*nrow(all_jobs)))

colnames(results) <- c("Dataset","Chain")

for (i in 1:nrow(all_jobs)) {
  print(c((npost*(i-1)+1),(npost*(i))))
  results$Dataset[(npost*(i-1)+1):(npost*(i))] <- rep(all_jobs$Dataset[i], npost)
  results$Chain[(npost*(i-1)+1):(npost*(i))] <- rep(all_jobs$Chain[i], npost)
}

results$RMSE <- NA
results$pred1 <- NA
results$sigma <- NA

# Here we focus on 4 main summary statistics of the bart mcmc chain
# 1) Avg number of leaves per tree
# 2) MSE on the same fixed test set
# 3) the predictions for a single fixed point (point 1 in the test set)
# 4) The split proportions of feature 1
# This yields 4 * ndpost statistics for each chain

for ( i in 1:nrow(all_jobs) ) {

  # Read in the data set
  set.seed(6264175)
  (this_job <- all_jobs[i, ])
  print(this_job)
  ds <- datasets_grid[[as.character(this_job$Dataset)]]

  # Fit BART to the data set
  print("Fitting Model")
  print("Using Grow/Prune moves only")
  es_trnd <- bart_bartMachine(Xobs = ds$train %>% dplyr::select(-y),
                              Yobs = ds$train %>% dplyr::select(y) %>% .[,1],
                              seed = i,
                              note = as.character(this_job$Dataset))



  print("Getting Predictions")
  pdctns <-  bart_machine_get_posterior(es_trnd$bart, ds$test %>% dplyr::select(-y))$y_hat_posterior_samples

  print("Summary Statistics")
  # Get test set RMSE
  RMSE <- apply(pdctns,
                MARGIN = 2,
                FUN = function(x){return(sqrt(mean((x - ds$test %>% dplyr::select(y) %>% .[,1])^2)))} )

  # Get the test set, first observation predictions
  pred1 <- bart_machine_get_posterior(es_trnd$bart, ds$test[1,] %>% dplyr::select(-y))$y_hat_posterior_samples[1,]


  # Get the length of the prediction interval for the first data point
  pred_sigma <- get_sigsqs(es_trnd$bart)

  results$RMSE[(npost*(i-1)+1):(npost*(i))] <- RMSE
  results$pred1[(npost*(i-1)+1):(npost*(i))] <- pred1
  results$sigma[(npost*(i-1)+1):(npost*(i))] <- pred_sigma

  print(head(results[(npost*(i-1)+1):(npost*(i)),]))

  write.csv(results, file = outfile)
  rm(pdctns)
  rm(es_trnd)
}

write.csv(results, file = outfile)




