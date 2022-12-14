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

data_folder_name <- "results/"


source("code/generate_data_3.R")
source("code/generate_estimators.R")
source("code/bart_helpers.R")

set.seed(5387479)

# outfile = "~/Desktop/bart-sims/results/high_splits.csv"
# use_all_moves=FALSE

outfile = "~/Desktop/bart-sims/results/full_high_splits.csv"
use_all_moves=TRUE

# generate all the different jobs and save it ----------------------------------
# ds_names = c("Friedman_1","Friedman_3")
(ds_names <- names(datasets_grid))
(rep <- 1:10)

(all_jobs <- expand.grid(ds_names, rep) %>%
    dplyr::rename(Dataset = Var1, Chain = Var2)) %>%
  dplyr::arrange(Dataset,Chain)

all_jobs$Dataset <- as.character(all_jobs$Dataset)

npost = 20e3

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
results$first_split_var <- NA
results$leaf_counts <- NA

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
  if (use_all_moves) {
    print("Using all moves")
    es_trnd <- bart_fit(Xobs = ds$train %>% dplyr::select(-y),
                        Yobs = ds$train %>% dplyr::select(y) %>% .[,1],
                        seed = i,
                        note = as.character(this_job$Dataset))
  } else {
    print("Using Grow/Prune moves only")
    es_trnd <- bart_fit_grow_prune(Xobs = ds$train %>% dplyr::select(-y),
                                   Yobs = ds$train %>% dplyr::select(y) %>% .[,1],
                                   seed = i,
                                   note = as.character(this_job$Dataset))
  }


  print("Getting Predictions")
  pdctns <- bart_predict(estimator = es_trnd,
                         feat = ds$test %>% dplyr::select(-y))

  print("Summary Statistics")
  # Get test set RMSE
  RMSE <- apply(pdctns,
                MARGIN = 1,
                FUN = function(x){return(sqrt(mean((x - ds$test %>% dplyr::select(y) %>% .[,1])^2)))} )

  # Get the test set, first observation predictions
  pred1 <-  pdctns[,1]



  # Get the average number of leaves per tree
  all_data <- es_trnd$bart$fit$getTrees(treeNums = 1,
                                        chainNums = 1,
                                        sampleNums = 1:(npost))

  all_data %>%
    dplyr::select(var, tree, sample) %>%
    filter(var < 0) %>%
    group_by(sample, tree) %>%
    summarise(numLeaves = n()) %>%
    group_by(sample) %>%
    summarise(numLeaves = mean(numLeaves)) %>%
    dplyr::select(numLeaves) -> leaf_counts

  # Get the first splitting variable
  all_data %>%
    filter(n==ncol(es_trnd$bart$yhat.train)) %>%
    dplyr::select(sample, var) -> samples

  first_splits = samples$var

  results$RMSE[(npost*(i-1)+1):(npost*(i))] <- RMSE
  results$pred1[(npost*(i-1)+1):(npost*(i))] <- pred1
  results$first_split_var[(npost*(i-1)+1):(npost*(i))] <- first_splits
  results$leaf_counts[(npost*(i-1)+1):(npost*(i))] <- leaf_counts$numLeaves

  print(head(results[(npost*(i-1)+1):(npost*(i)),]))

  write.csv(results, file = outfile)
}

write.csv(results, file = outfile)




