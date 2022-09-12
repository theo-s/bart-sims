library(assertthat)
library(here)

test_that("plots data function", {
  setwd(here())
  options(run.main=FALSE)
  source("code/bart_sim.R")
  
  ds_name <- "california_housing"
  n <- 200
  n_tree <- 1
  nskip <- 5000
  ndpost <- 1000
  nchain <- 8
  p <- 1
  
  data_all <- get_data(ds_name, n, p, FALSE)
  data_train <- data_all[["train"]]
  data_test <- data_all[["test"]]
  y.train <- get.labels(data_train)
  n <- length(y.train)
  dir_data <- .get_dir_data(n_tree, ds_name)
  dir_fig <- .get_dir_fig(n_tree, ds_name)
  
  fname <- .get_fname(nskip = nskip,ndpost = ndpost, n=n, nchain = nchain, synthetic = synthetic)
  
  plots_data <- .get_plots_data(data_train, data_test,n_tree, nskip, ndpost, nchain, fname, dir_data)
  
  n_chains_rmse <- dim(plots_data$rmse_mat)[2] -1
  n_post_rmse <- dim(plots_data$rmse_mat)[1]
  
  total_sample_split <- dim(plots_data$split_mat)[1]
  
  expect_equal(n_chains_rmse, nchain)
  expect_equal(n_post_rmse, ndpost)
  expect_equal(total_sample_split, (ndpost+nskip) * nchain)
  
  
})

test_that("rmse mat", {
  setwd(here())
  options(run.main=FALSE)
  source("code/bart_sim.R")
  
  ds_name <- "california_housing"
  n <- 200
  n_tree <- 1
  nskip <- 5000
  ndpost <- 1000
  nchain <- 8
  p <- 1
  
  data_all <- get_data(ds_name, n, p, FALSE)
  data_train <- data_all[["train"]]
  data_test <- data_all[["test"]]
  y.train <- get.labels(data_train)
  n <- length(y.train)
  dir_data <- .get_dir_data(n_tree, ds_name)
  dir_fig <- .get_dir_fig(n_tree, ds_name)
  
  fname <- .get_fname(nskip = nskip,ndpost = ndpost, n=n, nchain = nchain, synthetic = synthetic)
  
  x.train <- get.features(data_train)
  bart_fit <- bart(x.train = x.train,
                   y.train = y.train,
                   keeptrees = TRUE,
                   verbose = TRUE,
                   nskip = 0,
                   keepevery = 1,
                   ntree = n_tree,
                   ndpost = ndpost+nskip,
                   nchain = nchain)
  
  x.test <- get.features(data_test)
  y.test <- get.labels(data_test)
  
  pdctns <- predict(bart_fit, x.test) #stacked chains, i.e first ndpost+nskip rows is chain 1
  
  random_chain <- sample(nchain, 1)
  random_mcmc_sample <- sample(ndpost, 1)
  random_idx <- nskip + (random_chain-1) * ndpost + random_mcmc_sample
  random_prediction <- pdctns[random_idx,]
  
  rmse_random_idx <- rmse(y.test, random_prediction)
  
  
  rmse_mat <- .get_rmse_mat(data_test = data_test, bart_fit = bart_fit,
                            ndpost = ndpost, nskip = nskip, nchain = nchain)
  
  rmse_random_idx_mat <- rmse_mat[random_mcmc_sample, random_chain]
  
  expect_equal(rmse_random_idx, rmse_random_idx_mat)
  
  
})