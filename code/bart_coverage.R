library(dbarts)
library(MASS)
library(purrr)
library(optparse)
library(here)
library(onehot)
library(dgpoix)

option_list = list(
  make_option(c("-d", "--dgp"), type="character", default="sum",
              help="data generating process", metavar="character"),
  make_option(c("-r", "--runs"), type="integer", default=8,
              help="number of runs", metavar="integer")
);


athey_outcome <- function(X){
  2 / (1 + exp(-12 * (X[, 1] - 0.5))) * 2 / (1 + exp(-12 * (X[, 2] - 0.5)))
}

piecewise_linear_outcome <- function(X) {
  d <- ncol(X)

  beta1 <- runif(d, -15, 15)
  beta2 <- runif(d, -15, 15)
  beta3 <- runif(d, -15, 15)

  ifelse(
    X[, ncol(X)] < -0.4,
    as.matrix(X) %*% beta1,
    ifelse(
      X[, ncol(X)] < 0.4,
      as.matrix(X) %*% beta2,
      as.matrix(X) %*% beta3))
}

get.labels <- function(data){
  cols <- ncol(data)
  return(data[, cols])
  # return(data %>% dplyr::select(last_col()) %>% .[, 1])
}

get.features <- function(data){
  feature_cols <- ncol(data) - 1
  return(data[, 1:feature_cols])
  # return(data %>% dplyr::select(-last_col()))
}

.generate_data <- function(dgp, n, q, rho){
  if (dgp == "sum"){
    cov_q <- diag(1, q)
    cov_q_q <- diag(rho, q)
    cov_matrix <- cbind(rbind(cov_q, cov_q_q), rbind(cov_q_q, cov_q))
    X <- mvrnorm(n = n, mu = rep(0, 2*q), Sigma = cov_matrix)
    y <- rowSums(X[, 1:q])
  } else if (dgp == "atheylow"){
    X <- matrix(rnorm(n * 10), nrow = n, ncol = 10)
    fac <- rnorm(n)
    X <- X * sqrt(1 - rho) + fac * sqrt(rho)
    X <- pnorm(X)
    y <- athey_outcome(X)
  } else if (dgp == "atheyhigh"){
    X <- matrix(rnorm(n * 50), nrow = n, ncol = 50)
    fac <- rnorm(n)
    X <- X * sqrt(1 - rho) + fac * sqrt(rho)
    X <- pnorm(X)
    y <- athey_outcome(X)
  } else if (dgp == "piecewise"){
    X <- matrix(rnorm(n * 20), nrow = n, ncol = 20)
    fac <- rnorm(n)
    X <- X * sqrt(1 - rho) + fac * sqrt(rho)
    X <- pnorm(X)
    y <- piecewise_linear_outcome(X)
  } else if (dgp == "tree"){
    X <- matrix(rnorm(n * 10), nrow = n, ncol = 10)
    tree_loaded <- loadForestry("code/saved_tree.RDS")
    # Note variance is 0.1543403
    y <- predict(tree_loaded, newdata = X)
  } else if (dgp == "lss") {
    X <- dgpoix::generate_X_gaussian(.n = n, .p = 10)
    # y = 2 * 1(X_1 < 0, X_3 > 0) - 3 * 1(X_5 > 0, X_6 > 1) + .8 * 1(X_3 < 1.5, X_5 < 1)
    y <- dgpoix::generate_y_lss(X = X,
                                k = 2,
                                s = matrix(c(1,3,5,6,3,5), nrow = 3, byrow = TRUE),
                                thresholds = matrix(c(0,0,0,1,1.5,1), nrow = 3),
                                signs = matrix(c(-1,1,1,1,-1,-1), nrow = 3),
                                betas = c(2,-3,.8),
                                err = NULL)
  }

  return(list(X=X, y=y))

}

get_data <- function(dgp,n, q=10, rho=0.05, seed=0){

  train_n <- n
  test_n <- 1000
  set.seed(seed)

  if ((dgp %in% c("2016","2017","2018","2019"))) {
    data <- read.csv(paste0("data/ACIC",dgp,".csv"))

    # Shuffle the data set, so the train and test set are disjoint
    # but we break any unintended clustering of observations
    data <- data[sample(1:nrow(data), size = train_n+test_n, replace = FALSE),]
    encoding <- onehot::onehot(data, stringsAsFactors = TRUE, max_levels = 100)
    data <- predict(encoding, data)

    if (nrow(data) < train_n+test_n) {
      stop("N is too large for the current dgp")
    }

    data_train <- data[1:train_n,-which(colnames(data) == "Ytrue")]
    data_test <- data[(train_n+1):(train_n+200),-which(colnames(data) == "Y")]

    colnames(data_train)[which(colnames(data_train) == "Y")] <- "y"
    colnames(data_test)[which(colnames(data_test) == "Ytrue")] <- "y"

    data_train <- cbind(data_train[,-which(colnames(data_train) == "y")],
                        data_train[,which(colnames(data_train) == "y")])

    data_test <- cbind(data_test[,-which(colnames(data_test) == "y")],
                       data_test[,which(colnames(data_test) == "y")])


    data_all <- list(train=as.matrix(data_train),
                     test=as.matrix(data_test))
  } else {
    data_train <- .generate_data(dgp, train_n, q, rho)

    # Modify noise level based on the variance of the outcome
    if (dgp == "sum") {
      noise_sd <- 2
    } else if (dgp %in% c("atheylow","atheyhigh")) {
      noise_sd <- 1
    } else if (dgp == "piecewise") {
      noise_sd <- sd(data_train$y)
    } else if (dgp == "tree") {
      noise_sd <- 2*0.1543403
    } else if (dgp == "lss") {
      noise_sd <- 3
    }

    data_train$y <- data_train$y + noise_sd * rnorm(train_n)
    data_test <- .generate_data(dgp, test_n, q, rho)

    data_all <- list(train=cbind(data_train$X, data_train$y),
                     test=cbind(data_test$X, data_test$y))
  }
  return(data_all)
}

bart_sim <- function(dgp, nchain, n, total_ndpost, seed){
  nskip <- 1000
  ndpost <- ceiling(total_ndpost / nchain)
  n_tree <- 200
  probs <-c(0.5, 0.1, 0.4,0.5)
  q <- 10
  data_all <- get_data(dgp=dgp, n = n, q = q, rho = 0.01, seed = seed)
  data_train <- data_all$train
  data_test <- data_all$test
  x.train <- get.features(data_train)
  y.train <- get.labels(data_train)
  x.test <- get.features(data_test)
  y.test <- get.labels(data_test)
  names(probs) <- c("birth_death", "change","swap","birth")
  bart_fit <- bart(x.train = x.train,
                   y.train = y.train,
                   x.test = x.test,
                   keeptrees = F,
                   verbose = F,
                   nskip = nskip,
                   keepevery = 1,
                   ntree = n_tree,
                   ndpost = ndpost,
                   nchain = nchain,
                   seed = seed,
                   proposalprobs=probs)

  point_preds_bart <-  bart_fit$yhat.test.mean
  rmse <- sqrt(mean((point_preds_bart-y.test)^2))
  posterior_bart <- bart_fit$yhat.test
  quantiles <- apply(posterior_bart, 2, quantile, probs = c(0.025, 0.975))
  average_length <- mean(quantiles[2, ] - quantiles[1, ])
  average_coverage <- mean(y.test > quantiles[1, ] & y.test < quantiles[2, ])
  return(list(rmse=rmse,
              coverage = average_coverage,
              upper=quantiles[2, ],
              lower=quantiles[1, ],
              true_outcome=y.test))
}

main <- function(args){
  runs <- args$runs
  dgp <- args$dgp
  total_ndpost <- 1000
  column_names <- c("RMSE", "Coverage", "Interval length", "n", "run", "Chains")
  results <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  colnames(results) <- column_names
  for (n in c(100,1000,10e3,20e3,50e3,100e3)){
    for (run in 1:runs){

      bart_sim_partial <- partial(bart_sim, dgp = dgp, total_ndpost=total_ndpost, seed=run, n=n)

      bart_1 <- bart_sim(nchain=1,dgp = dgp, total_ndpost=total_ndpost, seed=run, n=n)
      results_1 <- list("rmse" = bart_1$rmse,
                        "empirical_cov" = bart_1$coverage,
                        "ci_upper" = bart_1$upper,
                        "ci_lower" = bart_1$lower,
                        "true_y" = bart_1$true_outcome,
                        "n" = n,
                        "run" = run,
                        "nchain" = 1)

      bart_2 <- bart_sim(nchain=2,dgp = dgp, total_ndpost=total_ndpost, seed=run, n=n)
      results_2 <- list("rmse" = bart_2$rmse,
                        "empirical_cov" = bart_2$coverage,
                        "ci_upper" = bart_2$upper,
                        "ci_lower" = bart_2$lower,
                        "true_y" = bart_2$true_outcome,
                        "n" = n,
                        "run" = run,
                        "nchain" = 2)

      bart_5 <- bart_sim(nchain=5,dgp = dgp, total_ndpost=total_ndpost, seed=run, n=n)
      results_5 <- list("rmse" = bart_5$rmse,
                        "empirical_cov" = bart_5$coverage,
                        "ci_upper" = bart_5$upper,
                        "ci_lower" = bart_5$lower,
                        "true_y" = bart_5$true_outcome,
                        "n" = n,
                        "run" = run,
                        "nchain" = 5)

      combined_results <- list(results_1, results_2, results_5)

      file_name <- here(file.path("results/coverage/", paste("dgp", dgp,"run",run,"n",n,"coverage.RDS", sep = '_')))
      saveRDS(combined_results, file_name)
    }}
  return(results)
}


if (getOption('run.main', default=TRUE)) {
  parser <- OptionParser(option_list=option_list);
  args <- parse_args(parser);

  print(main(args))
}

