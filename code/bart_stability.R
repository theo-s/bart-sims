library(dbarts)
library(MASS)
library(purrr)
library(optparse)
library(here)
library(mvtnorm)
library(onehot)

option_list = list(
  make_option(c("-d", "--dgp"), type="character", default="sum",
              help="data generating process", metavar="character"),
  make_option(c("-r", "--runs"), type="integer", default=8,
              help="number of runs", metavar="integer")
);

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
  }
  if (dgp == "rockova"){
    X <- t(replicate(n, runif(q))) # draw samples

    y <- .rockova_dgp(X, q)
  }
  return(list(X=X, y=y))
}

.rockova_dgp <- function(X, q){
  first_term <- 1
  second_term <- rowSums((-1)^(1:q) * (X[, 1:q] >= 1/2))
  third_term <- rowSums((X[, 1:q] - 1/2)^2)
  return (first_term + 1/q * ( second_term * third_term))
}

get_data <- function(dgp,n, q=10, rho=0.05, seed=0){

    train_n <- n
    test_n <- 200
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
      data_train$y <- data_train$y + 2 * rnorm(train_n)
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
  return(list(rmse=rmse, coverage=average_coverage, interval_length=average_length))


}

main <- function(args){
  runs <- args$runs
  dgp <- args$dgp
  total_ndpost <- 1000
  column_names <- c("RMSE", "Coverage", "Interval length", "n", "run", "Chains")
  results <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  colnames(results) <- column_names
  for (n in c(100, 1000, 10000, 100000)){
        for (run in 1:runs){

    bart_sim_partial <- partial(bart_sim, dgp = dgp, total_ndpost=total_ndpost, seed=run, n=n)

    bart_1 <- bart_sim(nchain=1,dgp = dgp, total_ndpost=total_ndpost, seed=run, n=n)
    results_1 <- c(bart_1$rmse, bart_1$coverage, bart_1$interval_length, n, run, 1)

    bart_5 <- bart_sim(nchain=8,dgp = dgp, total_ndpost=total_ndpost, seed=run, n=n)
    results_5 <- c(bart_5$rmse, bart_5$coverage, bart_5$interval_length, n, run, 8)

    bart_10 <- bart_sim(nchain=20,dgp = dgp, total_ndpost=total_ndpost, seed=run, n=n)
    results_10 <- c(bart_10$rmse, bart_10$coverage, bart_10$interval_length, n, run, 20)

    combined_results <- rbind(results_1, results_5, results_10)
    results <- rbind(results, combined_results, make.row.names=FALSE)





      }}
  colnames(results) <- column_names
  file_name <- here(file.path("results", paste("dgp", dgp,"chain_analysis.csv", sep = '_')))
  write.csv(results, file_name, row.names = FALSE, sep = ",")

  return(results)



}


if (getOption('run.main', default=TRUE)) {
  parser <- OptionParser(option_list=option_list);
  args <- parse_args(parser);

  print(main(args))
}

