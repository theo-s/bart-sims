

.check_create <- function(dir){
  if (dir.exists(dir)){
    return()
  }
  dir.create(dir, recursive = TRUE)

}

.get_rmse_mat <- function(data_test, bart_fit, ndpost, nskip, nchain){
  x.test <- get.features(data_test)
  y.test <- get.labels(data_test)

  pdctns <- predict(bart_fit, x.test)
  rmse_mat <- matrix(NA, nrow = ndpost, ncol = nchain+1)
  rmse_mat[, nchain+1] <- 1:ndpost
  for (chain in 1:nchain){
    for (s in 1:ndpost){
      idx <- nskip+ (chain-1) * ndpost + s
      preds <- pdctns[idx, ]
      rmse_mat[s, chain] <- rmse(y.test, preds)
    }
  }

  return(rmse_mat)
}

.get.first.splits <- function(data_test, bart_fit, ndpost, nskip) {
  # rmse_mat <- .get_rmse_mat(data_test, bart_fit, ndpost, nskip)
  # n_c <- dim(rmse_mat)[2]-1
  # rmse_mat <- rmse_mat[, 1:n_c]
  # mean_rmse_chain <- colMeans(rmse_mat)
  # order_rmse <- order(mean_rmse_chain)
  # # selected_chain <- c(order_rmse >= nchain-1 | order_rmse <= 2, TRUE)
  # chains <- which(order_rmse >= nchain-1 | order_rmse <= 2)
  all_samples = ndpost+nskip
  fit <- bart_fit$fit

  all_data <- fit$getTrees(treeNums =1,
                           # chainNums = chains,
                           sampleNums = 1:all_samples)

  # i <- 1
  # all_data$ord_chain <- 0
  # for (c in chains){
  #   all_data$ord_chain[all_data$chain == c] = i
  #   i <- i+1
  # }
  # all_data$chain <- all_data$ord_chain

  first.split.data <- all_data %>%
    filter(n==ncol(bart_fit$yhat.train)) %>%
    dplyr::select(sample, var, chain)
  first.split.data$chain <- factor(first.split.data$chain)
  return(first.split.data)
}

.get.xlim <- function(ds, synthetic){
  if ((ds %in% c("echo_months", "breast_tumor") & !synthetic)){
    return(c(0.93, 1.07))
  } else {return(c(0.8, 1.2 ))}

}

.get_gelman_rubin <- function(rmse_mat){
  chains <- list()
  for (i in 1:ncol(rmse_mat)){
    chains[[i]] <- mcmc(rmse_mat[, i])
  }
  return(gelman.diag(mcmc.list(chains)))

}

.get_dir_fig <- function(n_tree, ds_name, restricted){
  bart_model <- ifelse(restricted,paste("trees",n_tree,"restricted", sep="_"), paste("trees",n_tree, sep="_"))
  dir_fig <- file.path("figures", "aistats",bart_model , ds_name)
  .check_create(dir_fig)
  return(dir_fig)

}

.get_dir_data <- function(n_tree, ds_name, restricted){
  bart_model <- ifelse(restricted,paste("trees",n_tree,"restricted", sep="_"), paste("trees",n_tree, sep="_"))
  dir_data <- file.path("data", "aistats", bart_model, ds_name)
  .check_create(dir_data)
  return(dir_data)

}

.get_fname <- function(nskip, ndpost, n,nchain,p,run,  synthetic=FALSE){
  fname <- paste("burn", nskip, "post", ndpost,"nchain",nchain,"run",run, sep = "_")
  if (synthetic){
    fname <- paste(fname, "synthetic", sep = "_")
  }
  if (p>1){
    fname <- paste(fname, "p",p, sep = "_")

  }
  # n <- get_data(ds_name = ds_name, n = n, p = p, synthetic = synthetic)[["n"]]
  fname <- paste(fname, "ndp", n, sep = "_")
  return(fname)
}

.get_plots_data <- function(data_train, data_test,n_tree, nskip, ndpost, nchain, fname, dir_data, run, restricted){

  fname_csv_rmse <- paste0(fname, "_rmse.csv")
  dir_csv_rmse <- file.path(dir_data, fname_csv_rmse)
  fname_csv_split <- paste0(fname, "_first_split.csv")
  dir_csv_split <- file.path(dir_data, fname_csv_split)

  if (file.exists(dir_csv_rmse) & file.exists(dir_csv_split)){
    rmse_mat <- read.csv(dir_csv_rmse, header = TRUE)
    rmse_mat <- rmse_mat[, 2:ncol(rmse_mat)]
    split_mat <- read.csv(dir_csv_split, header = TRUE)
    split_mat <- split_mat[, 2:ncol(split_mat)]
  } else{
    x.train <- get.features(data_train)
    y.train <- get.labels(data_train)
    data_train <- c()
    probs <-c(0.5, 0.1, 0.4,0.5)
    if (restricted){
      probs <-c(1-1e-5,1e-5,0,.5)
    }
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
    x.train <- c()
    y.train <- c()




    rmse_mat <- .get_rmse_mat(data_test = data_test, bart_fit = bart_fit,
                              ndpost = ndpost, nskip = nskip, nchain = nchain)
    write.csv(rmse_mat, dir_csv_rmse)

    split_mat <- .get.first.splits(data_test, bart_fit, ndpost, nskip)
    write.csv(split_mat, dir_csv_split)}
  split_mat$Chain <- split_mat$chain
  return(list(rmse_mat=rmse_mat, split_mat=split_mat))
}

.get.label.name<-function(ds_name){
  return(str_to_title(str_replace(ds_name, "_", " ")))
}

.test.gr <- function(){
  n_chains <- 15
  rmse_mat <- mvrnorm(n_chains, rep(0, 500), diag(rep(1, 500)))
  gr_same_dist <- .get_gelman_rubin(rmse_mat)

  print(paste("same dist: ", gr_same_dist[[1]][1]))

  rmse_mat <- mvrnorm(n_chains, c(rep(0, 250), rep(20, 250)), diag(c(rep(1, 250), rep(20, 250))))
  gr_different_dist <- .get_gelman_rubin(rmse_mat)
  print(paste("different dist: ", gr_different_dist[[1]][1]))

}

.get.real.n <- function(ds_name, n){
  data_all <- get_data(ds_name = ds_name, n = n, p = 1, synthetic=FALSE, seed = 42)
  data_train <- data_all[["train"]]
  y.train <- get.labels(data_train)
  n <- length(y.train)
  return(n)

}