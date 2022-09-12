library("ucidata")
library("pmlbr")
library("mlbench")
library("klaR")


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


get_data <- function(ds_name, n=NULL, p=1, synthetic=FALSE, seed=42){
  if (ds_name == "abalone"){
    ds <- abalone
  } else if (ds_name == "satellite_image"){
    ds <- fetch_data("294_satellite_image")
  } else if (ds_name == "echo_months"){
    ds <- fetch_data("1199_BNG_echoMonths") 
  } else if (ds_name == "breast_tumor"){
    ds <- fetch_data("1201_BNG_breastTumor") 
  } else if (ds_name == "california_housing"){
    ds <- fetch_data("537_houses") 
  } else if (ds_name == "diabetes" ){
    ds <- PimaIndiansDiabetes
    ds$diabetes <- ifelse(PimaIndiansDiabetes$diabetes == "pos",1 ,0)
  } else if (startsWith(ds_name,"friedman")){
    setting <- as.integer(substr(ds_name, nchar(ds_name), nchar(ds_name)+1))
    ds <- friedman.data(setting = setting, p = p, samplesize = n)
  }
  if (synthetic){
    friedman_data <- 10 * sin(pi * ds[, 1] * ds[, 2]) + 20 * (ds[, 3] - 0.5) ^ 2 + 10 * ds[, 4] + 5 * ds[, 5]
    ds[,-1] <- friedman_data + 2 * rnorm(length(friedman_data))
  }

  n_ds <- dim(ds)[1]
  set.seed(seed)
  test_id <- sample(n_ds, ceiling(0.1 * n_ds))
  # test_id <- seq(1, 0.1 * n_ds)
  train_id <- setdiff(1:n_ds, test_id)
  n_ds_train <- length(train_id)

  if (n<n_ds_train){
    set.seed(seed)
    train_id <- sample(train_id, n)
    n_ds_train <- n

  }
  if (p > 1){
    ratio_to_add <- p-1
    n_features <- ncol(ds)-1
    cols_to_add <- sample(n_features, ceiling(n_features * ratio_to_add), replace=TRUE)
    for (col in cols_to_add){
    random_col <- ds[, col]
    permuted_random_col <- random_col[sample(length(random_col))]
    ds <- cbind(permuted_random_col, ds)
    }
  }


   data_all <- list(
     "train" = ds[train_id, ],
     "test" = ds[test_id, ],
      "n" = n_ds_train)
   return(data_all)
 }