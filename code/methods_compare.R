library(randomForest)
library(dbarts)
library(purrr)
library(optparse)
library(Metrics)
library(rpart)
source("code/ds_reg.R")

.check_create <- function(dir){
  if (dir.exists(dir)){
    return()
  }
  dir.create(dir, recursive = TRUE)
  
}

option_list = list(
  make_option(c("-n", "--n_d_p"), type="double", default=Inf,
              help="number of data points", metavar="double"),
  make_option(c("-p", "--n_features"), type="integer", default=40,
              help="number of feature", metavar="integer")
);


main <- function(){
parser <- OptionParser(option_list=option_list);
args <- parse_args(parser);

datasets <- c("california_housing", "echo_months", "satellite_image", "breast_tumor")

n <- args$n_d_p
p <- args$n_features
    results_dir <- "results"
  .check_create(results_dir)
for (ds_name in datasets){
  df <- compare_methods(ds_name, n)
  fname <- file.path(results_dir, paste0(ds_name, ".csv"))
  write.csv(df, fname)}

}
.get.bart.rmse <- function(bart_fit, x.test, y.test){
  pdctns_chains <- predict(bart_fit, x.test)
  average_preds <- colMeans(pdctns_chains)
  return(rmse(y.test, average_preds))
  
}

.get.cart.rmse <- function(cart_fit, x.test, y.test){
  preds <- predict(cart_fit, x.test)
  return(rmse(preds, y.test))
  
}

compare_methods <- function(ds_name, n){
  
  df <- data.frame(matrix(ncol = 5, nrow = 0))
  

  probs <- c(1-1e-5,1e-5,0,.5)
  names(probs) <- c("birth_death", "change","swap","birth")
  
  for (seed in 1:5){
    data_all <- get_data(ds_name, n, seed=seed)
    data_train <- data_all[["train"]]
    data_test <- data_all[["test"]]
    
    x.train <- get.features(data_train)
    y.train <- get.labels(data_train)
    
    x.test <- get.features(data_test)
    y.test <- get.labels(data_test)
    

    
  
  B_CART_Restricted <-  bart(x.train = x.train,
                            y.train = y.train,
                             ntree = 1, proposalprobs=probs,
                            keeptrees=TRUE,
                            verbose = FALSE,
                            nchain=8)
  
  B_CART <-  bart(x.train = x.train,
                             y.train = y.train,
                             ntree = 1,
                  keeptrees=TRUE,
                  verbose = FALSE, nchain=8)
  BART <-  bart(x.train = x.train,
                  y.train = y.train,
                keeptrees=TRUE,
                verbose = FALSE, nchain=8)
  
  
  # CART <- randomForest(x=x.train, y=y.train, ntree=1)

  data <- cbind(x.train, y.train)
  CART <- rpart(y.train ~ .,data=data,
                ,method="anova")

  CART_pruned<- prune(CART,
                , cp= CART$cptable[which.min(CART$cptable[,"xerror"]),"CP"])
  RF <- randomForest(x=x.train, y=y.train) 

  rmse_vec <- c(.get.bart.rmse(B_CART_Restricted,x.test,y.test),
                .get.bart.rmse(B_CART,x.test,y.test),
                .get.bart.rmse(BART,x.test,y.test),
                .get.cart.rmse(CART_pruned,x.test,y.test),
                .get.cart.rmse(RF,x.test,y.test))
  df <- rbind(df, rmse_vec)
  
  
  
  }
  colnames(df) <- c("B-CART (Restricted)", "B-CART", "BART", "CART", "RF")
  results <- rbind(apply(df, 2, mean), apply(df, 2, sd))
  rownames(results) <- c("Mean RMSE", "SD")
  return(round(t(results), 2))


  
  
}

if (getOption('run.main', default=TRUE)) {
  main()
}