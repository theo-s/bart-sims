library(MASS)
library(gclus)
library(dplyr)
library(Rforestry)

set.seed(3984938)
datasets_grid <- list()
# Simulated Datasets -----------------------------------------------------------

n <- 1e4
test_id <- (2e3+1):1e4
train_id <- 1:2e3

x <- matrix(runif(10 * n), nrow = n, ncol = 10)
x <- as.data.frame(x)

y <- 10*sin(pi*x[,1]*x[,2]) + 20*(x[,3] - .5)^2 + 10*x[,4] + 5*x[,5]
friedman_1 <- cbind(x, y)



datasets_grid[["Friedman_1"]] <- list(
  "train" = friedman_1[train_id, ] %>%
    dplyr::mutate(y = y + rnorm(nrow(friedman_1[train_id, ]), sd = 1)),
  "test" = friedman_1[test_id, ])

# XOR data =====================================================================
x <- matrix(rbinom(10 * n, size = 1,prob=.5), nrow = n, ncol = 10)
x <- as.data.frame(x)

y <- as.numeric(xor(x[,1],x[,2]))
xor_data <- cbind(x, y)

datasets_grid[["XOR_10d"]] <- list(
  "train" = xor_data[train_id, ] %>%
    dplyr::mutate(y = y + rnorm(nrow(xor_data[train_id, ]), sd = 1)),
  "test" = xor_data[test_id, ])



# Bayesian CART paper data =====================================================
outcome_func <- function(X) {
  if (X[1] <= 5 && X[2] %in% c(1,2)) {
    return(8)
  } else if (X[1] > 5 && X[2] %in% c(1,2)) {
    return(2)
  } else if (X[1] <= 3 && X[2] %in% c(3,4)) {
    return(1)
  } else if (X[1] <= 7 && X[2] %in% c(3,4)) {
    return(5)
  } else if (X[1] > 7 && X[2] %in% c(3,4)) {
    return(8)
  }
}

x <- data.frame(V1 = runif(n,min=0,max=10),
                V2 = sample(c(1:4), size = n,replace = TRUE),
                V3 = runif(n,min=0,max=10),
                V4 = runif(n,min=0,max=10),
                V5 = runif(n,min=0,max=10),
                V6 = runif(n,min=0,max=10),
                V7 = runif(n,min=0,max=10),
                V8 = runif(n,min=0,max=10),
                V9 = runif(n,min=0,max=10),
                V10 = runif(n,min=0,max=10))

y <- apply(x, MARGIN=1, FUN=outcome_func)
paper_data <- cbind(x,y)

datasets_grid[["PAPER_10d"]] <- list(
  "train" = paper_data[train_id, ] %>%
    dplyr::mutate(y = y + rnorm(nrow(paper_data[train_id, ]), sd = 2)),
  "test" = paper_data[test_id, ])
