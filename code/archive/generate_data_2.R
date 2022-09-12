library(MASS)
library(gclus)
library(Rforestry)
library(dplyr)

set.seed(3984938)
datasets_grid <- list()
# Simulated Datasets -----------------------------------------------------------

# Friedman 1 ===================================================================
n <- 1e4
x <- matrix(runif(10 * n), nrow = n, ncol = 10)
x <- as.data.frame(x)

y <- 10*sin(pi*x[,1]*x[,2]) + 20*(x[,3] - .5)^2 + 10*x[,4] + 5*x[,5]
friedman_1 <- cbind(x, y)

test_id <- 2001:1e4
train_id <- 1:200

x_med <- matrix(runif(10 * n), nrow = n, ncol = 10)
x_med <- as.data.frame(x_med)
colnames(x_med) <- paste0("X",1:10)

x_high <- matrix(runif(100 * n), nrow = n, ncol = 100)
x_high <- as.data.frame(x_high)
colnames(x_high) <- paste0("X",1:100)

datasets_grid[["Friedman_1_low_noise"]] <- list(
  "train" = friedman_1[train_id, ] %>%
    dplyr::mutate(y = y + rnorm(nrow(friedman_1[train_id, ]), sd = 1)),
  "test" = friedman_1[test_id, ])

datasets_grid[["Friedman_1_med_noise"]] <- list(
  "train" = friedman_1[train_id, ] %>%
    dplyr::mutate(y = y + rnorm(nrow(friedman_1[train_id, ]), sd = 3)),
  "test" = friedman_1[test_id, ])

datasets_grid[["Friedman_1_high_noise"]] <- list(
  "train" = friedman_1[train_id, ] %>%
    dplyr::mutate(y = y + rnorm(nrow(friedman_1[train_id, ]), sd = 5)),
  "test" = friedman_1[test_id, ])

# Generate varying dimension data

datasets_grid[["Friedman_1_low_d"]] <- list(
  "train" = friedman_1[train_id, ] %>%
    dplyr::mutate(y = y + rnorm(nrow(friedman_1[train_id, ]), sd = 1)),
  "test" = friedman_1[test_id, ])

friedman_1_med <- cbind(x_med,x, y)

datasets_grid[["Friedman_1_med_d"]] <- list(
  "train" = friedman_1_med[train_id, ] %>%
    dplyr::mutate(y = y + rnorm(nrow(friedman_1_med[train_id, ]), sd = 1)),
  "test" = friedman_1_med[test_id, ])

friedman_1_high <- cbind(x_high,x, y)

datasets_grid[["Friedman_1_high_d"]] <- list(
  "train" = friedman_1_high[train_id, ] %>%
    dplyr::mutate(y = y + rnorm(nrow(friedman_1_high[train_id, ]), sd = 1)),
  "test" = friedman_1_high[test_id, ])

# Friedman 3 ===================================================================
# Error SD selected to roughly give signal to noise ratio of 3:1
#
y <- atan( (x[,2]*x[,3] - (1/(x[,2]*x[,4]) )) / x[,1])

# noise <- rnorm(n)
#
# ratio <- sqrt(var(y)/(3*var(noise)))
#
# y <- y + ratio * noise

friedman_3 <- cbind(x, y)


datasets_grid[["Friedman_3_low_noise"]] <- list(
  "train" = friedman_3[train_id, ] %>%
    dplyr::mutate(y = y + rnorm(nrow(friedman_3[train_id, ]), sd = sqrt(var(y)/(3)))),
  "test" = friedman_3[test_id, ])

datasets_grid[["Friedman_3_med_noise"]] <- list(
  "train" = friedman_3[train_id, ] %>%
    dplyr::mutate(y = y + rnorm(nrow(friedman_3[train_id, ]), sd = sqrt(var(y)/(2)))),
  "test" = friedman_3[test_id, ])

datasets_grid[["Friedman_3_high_noise"]] <- list(
  "train" = friedman_3[train_id, ] %>%
    dplyr::mutate(y = y + rnorm(nrow(friedman_3[train_id, ]), sd = sqrt(var(y)/(1)))),
  "test" = friedman_3[test_id, ])

# Generate varying dimension data

datasets_grid[["Friedman_3_low_d"]] <- list(
  "train" = friedman_3[train_id, ] %>%
    dplyr::mutate(y = y + rnorm(nrow(friedman_3[train_id, ]), sd = sqrt(var(y)/(3)))),
  "test" = friedman_3[test_id, ])

friedman_3_med <- cbind(x_med,x, y)

datasets_grid[["Friedman_3_med_d"]] <- list(
  "train" = friedman_3_med[train_id, ] %>%
    dplyr::mutate(y = y + rnorm(nrow(friedman_3_med[train_id, ]), sd = sqrt(var(y)/(3)))),
  "test" = friedman_3_med[test_id, ])

friedman_3_high <- cbind(x_high,x, y)

datasets_grid[["Friedman_3_high_d"]] <- list(
  "train" = friedman_3_high[train_id, ] %>%
    dplyr::mutate(y = y + rnorm(nrow(friedman_3_high[train_id, ]), sd = sqrt(var(y)/(3)))),
  "test" = friedman_3_high[test_id, ])

