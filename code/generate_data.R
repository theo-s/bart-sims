library(MASS)
library(gclus)
library(dplyr)
library(Rforestry)

diabetes <- read.csv("data/pima-indians-diabetes.csv")
abalone <- read.csv("data/abalone.csv")

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

datasets_grid[["Friedman_1"]] <- list(
  "train" = friedman_1[train_id, ] %>%
    dplyr::mutate(y = y + rnorm(nrow(friedman_1[train_id, ]), sd = 1)),
  "test" = friedman_1[test_id, ])

# Friedman 2 ===================================================================
# Error SD selected to roughly give signal to noise ratio of 3:1
x1 <- runif(n, min = 0, max = 100)
x2 <- runif(n, min = 40*pi, max = 560*pi)
x3 <- runif(n, min = 0.01, max = 1)
x4 <- runif(n, min = 1, max = 11)

x <- data.frame(x1, x2, x3, x4)

y <- x[, 1] ^ 2 + (x[, 2] * x[, 3] - (1 / (x[, 2] * x[, 3])) ^ 2) ^ (.5)

noise <- rnorm(n)

ratio <- sqrt(var(y)/(3*var(noise)))

y <- y + ratio * noise

friedman_2 <- cbind(x, y)
# summary(friedman_2)
#
datasets_grid[["Friedman_2"]] <- list(
  "train" = friedman_2[train_id, ],
  "test" = friedman_2[test_id, ])

# Friedman 3 ===================================================================
# Error SD selected to roughly give signal to noise ratio of 3:1
#
y <- atan( (x[,2]*x[,3] - (1/(x[,2]*x[,4]) )) / x[,1])

noise <- rnorm(n)

ratio <- sqrt(var(y)/(3*var(noise)))

# y <- y + ratio * noise

friedman_3 <- cbind(x, y)


datasets_grid[["Friedman_3"]] <- list(
  "train" = friedman_3[train_id, ] %>%
    dplyr::mutate(y = y + ratio * noise[train_id]),
  "test" = friedman_3[test_id, ])


# Abalone ======================================================================
abalone_onehot_translator <- onehot::onehot(abalone, stringsAsFactors = TRUE)
abalone <- as.data.frame(predict(abalone_onehot_translator, abalone))
n <- nrow(abalone)

colnames(abalone)[ncol(abalone)] <- "y"
colnames(abalone)[1:(ncol(abalone) - 1)] <- paste0("x", 1:(ncol(abalone) - 1))

test_id <- sort(sample(n, size = round(.5*n))); length(test_id)
train_id <- (1:n)[!(1:n) %in% test_id]; length(train_id)


datasets_grid[["Abalone"]] <- list(
  "train" = abalone[train_id, ],
  "test" = abalone[test_id, ])

# Diabetes =====================================================================
n <- nrow(diabetes)


colnames(diabetes)[ncol(diabetes)] <- "y"
colnames(diabetes)[1:(ncol(diabetes) - 1)] <- paste0("x", 1:(ncol(diabetes) - 1))

test_id <- sort(sample(n, size = round(.5*n))); length(test_id)
train_id <- (1:n)[!(1:n) %in% test_id]; length(train_id)

datasets_grid[["Diabetes"]] <- list(
  "train" = diabetes[train_id, ],
  "test" = diabetes[test_id, ])
