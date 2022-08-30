library(dbarts)
library(dplyr)
library(ggplot2)

set.seed(3984938)

# Simulated Datasets -----------------------------------------------------------
# Friedman 1
n <- 1e4
x <- matrix(runif(10 * n), nrow = n, ncol = 10)
x <- as.data.frame(x)

y <- 10*sin(pi*x[,1]*x[,2]) + 20*(x[,3] - .5)^2 + 10*x[,4] + 5*x[,5] + rnorm(n, sd = 1)
friedman_1 <- cbind(x, y)

test_id <- 1001:3000
train_id <- 3001:10000

data_all <- list(
  "train" = friedman_1[train_id, ],
  "test" = friedman_1[test_id, ])


data_train <- data_all[["train"]]
data_test <- data_all[["test"]]

bart_fit_2 <- bart(x.train = data_train %>% dplyr::select(-last_col()),
                   y.train = data_train %>% dplyr::select(last_col()) %>% .[,1],
                   keeptrees = TRUE,
                   verbose = TRUE,
                   nskip = 3000,
                   keepevery = 1,
                   ntree = 50,
                   ndpost = 1000)

plot_var_proportions(bart_fit_2, "Friedman 1, n=1e4")
ggsave(file = "~/Desktop/bart_props.pdf", width = 6, height = 5)

plot_num_leaves(bart_fit_2, "Friedman 1, n=1e4")
ggsave(file = "~/Desktop/bart_leaves.pdf", width = 6, height = 5)

