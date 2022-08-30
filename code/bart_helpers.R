library(MASS)
library(ggplot2)
library(dplyr)
library(caret)
library(pdp)
library(iml)
library(dbarts)
library(TTR)
library(reshape)
library(viridis)
library(ggrepel)

plot_var_proportions <- function(bart_fit, data_name = "") {
  fit <- bart_fit$fit
  count_totals <- apply(bart_fit$varcount, 1, sum)
  var_proportions <- bart_fit$varcount/ count_totals
  var_proportions <- var_proportions %>% as.data.frame()
  head(var_proportions)
  # cum_mean <- function(x) {cumsum(x) / seq_along(x)}
  # var_proportions_smoothed <- apply(var_proportions, 2, cum_mean)
  # var_proportions_smoothed <- var_proportions_smoothed %>% as.data.frame()
  # head(var_proportions_smoothed)
  #
  # var_proportions_smoothed$iter <- (1:nrow(var_proportions_smoothed))
  # colnames(var_proportions_smoothed) <- c(colnames(data_train %>%
  #                                                    dplyr::select(-last_col())), "iter")
  #
  # var_proportions <- melt(data = var_proportions_smoothed, id.vars = c("iter"))
  ends <- melt(data.frame(iter =nrow(var_proportions), var_proportions[nrow(var_proportions),]),
               id.vars = c("iter"))

  data.frame(iter = 1:nrow(var_proportions), var_proportions) %>%
    melt(id.vars = c("iter")) %>%
    ggplot(aes(x = iter, y = value, color = variable))  +
    geom_line() +
    theme_bw() +
    ggtitle(label = paste0("BART Variable Splitting proportion - ", data_name))  +
    labs(x = "MCMC Iteration", y = "Proportion of all splits",
         color = "Variable")+
    geom_text_repel(
      aes(label = variable), data = ends,
      color = "black", size = 3
    )+
    scale_color_viridis_d()
}

get_first_splits <- function(bart_fit) {
  fit = bart_fit$fit

  all_data <- fit$getTrees(treeNums =1,
                           chainNums = 1,
                           sampleNums = 1:bart_fit$call$ndpost)

  all_data %>%
    filter(n==ncol(bart_fit$yhat.train)) %>%
    dplyr::select(sample, var) -> samples
  return(samples$var)
}

plot_num_leaves <- function(bart_fit, data_name = "") {
  fit <- bart_fit$fit

  all_data <- fit$getTrees(treeNums = 1:(bart_fit$call$ntree), chainNums = 1,
                           sampleNums = 1:(round((bart_fit$call$ndpost)/bart_fit$call$keepevery)))

  all_data %>%
    dplyr::select(var, tree, sample) %>%
    filter(var < 0) %>%
    group_by(sample, tree) %>%
    summarise(numLeaves = n()) %>%
    group_by(sample) %>%
    summarise(numLeaves = mean(numLeaves)) -> leaf_counts

  leaf_counts %>%
    ggplot(aes(x = sample, y = numLeaves))  +
    geom_line() +
    theme_bw() +
    ggtitle(label = paste0("BART Number of Leaves Distribution -", data_name))  +
    labs(x = "MCMC Iteration", y = paste0("Average Number of Leaves (",bart_fit$call$ntree," trees)"))
}
