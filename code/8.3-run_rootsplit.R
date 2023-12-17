library(dbarts)
library(dplyr)
library(ggplot2)
library(Metrics)
library(tidyverse)
library(optparse)
library(coda)
library(gridExtra)
library(ggridges)
library(latex2exp) 
library(scales)
library(stringr)


.get_rmse_mat = function(data_test, bart_fit, ndpost, nskip, nchain){
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

.get.first.splits = function(bart_fit, ndpost, nskip) {


  chains <- 1:bart_fit$fit$control@n.chains
  all_samples = ndpost+nskip
  fit <- bart_fit$fit
  
  all_data <- fit$getTrees(treeNums = 1:bart_fit$call$ntree,
                           chainNums = chains,
                           sampleNums = (nskip+1):all_samples)
  
  first.split.data <- all_data %>%
    filter(n==ncol(bart_fit$yhat.train)) %>%
    dplyr::select(sample, tree, var, chain, value) %>% 
    group_by(chain) %>% 
    summarise(pct_correct = length(which(var == 1))/length(var),
              num_changes = length(unique(value))-1)
  return(first.split.data)
}


# Experiment 2 =================================================================
exp = "1"

df_all = data.frame()

for (exp in c("1","2")) {
  for (n in c(1e2, 1e4)) {
    print(n)
    x.train <- data.frame(matrix(rnorm(10*n), ncol = 10))
    if (exp == "1") {
      y.train <- ifelse(x.train$X1 > .1,
                        ifelse(x.train$X2 > .7, 12, 9),
                        ifelse(x.train$X3 > -.2, 3, 6))+rnorm(n, sd = 1)
    } else if (exp == "2") {
      y.train <- ifelse(x.train$X1 > .1,
                        ifelse(x.train$X2 > .7, 
                               ifelse(x.train$X4 > .1, 12, 9),
                               ifelse(x.train$X5 > .1, 3, 4)),
                        ifelse(x.train$X3 > -.2, 
                               ifelse(x.train$X6 > .2, 3, 4), 
                               ifelse(x.train$X7 > .15, 2, 1)))+rnorm(n, sd = 1)
    }
    
    data_train <- c()
    probs <-c(0.5, 0.1, 0.4,0.5)
    
    ndpost = 500
    nskip = 0
    nchain = 1
    run = 1
    TEXT_SIZE=22
    
    names(probs) <- c("birth_death", "change","swap","birth")
    bart_fit <- bart(x.train = x.train,
                     y.train = y.train,
                     keeptrees = TRUE,
                     verbose = TRUE,
                     nskip = 0,
                     keepevery = 1,
                     ntree = 1,
                     ndpost = ndpost+nskip,
                     nchain = 100,
                     seed=run,
                     proposalprobs=probs)
    
    
    split_data <- .get.first.splits( bart_fit, ndpost, nskip)
    #split_data$Chain = split_data$chain
    
    #split_data$Chain <- factor(split_data$Chain)
    #split_data$var[split_data$var == -1] <- "Empty Tree" 
    
    
    #split_data$Feature <- factor(split_data$var)
    split_data$Size = n
    split_data$exp = exp
    if (nrow(df_all) == 0) {
      df_all = split_data
    } else {
      df_all = rbind(df_all, split_data)
    }
  }
}

library(xtable)
df_all %>% 
  filter(exp == "1") %>% 
  group_by(Size) %>% 
  summarise(`Percent Correct` = mean(pct_correct),
            `Percent Correct (SE)` = sd(pct_correct)/10,
            `Changes to root split` = mean(num_changes),
            `Changes to root split (SE)` = sd(num_changes)/10) %>% 
  xtable()

df_all %>% 
  filter(exp == "1") %>% 
  mutate(SampleSize = factor(Size)) %>% 
  ggplot(aes(x = pct_correct, fill = SampleSize)) +
  theme_bw()+
  labs(x = "Percentage of correct initial splits", title = "DGP 1")+
  geom_density() -> p1
ggsave(filename = "results/figures/coverage/experiment3_dgp1.pdf", height = 2.5, width = 3)
df_all %>% 
  filter(exp == "2") %>% 
  mutate(SampleSize = factor(Size)) %>% 
  ggplot(aes(x = pct_correct, fill = SampleSize)) +
  theme_bw()+
  labs(x = "Percentage of correct initial splits", title = "DGP 2")+
  geom_density() -> p2
ggsave(filename = "results/figures/coverage/experiment3_dgp2.pdf", height = 2.5, width = 3)


p_final = grid.arrange(p1+theme(legend.position ="none"),
                       p2+theme(axis.title.y=element_blank()), 
                       widths =c(4,5.5),
                       ncol = 2)
ggsave(p_final, filename = "results/figures/coverage/experiment3_all.pdf", height = 5, width = 8)
