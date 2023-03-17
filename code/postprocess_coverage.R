library(ggplot2)
library(dplyr)

# Should be one of sum,high, low, and piecewise
dgp = "low"

calc_winkler <- function(Y,
                         ci.upper,
                         ci.lower) {
  if (length(Y) != length(ci.lower) | length(Y) != length(ci.upper)) {
    stop("Lengths of inputs don't match")
  }
  
  # First calculate if the CI covers each observation
  over <- ifelse(Y > ci.upper,
                 Y - ci.upper,0)
  
  under <- ifelse(Y < ci.lower,
                  ci.lower - Y,0)
  
  # Winkler 
  winkler <- ifelse(over > 0,
                    ci.upper-ci.lower + 40*over, # If over upper limit, pay 2 / .05 * over penalty
                    ifelse(under > 0,
                           ci.upper-ci.lower + 40*over, # If below lower limit, pay 2 / .05 * under penalty
                           ci.upper-ci.lower # Otherwise just pay interval length
                           )
                    )
  return(mean(winkler))
  
}

results <- data.frame(n = NA,
                      nchain = NA,
                      run = NA,
                      exp = 1,
                      rmse = NA,
                      cov = NA,
                      wink = NA)


# file = "dgp_high_run_8_n_10000_coverage.RDS"
# f <- readRDS(file = paste0("results/coverage/",file))

for (file in dir("results/coverage/")) {
  if (grepl(pattern = dgp, x = file)) {
    print(file)
    f <- readRDS(file = paste0("results/coverage/",file))
    
    for (iter in 1:3) {
      results <- rbind(results, 
                       c(f[[iter]]$n, f[[iter]]$nchain, f[[iter]]$run, 1,
                         f[[iter]]$rmse, f[[iter]]$empirical_cov, calc_winkler(Y = f[[iter]]$true_y,
                                                                               ci.upper = f[[iter]]$ci_upper,
                                                                               ci.lower = f[[iter]]$ci_lower)))
    }
  }
}

results[-1,] %>% 
  filter(n>100) %>% 
  group_by(n, nchain, exp) %>% 
  summarise(mean_rmse = mean(rmse),
            mean_coverage = mean(cov),
            mean_wink = mean(wink)) %>% 
  select(n, nchain, mean_wink) %>% 
  mutate(nchain = as.factor(nchain)) %>% 
  ggplot(aes(x = n, y= mean_wink, color = nchain))+
  geom_line()+
  labs(y = "Mean Winkler Score (1000 test pts)", title = paste0("DGP: ",dgp))+
  theme_classic()
ggsave(filename = paste0("results/figures/coverage/",dgp,"winkler.pdf"), height = 4, width = 6)

results[-1,] %>% 
  filter(n>100) %>% 
  group_by(n, nchain, exp) %>% 
  summarise(mean_rmse = mean(rmse),
            mean_coverage = mean(cov),
            mean_wink = mean(wink)) %>% 
  select(n, nchain, mean_coverage) %>% 
  mutate(nchain = as.factor(nchain)) %>% 
  ggplot(aes(x = n, y= mean_coverage, color = nchain))+
  geom_line()+
  labs(y = "Empirical Coverage (1000 test pts)", title = paste0("DGP: ",dgp))+
  geom_hline(yintercept = .95,linetype="dashed", 
             color = "red")+
  theme_classic()
ggsave(filename = paste0("results/figures/coverage/",dgp,"coverage.pdf"), height = 4, width = 6)

