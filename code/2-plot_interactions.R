# If needed, install vthemes from Yu-Group website
# devtools::install_github("Yu-Group/vthemes")
library(ggplot2)
library(dplyr)
library(vthemes)

# Plot the interaction term results --------------------------------------------

all_plots = list()
cov_plots = list()


results <- data.frame(n = NA,
                      nchain = NA,
                      run = NA,
                      exp = 1,
                      rmse = NA,
                      cov = NA)
for (dgp in c("lss2")) {
  
  
  if(dgp=="lss2") {
    exp = 2
  } else if(dgp=="lss3") {
    exp = 3
  } else if(dgp=="lss5") {
    exp = 5
  }
  
  for (file in dir("results/coverage5/")) {
    if (grepl(pattern = dgp, x = file)) {
      #print(file)
      f <- readRDS(file = paste0("results/coverage5/",file))
      if (grepl(pattern = "knn", x = file)) {
        estimator = 1
      } else {
        estimator = 2
      }
      
      for (iter in 1:length(f)) {
        n_i = f[[1]]$n
        run_i = f[[1]]$run
        nchain_i =f[[1]]$nchain
        if (iter == 1) {
          results <- rbind(results,
                           c(n_i, nchain_i, run_i, estimator,
                             f[[iter]]$rmse, f[[iter]]$empirical_cov))
        } else if (iter == 2) {
          results <- rbind(results,
                           c(n_i, 2, run_i, estimator,
                             f[[iter]]$rmse, f[[iter]]$coverage))
        } else if (iter == 3) {
          results <- rbind(results,
                           c(n_i, 5, run_i, estimator,
                             f[[iter]]$rmse, f[[iter]]$coverage))
        }
        
      }
    }
  }
}


  
  
  results[-1,] %>%
    group_by(n, nchain, exp) %>%
    summarise(reps_completed = max(run)) -> num_runs
  print(dgp)
  print(num_runs)
  
  results[-1,] %>%
    filter(n>1000) %>%
    mutate(`Interaction Count` = exp) %>% 
    filter(nchain == 1) %>% 
    group_by(n, nchain, run) %>%
    filter(n() > 1) %>% 
    mutate(rmse_one = rmse[`Interaction Count`==1]) %>% 
    ungroup() %>% 
    mutate(rmse = rmse / rmse_one) %>%
    group_by(n, nchain, `Interaction Count`) %>%
    summarise(mean_rmse = mean(rmse),
              mean_coverage = mean(cov),
              sd_coverage = sd(cov)/10,
              mean_rmse = mean(rmse),
              sd_rmse = sd(rmse)/10) %>%
    group_by(n) %>%
    dplyr::select(n, `Interaction Count`, mean_rmse,sd_rmse) %>%
    mutate(`Interactions` = as.factor(`Interaction Count`)) %>%
    ggplot(aes(x = n, y= mean_rmse, color = `Interactions`))+
    geom_line(aes(linetype = `Interactions`))+
    geom_point(aes(color = `Interactions`))+
    scale_linetype_manual(values = c("2" = "solid", "3" = "solid", "5" = "solid"),)+
    geom_errorbar(aes(ymin = mean_rmse - 1.96*sd_rmse, ymax = mean_rmse + 1.96*sd_rmse, color = `Interactions`), width = 0) +
    labs(y = "Relative RMSE")+
    scale_color_manual(values = c("2"="turquoise1", "3"="steelblue1", "5"="royalblue2"))+
    vthemes::theme_vmodern() +
    theme(axis.line = element_line(color='black'),
          panel.background = element_rect(fill = 'white', color = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none",
          text = element_text(family = "Times"),
          panel.border = element_blank(),
          axis.title=element_text(size=6))+
    scale_x_continuous(labels = function(x){return(paste0(as.character(x/1e3), "K"))})+
    ylim(0,.45) -> plot_i
  
ggsave(plot_i,filename = paste0("results/figures/coverage/xor_knn_rmse.pdf"), height = 2.5, width = 4)



