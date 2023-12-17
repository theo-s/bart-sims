# If needed, install vthemes from Yu-Group website
# devtools::install_github("Yu-Group/vthemes")
library(ggplot2)
library(dplyr)
library(vthemes)

all_plots = list()

idx=1
for (dgp_name in c("linear_add", "smooth_add")) {
  
  res <- read.csv(paste0("results/theory_results3/dgp_",dgp_name,"_components_analysis.csv"))
  
  if (dgp_name == "linear_add") {
    name_use = "Linear"
  } else if (dgp_name == "smooth_add") {
    name_use = "Smooth"
  } else {
    name_use = "Step Function"
  }
  
  res %>% 
    mutate(Trees = rep(c(1:10), nrow(res)/10)) -> res
  
  results = res
    
  results[-1,] %>%
    group_by(n, Trees) %>%
    summarise(reps_completed = max(run)) -> num_runs
  
  print(num_runs)
    
  results %>%
    filter(n>1000) %>%
    group_by(n,run) %>%
    #mutate(rmse_one = RMSE[Trees==5]) %>% 
    #ungroup() %>% 
    mutate(rmse = RMSE) %>%
    group_by(n, Trees) %>%
    summarise(mean_rmse = mean(rmse),
              mean_coverage = mean(Coverage),
              sd_coverage = sd(Coverage)/10,
              mean_rmse = mean(rmse),
              sd_rmse = sd(rmse)/10) %>%
    group_by(n) %>%
    dplyr::select(n, Trees, mean_rmse,sd_rmse) %>%
    mutate(n = as.factor(paste0(n/1000,"K"))) %>%
    ggplot(aes(x = Trees, y= mean_rmse, color = n))+
    geom_line()+
    geom_point(aes(color = n))+
    geom_errorbar(aes(ymin = mean_rmse - 1.96*sd_rmse, ymax = mean_rmse + 1.96*sd_rmse, color = n), width = 0) +
    labs(y = "RMSE", title = paste0(name_use))+
    scale_color_manual(values = c("10K"="turquoise1", "20K"="steelblue1", "50K"="royalblue2", "100K"="royalblue3"))+
    vthemes::theme_vmodern() +
    theme(axis.line = element_line(color='black'),
          panel.background = element_rect(fill = 'white', color = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          text = element_text(family = "Times"),
          axis.title=element_text(size=6)) +
    #axis(side=2, at=1:10, labels = TRUE)+
    scale_x_continuous(breaks = 1:10)+
    #scale_x_continuous(labels = function(x){return(paste0(as.character(x/1e3), "K"))})+
    ylim(0,1.05) -> plot_i
  
  ggsave(plot_i,filename = paste0("results/figures/coverage/",dgp_name,"components_rmse.pdf"), height = 2.5, width = 3)
  all_plots[[idx]] = plot_i
  idx = idx + 1
}


p_final = grid.arrange(all_plots[[1]]+theme(legend.position ="none"),
                       all_plots[[2]]+theme(axis.title.y=element_blank()),
                       widths = c(4,4.75),
                       ncol = 2)
ggsave(p_final,filename = paste0("results/figures/coverage/all_components_rmse.pdf"), height = 2.5, width = 8)


# Now do coverages
idx=1
for (dgp_name in c("linear_add", "smooth_add")) {
  
  res <- read.csv(paste0("results/theory_results3/dgp_",dgp_name,"_components_analysis.csv"))
  
  if (dgp_name == "linear_add") {
    name_use = "Linear"
  } else if (dgp_name == "smooth_add") {
    name_use = "Smooth"
  } else {
    name_use = "Step Function"
  }
  
  res %>% 
    mutate(Trees = rep(c(1:10), nrow(res)/10)) -> res
  
  results = res
  
  results[-1,] %>%
    group_by(n, Trees) %>%
    summarise(reps_completed = max(run)) -> num_runs
  
  print(num_runs)
  
  results %>%
    filter(n>1000) %>%
    group_by(n,run) %>%
    #mutate(rmse_one = RMSE[Trees==5]) %>% 
    #ungroup() %>% 
    mutate(rmse = RMSE) %>%
    group_by(n, Trees) %>%
    summarise(mean_rmse = mean(rmse),
              mean_coverage = mean(Coverage),
              sd_coverage = sd(Coverage)/10,
              mean_rmse = mean(rmse),
              sd_rmse = sd(rmse)/10) %>%
    group_by(n) %>%
    dplyr::select(n, Trees, mean_coverage,sd_coverage) %>%
    mutate(n = as.factor(paste0(n/1000,"K"))) %>%
    ggplot(aes(x = Trees, y= mean_coverage, color = n))+
    geom_line()+
    geom_point(aes(color = n))+
    geom_errorbar(aes(ymin = mean_coverage - 1.96*sd_coverage, ymax = mean_coverage + 1.96*sd_coverage, color = n), width = 0) +
    labs(y = "Empirical Coverage", title = paste0(name_use))+
    scale_color_manual(values = c("10K"="turquoise1", "20K"="steelblue1", "50K"="royalblue2", "100K"="royalblue3"))+
    vthemes::theme_vmodern() +
    theme(axis.line = element_line(color='black'),
          panel.background = element_rect(fill = 'white', color = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          text = element_text(family = "Times"),
          axis.title=element_text(size=6)) +
    #axis(side=2, at=1:10, labels = TRUE)+
    scale_x_continuous(breaks = 1:10)+
    scale_y_continuous(labels = function(x){return(paste0(as.character(x*100), "%"))}, limits = c(.65, 1.01))+
    geom_hline(yintercept = .95,linetype="dashed",
                 color = "red") -> plot_i
  
  ggsave(plot_i,filename = paste0("results/figures/coverage/",dgp_name,"components_coverage.pdf"), height = 2.5, width = 3)
  all_plots[[idx]] = plot_i
  idx = idx + 1
}


p_final = grid.arrange(all_plots[[1]]+theme(legend.position ="none"),
                       all_plots[[2]]+theme(axis.title.y=element_blank()),
                       widths = c(4,4.75),
                       ncol = 2)
ggsave(p_final,filename = paste0("results/figures/coverage/all_components_coverage.pdf"), height = 2.5, width = 8)

