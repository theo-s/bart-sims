# If needed, install vthemes from Yu-Group website
# devtools::install_github("Yu-Group/vthemes")
library(ggplot2)
library(dplyr)
library(vthemes)

all_plots = list()

res <- read.csv("results/theory_results/dgp_sum_components_analysis.csv")

res %>% 
  mutate(Trees = rep(c(9,10,11), nrow(res)/3)) -> res

results = res
  
results[-1,] %>%
  group_by(n, Trees) %>%
  summarise(reps_completed = max(run)) -> num_runs

print(num_runs)
  
results %>%
  filter(n>100) %>%
  group_by(n,run) %>%
  mutate(rmse_one = RMSE[Trees==9]) %>% 
  ungroup() %>% 
  mutate(rmse = RMSE / rmse_one) %>%
  group_by(n, Trees) %>%
  summarise(mean_rmse = mean(rmse),
            mean_coverage = mean(Coverage),
            sd_coverage = sd(Coverage)/10,
            mean_rmse = mean(rmse),
            sd_rmse = sd(rmse)/10) %>%
  group_by(n) %>%
  select(n, Trees, mean_rmse,sd_rmse) %>%
  mutate(Trees = as.factor(Trees)) %>%
  ggplot(aes(x = n, y= mean_rmse, color = Trees))+
  geom_line(aes(linetype = Trees))+
  geom_point(aes(color = Trees))+
  scale_linetype_manual(values = c("9" = "dashed", "10" = "solid", "11" = "solid"))+
  geom_errorbar(aes(ymin = mean_rmse - 1.96*sd_rmse, ymax = mean_rmse + 1.96*sd_rmse, color = Trees), width = 0) +
  labs(y = "RMSE/RMSE(1 chain) (1000 test pts)", title = paste0("Additive generating model: 10 components"))+
  scale_color_manual(values = c("9"="turquoise1", "10"="steelblue1", "11"="royalblue2"))+
  vthemes::theme_vmodern() +
  theme(axis.line = element_line(color='black'),
        panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title=element_text(size=6))+
  ylim(.7,1.1) -> plot_i

ggsave(plot_i,filename = paste0("results/figures/coverage/components_rmse.pdf"), height = 2.5, width = 3)
  


# Plot the interaction term results --------------------------------------------

all_plots = list()
cov_plots = list()


iteration = 1
for (dgp in c("highint","lowint")) {
  
  
  results <- read.csv(paste0("results/theory_results/dgp_",dgp,"_chain_analysis.csv"))
  
  
  results %>%
    group_by(n, Chains) %>%
    summarise(reps_completed = max(run)) -> num_runs
  print(dgp)
  print(num_runs)
  
  results %>%
    filter(n>100) %>%
    group_by(n,run) %>%
    mutate(rmse_one = RMSE[Chains==1]) %>% 
    ungroup() %>% 
    mutate(rmse = RMSE / rmse_one) %>%
    group_by(n, Chains) %>%
    summarise(mean_rmse = mean(rmse),
              mean_coverage = mean(Coverage),
              sd_coverage = sd(Coverage)/7,
              mean_rmse = mean(rmse),
              sd_rmse = sd(rmse)/7) %>%
    group_by(n) %>%
    select(n, Chains, mean_rmse,sd_rmse) %>%
    mutate(Chains = as.factor(Chains)) %>%
    ggplot(aes(x = n, y= mean_rmse, color = Chains))+
    geom_line(aes(linetype = Chains))+
    geom_point(aes(color = Chains))+
    scale_linetype_manual(values = c("1" = "dashed", "8" = "solid", "20" = "solid"))+
    geom_errorbar(aes(ymin = mean_rmse - 1.96*sd_rmse, ymax = mean_rmse + 1.96*sd_rmse, color = Chains), width = 0) +
    labs(y = "RMSE/RMSE(1 chain) (1000 test pts)", title = paste0("DGP: ",dgp))+
    scale_color_manual(values = c("1"="turquoise1", "8"="steelblue1", "20"="royalblue2"))+
    vthemes::theme_vmodern() +
    theme(axis.line = element_line(color='black'),
          panel.background = element_rect(fill = 'white', color = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.title=element_text(size=6))+
    ylim(.7,1.1) -> plot_i
  all_plots[[iteration]] = plot_i
  ggsave(plot_i,filename = paste0("results/figures/coverage/",dgp,"rmse.pdf"), height = 2.5, width = 3)
  

  
  results %>%
    filter(n>100) %>%
    group_by(n, Chains) %>%
    summarise(mean_rmse = mean(RMSE),
              mean_coverage = mean(Coverage),
              sd_coverage = sd(Coverage)/7) %>%
    select(n, Chains, mean_coverage,sd_coverage) %>%
    mutate(Chains = as.factor(Chains)) %>%
    ggplot(aes(x = n, y = mean_coverage))+
    ggplot2::geom_point(aes(color = Chains)) +
    ggplot2::geom_line(aes(color = Chains)) +
    vthemes::theme_vmodern() +
    #vthemes::scale_color_vmodern(discrete = FALSE)+
    labs(y = "Empirical Coverage (1000 test pts)", title = paste0("DGP: ",dgp))+
    geom_errorbar(aes(ymin = mean_coverage - 1.96*sd_coverage, ymax = mean_coverage + 1.96*sd_coverage, color = Chains), width = 0)+
    scale_color_manual(values = c("1"="turquoise1", "8"="steelblue1", "20"="royalblue2"))+
    theme(axis.line = element_line(color='black'),
          panel.background = element_rect(fill = 'white', color = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.title=element_text(size=6))+
    geom_hline(yintercept = .95,linetype="dashed",
               color = "red") -> plot_i
  
  cov_plots[[iteration]] = plot_i
  ggsave(plot_i,filename = paste0("results/figures/coverage/",dgp,"coverage.pdf"), height = 2.5, width = 3)
  iteration <- iteration+1
  
}

library(gridExtra)

p_final = grid.arrange(all_plots[[1]]+theme(legend.position ="none"),
                       all_plots[[2]]+theme(axis.title.y=element_blank()),
                       widths =c(4,4.75),
                       ncol = 2)
ggsave(p_final,filename = paste0("results/figures/coverage/all_interaction_rmse.pdf"), height = 4, width = 7)

p_final = grid.arrange(cov_plots[[1]]+theme(legend.position ="none"),
                       cov_plots[[2]]+theme(axis.title.y=element_blank()), 
                       widths =c(4,4.75),
                       ncol = 2)
ggsave(p_final,filename = paste0("results/figures/coverage/all_interaction_coverages.pdf"), height = 4, width = 7)
