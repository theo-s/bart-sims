# If needed, install vthemes from Yu-Group website
# devtools::install_github("Yu-Group/vthemes")
library(ggplot2)
library(dplyr)
library(vthemes)

all_plots = list()

# Unused for now
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

iteration = 1
for (dgp in c("sum","high", "low", "piecewise", "tree", "lss")) {


  results <- data.frame(n = NA,
                        nchain = NA,
                        run = NA,
                        exp = 1,
                        rmse = NA,
                        cov = NA)

  for (file in dir("results/coverage/")) {
    if (grepl(pattern = dgp, x = file)) {
      #print(file)
      f <- readRDS(file = paste0("results/coverage/",file))

      for (iter in 1:3) {
        n_i = f[[1]]$n
        run_i = f[[1]]$run
        nchain_i =f[[1]]$nchain
        if (iter == 1) {
          results <- rbind(results,
                           c(n_i, nchain_i, run_i, 1,
                             f[[iter]]$rmse, f[[iter]]$empirical_cov))
        } else if (iter == 2) {
          results <- rbind(results,
                           c(n_i, 2, run_i, 1,
                             f[[iter]]$rmse, f[[iter]]$coverage))
        } else if (iter == 3) {
          results <- rbind(results,
                           c(n_i, 5, run_i, 1,
                             f[[iter]]$rmse, f[[iter]]$coverage))
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
    filter(n>100) %>%
    group_by(n, exp,run) %>%
    mutate(rmse_one = rmse[nchain==1]) %>% 
    ungroup() %>% 
    mutate(rmse = rmse / rmse_one) %>%
    group_by(n, nchain, exp) %>%
    summarise(mean_rmse = mean(rmse),
              mean_coverage = mean(cov),
              sd_coverage = sd(cov)/10,
              mean_rmse = mean(rmse),
              sd_rmse = sd(rmse)/10) %>%
    group_by(n) %>%
    select(n, nchain, mean_rmse,sd_rmse) %>%
    mutate(nchain = as.factor(nchain)) %>%
    ggplot(aes(x = n, y= mean_rmse, color = nchain))+
    geom_line(aes(linetype = nchain))+
    geom_point(aes(color = nchain))+
    scale_linetype_manual(values = c("1" = "dashed", "2" = "solid", "5" = "solid"))+
    geom_errorbar(aes(ymin = mean_rmse - 1.96*sd_rmse, ymax = mean_rmse + 1.96*sd_rmse, color = nchain), width = 0) +
    labs(y = "RMSE/RMSE(1 chain) (1000 test pts)", title = paste0("DGP: ",dgp))+
    scale_color_manual(values = c("1"="turquoise1", "2"="steelblue1", "5"="royalblue2"))+
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

  iteration <- iteration+1
}

library(gridExtra)

p_final = grid.arrange(all_plots[[1]]+theme(legend.position ="none"),
                       all_plots[[2]]+theme(legend.position ="none",axis.title.y=element_blank()),
                       all_plots[[3]]+theme(axis.title.y=element_blank()),
                       all_plots[[4]]+theme(legend.position ="none"),
                       all_plots[[5]]+theme(legend.position ="none",axis.title.y=element_blank()),
                       all_plots[[6]]+theme(axis.title.y=element_blank()), 
                       widths =c(4,4,4.75),
                       ncol = 3)
ggsave(p_final,filename = paste0("results/figures/coverage/all_rmse.pdf"), height = 4, width = 7)
