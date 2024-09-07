#packages
pacman::p_load(tidyverse, #general data handling
               here, #easy file referencing
               ggplot2, #plots
               gridExtra, #arrange multiple ggplots
               cmprsk, #for cuminc package, Gray's CIF estimator 
               ggsurvfit, #ggcumnic() to plot
               tidycmprsk, #support ggsurvfit
               survival) #for cuminc package


#loading data
here::i_am("code/04_make_stillbirth_far_plot.R")

data <- readRDS(
  file = here::here("derived_data/data_clean.rds")
)

#filtering to Black, White maternal race
data_bw <- data %>% 
  filter(exposure == "White" | exposure == "Black")

#1 survival curve: stillbirth per gestational age (fetus at risk) pre- vs. post-policy, total population

#create table of estimates
stillbirth_total_far_tidy<- tidycmprsk::cuminc(Surv(stop, outcome) ~ policy, data)

#format data, generate confidence intervals
stillbirth_total_far_table<-as.data.frame(stillbirth_total_far_tidy$tidy) %>%
  filter(outcome == "Stillbirth",
         time > 0) %>% 
  mutate(stillbirth_risk_per_1000 = estimate*1000,
         ci_low = conf.low*1000,
         ci_high = conf.high*1000,
         "Policy" = strata)

#plot
stillbirth_total_far_plot<-stillbirth_total_far_table %>% 
  ggplot(aes(x=time,
             y=stillbirth_risk_per_1000,
             group = Policy,
             color = Policy)) +
  geom_line(linewidth = 2) + 
  scale_color_manual(values = c("#dfc27d","#a6611a")) +
    labs(
    title = "Rate of stillbirth in GA by policy (fetus-at-risk)",
    x = "Weeks gestation",
    y = "Stillbirth per 1,000 fetuses") +
  geom_ribbon(aes(ymin = ci_low,
                  ymax = ci_high),
              alpha = 0.2,
              linetype = 0) + 
  theme(text=element_text(size=15), #change font size of all text
        axis.text=element_text(size=15), #change font size of axis text
        axis.title=element_text(size=15), #change font size of axis titles
        plot.title=element_text(size=15), #change font size of plot title
        legend.text=element_text(size=15), #change font size of legend text
        legend.title=element_text(size=15)) + #change font size of legend title
  theme(plot.title = element_text(hjust = 0.5)) + #center title
  geom_vline(xintercept = 42, #line for full term pregnancies 
             color = "#636363",
             linewidth = 2) + 
  annotate("text", x=41.5, y=5, label="42 weeks", angle=90, size = 6, color = "#636363")


#2 survival plot: by race and by policy per 1,000 fetuses at risk

#create table of estimates
survival_race_policy_tidy<- tidycmprsk::cuminc(Surv(stop, outcome) ~ exposure + policy, data_bw) 

stillbirth_race_far_table<-as.data.frame(survival_race_policy_tidy$tidy) %>%
  filter(outcome == "Stillbirth",
         time > 0) %>% 
  mutate(stillbirth_risk_per_1000 = estimate*1000,
         ci_low = conf.low*1000, 
         ci_high = conf.high*1000, 
         "Race, Policy" = strata)

#plot
stillbirth_race_far_plot <- stillbirth_race_far_table %>% 
  ggplot(aes(x=time,
             y=stillbirth_risk_per_1000,
             group = `Race, Policy`,
             color = `Race, Policy`,
             fill = `Race, Policy`)) +
  scale_colour_manual(values = c("White, Pre-policy" = "#dfc27d" ,
                                 "White, Post-policy" = "#a6611a",
                                 "Black, Pre-policy" = "#80cdc1" ,
                                 "Black, Post-policy" = "#0E315F")) +
  scale_fill_manual(values = c("White, Pre-policy" = "#dfc27d" ,
                               "White, Post-policy" = "#a6611a",
                               "Black, Pre-policy" = "#80cdc1" ,
                               "Black, Post-policy" = "#0E315F")) +
  geom_line(linewidth = 2) + 
  labs(
    title = "Rate of stillbirth in GA by race, policy (fetus-at-risk)",
    x = "Weeks gestation",
    y = "Stillbirth per 1,000 fetuses") +
  geom_ribbon(aes(ymin = ci_low,
                  ymax = ci_high),
              alpha = 0.2,
              linetype = 0) +
  theme(text=element_text(size=15), #change font size of all text
        axis.text=element_text(size=15), #change font size of axis text
        axis.title=element_text(size=15), #change font size of axis titles
        plot.title=element_text(size=15), #change font size of plot title
        legend.text=element_text(size=15), #change font size of legend text
        legend.title=element_text(size=15)) + #change font size of legend title
  theme(plot.title = element_text(hjust = 0.5)) + #center title
  geom_vline(xintercept = 42, #line for full term pregnancies 
             color = "#636363",
             linewidth = 2) + 
  annotate("text", x=41.5, y=8, label="42 weeks", angle=90, size = 6, color = "#636363")

    
#saving object 1
ggsave(
  here::here("figures/stillbirth_total_far_plot.png"),
  plot = stillbirth_total_far_plot,
  device = "png",
  scale = 1,
  width = 8,
  height = 6
)

#saving object 2
ggsave(
  here::here("figures/stillbirth_race_far_plot.png"),
  plot = stillbirth_race_far_plot,
  device = "png",
  scale = 1,
  width = 8,
  height = 6
)

#check to see if script ran 
print("stillbirth FAR plot step complete")
