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
here::i_am("code/05_make_conventional_stillbirth_plot.R")

data <- readRDS(
  file = here::here("derived_data/data_clean.rds")
)

#formatting data for stillbirth calculation by policy 
data_policy <- data %>%
  mutate(GESTATION_WEEKS = as.numeric(GESTATION_WEEKS)) %>% 
  group_by(GESTATION_WEEKS, outcome, policy) %>% 
  summarize(n=n()) %>% #data formatting needs to be in this order to get summary statistics by gestation, outcome, and race, in order to get total number of events (births + stillbirths)
  group_by(GESTATION_WEEKS, policy) %>%
  mutate(total = sum(n)) %>% #total number of events (stillbirths + live births) by gestational week and race
  rename(n_event = n,
         Policy = policy) %>% 
  filter(outcome == "Stillbirth") %>% #removing summary statistics for live births (not outcome of interest)
  mutate(stillbirth_risk_per_1000 = (n_event/total)*1000, #stillbirth calculation, per gestational week: n(stillbirths)/ total (live births + stillbirths)
         ci_low = ((1000/total)*(n_event - 1.96*sqrt(n_event))), #derived from https://www.health.pa.gov/topics/HealthStatistics/Statistical-Resources/UnderstandingHealthStats/Documents/Confidence_Intervals_for_a_Crude_Rate.pdf 
         ci_high = ((1000/total)*(n_event + 1.96*sqrt(n_event)))) 

#formatting data for stillbirth calculation by race and policy 
data_race_policy <- data %>%
  mutate(GESTATION_WEEKS = as.numeric(GESTATION_WEEKS)) %>% 
  group_by(GESTATION_WEEKS, outcome, exposure, policy) %>% 
  summarize(n=n()) %>% #data formatting needs to be in this order to get summary statistics by gestation, outcome, and race, in order to get total number of events (births + stillbirths)
  group_by(GESTATION_WEEKS, exposure, policy) %>%
  mutate(total = sum(n)) %>% #total number of events (stillbirths + live births) by gestational week and race
  rename(n_event = n,
         Race = exposure, 
         Policy = policy) %>% 
  filter(outcome == "Stillbirth") %>% #removing summary statistics for live births (not outcome of interest)
  mutate(stillbirth_risk_per_1000 = (n_event/total)*1000, #stillbirth calculation, per gestational week: n(stillbirths)/ total (live births + stillbirths)
         ci_low = ((1000/total)*(n_event - 1.96*sqrt(n_event))), #derived from https://www.health.pa.gov/topics/HealthStatistics/Statistical-Resources/UnderstandingHealthStats/Documents/Confidence_Intervals_for_a_Crude_Rate.pdf 
         ci_high = ((1000/total)*(n_event + 1.96*sqrt(n_event)))) 

data_race_policy_bw <- data_race_policy %>% 
  filter(Race == "White" | Race == "Black")

#1 plot stillbirth risk by policy only, total population 
stillbirth_total_conventional <-data_policy %>% 
  ggplot(aes(x=GESTATION_WEEKS,
             y=stillbirth_risk_per_1000,
             group = Policy,
             color = Policy)) +
  geom_line(linewidth = 2) + 
  scale_color_manual(values = c("#dfc27d","#a6611a")) +
  labs(
    title = "Rate of stillbirth in GA by policy (conventional)",
    x = "Weeks gestation",
    y = "Stillbirth per 1,000 births") +
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
  annotate("text", x=41.5, y=400, label="42 weeks", angle=90, size = 6, color = "#636363")

#2 plot stillbirth risk by policy, stratified by race version 2
data_race_policy_bw_per1000<-data_race_policy_bw %>% 
  mutate("Race, Policy" = case_when(
    Race == "White" & Policy == "Pre-policy" ~ "White, Pre-policy",
    Race == "White" & Policy == "Post-policy" ~ "White, Post-policy",
    Race == "Black" & Policy == "Pre-policy" ~ "Black, Pre-policy",
    Race == "Black" & Policy == "Post-policy" ~ "Black, Post-policy"))

stillbirth_race_conventional <-data_race_policy_bw_per1000 %>% 
  ggplot(aes(x=GESTATION_WEEKS,
             y=stillbirth_risk_per_1000,
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
    title = "Rate of stillbirth in GA by race, policy (conventional)",
    x = "Weeks gestation",
    y = "Stillbirth per 1,000 births") + 
  geom_ribbon(aes(ymin = ci_low,
                  ymax = ci_high),
              alpha = 0.2,
              linetype = 0)  +
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
  annotate("text", x=41.5, y=500, label="42 weeks", angle=90, size = 6, color = "#636363")
  

#saving object 1
ggsave(
  here::here("figures/stillbirth_total_conventional.png"),
  plot = stillbirth_total_conventional,
  device = "png",
  scale = 1,
  width = 8,
  height = 6
)


#saving object 2
ggsave(
  here::here("figures/stillbirth_race_conventional.png"),
  plot = stillbirth_race_conventional,
  device = "png",
  scale = 1,
  width = 8,
  height = 6
)

#check to see if script ran 
print("conventional stillbirth plot step complete")