#packages
pacman::p_load(tidyverse, #general data handling
               here, #easy file referencing
               ggplot2, #plots
               gridExtra, #arrange multiple ggplots
               cmprsk, #for cuminc package, Gray's CIF estimator 
               ggsurvfit, #ggcumnic() to plot
               tidycmprsk, #support ggsurvfit
               survival, #for cuminc package
               gtsummary, #Table 1
               labelled) #labeling data


#loading data
here::i_am("code/03_make_stillbirth_far_table.R")

data <- readRDS(
  file = here::here("derived_data/data_clean.rds")
)

#filtering to Black, White maternal race
data_bw <- data %>% 
  filter(exposure == "White" | exposure == "Black")


#1 survival table data: by race and by policy per 1,000 fetuses at risk
survival_race_policy_tidy<- tidycmprsk::cuminc(Surv(stop, outcome) ~ exposure + policy, data_bw) #re-run, won;t save as tidy object when making plot

survival_race_policy_table<-as.data.frame(survival_race_policy_tidy$tidy) %>%
  filter(outcome == "Stillbirth",
         time > 0) %>% 
  mutate(stillbirth_risk_per_1000 = estimate*1000,
         ci_low = conf.low*1000, 
         ci_high = conf.high*1000, 
         "Race, Policy" = strata) %>%
  filter(time == 20 | time == 25 | time == 30 |time == 35 |time == 40 ) %>% 
  select(c(time,
           strata,
           stillbirth_risk_per_1000,
           ci_low,
           ci_high)) %>%
  mutate("Race" = case_when(
    strata == "White, Pre-policy" ~ "White",
    strata == "White, Post-policy" ~ "White",
    strata == "Black, Pre-policy" ~ "Black",
    strata == "Black, Post-policy" ~ "Black")) %>% 
  mutate("Policy" = case_when(
      strata == "White, Pre-policy" ~ "Pre-policy",
      strata == "White, Post-policy" ~ "Post-policy",
      strata == "Black, Pre-policy" ~ "Pre-policy",
      strata == "Black, Post-policy" ~ "Post-policy"))
    

#2 survival table data: total population by policy per 1,000 fetuses at risk
survival_total_pop_tidy<- tidycmprsk::cuminc(Surv(stop, outcome) ~ policy, data)

survival_total_pop_table<-as.data.frame(survival_total_pop_tidy$tidy) %>%
  filter(outcome == "Stillbirth",
         time > 0) %>% 
  mutate(stillbirth_risk_per_1000 = estimate*1000,
         ci_low = conf.low*1000,
         ci_high = conf.high*1000,
         "Policy" = strata) %>%
  filter(time == 20 | time == 25 | time == 30 |time == 35 |time == 40) %>% 
  select(c(time,
           strata,
           stillbirth_risk_per_1000,
           ci_low,
           ci_high))

#check to see if script ran 
print("FAR table step complete")
