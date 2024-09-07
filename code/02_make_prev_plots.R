#packages
pacman::p_load(tidyverse, #general data handling
               here, #easy file referencing
               ggplot2, #plots
               gridExtra, #arrange multiple ggplots
               cmprsk, #for cuminc package, Gray's CIF estimator 
               ggsurvfit, #ggcumnic() to plot
               tidycmprsk, #support ggsurvfit
               survival) #for cuminc package

#LOADING DATA

here::i_am("code/02_make_prev_plots.R")

data <- readRDS(
  file = here::here("derived_data/data_clean.rds")
)

stillbirth <- data

#create preterm birth as a binary variable
data <- data %>% 
  filter(outcome == "Live Birth") %>% 
  mutate(ptb = ifelse(GESTATION_WEEKS < 37, "Preterm birth", "Term"))

#create B-W dataset
data_bw <-data %>% 
  filter(exposure == "White" | exposure == "Black")

#CREATE TOTAL PTB DATASET

data <- data %>% 
  group_by(ptb, policy) %>%
  summarize(n=n())

#group by policy, get denominator (number of observations per policy)
data <- data %>%
  group_by(policy) %>%
  mutate(total = sum(n),
         prev = (n/total)*100) %>%
  rename(Outcome = ptb,
         Prevalence = prev,
         Policy = policy)

#filer to PTB
data <- data %>%
  filter(Outcome =="Preterm birth")

#CREATE B-W PTB DATASET

#group by outcome and policy, get counts of each event 
data_bw <- data_bw %>% 
  group_by(ptb, policy, exposure) %>%
  summarize(n=n())

#group by policy, get denominator (number of observations per policy)
data_bw <- data_bw %>%
  group_by(policy, exposure) %>%
  mutate(total = sum(n),
         prev = (n/total)*100) %>%
  rename(Outcome = ptb,
         Prevalence = prev,
         Policy = policy,
         Race = exposure)

#PLOT TOTAL PTB PREVALENCE 
ptb_prev<- ggplot(data, aes(x=Policy,
                 y=Prevalence,
                 fill = Policy)) + 
  geom_bar(stat = "identity",
           width = 0.5) + 
  labs(title = "Preterm birth prevalence in GA, pre- vs. post-policy",
    y = "Preterm birth prevalence (%)") +
  geom_text(aes(label = round(Prevalence, digits = 2),
                vjust = -0.2,
                size = 15)) + 
  scale_fill_manual(values = c("Pre-policy" = "#a6bddb" ,
                               "Post-policy" = "#0E315F")) +
  theme(text=element_text(size=15), #change font size of all text
        axis.text=element_text(size=15), #change font size of axis text
        axis.title=element_text(size=15), #change font size of axis titles
        plot.title=element_text(size=15), #change font size of plot title
        legend.text=element_text(size=15), #change font size of legend text
        legend.title=element_text(size=15)) + #change font size of legend title
  theme(aspect.ratio = 2/1) + 
  theme(plot.title = element_text(hjust = 0.5)) + #center title
  theme(legend.position="none") #remove legend
  
#PLOT PTB PREVALENCE BY RACE
data_bw <- data_bw %>%
  filter(Outcome =="Preterm birth")

ptb_prev_race<- ggplot(data_bw, aes(x=Policy,
                                    y=Prevalence,
                                    fill = Policy)) + 
  geom_bar(stat = "identity",
           width = 0.5) +
  facet_wrap("Race") +
  geom_text(aes(label = round(Prevalence, digits = 2),
                vjust = -0.2),
            size= 6) +
  labs(title = "Preterm birth prevalence in GA by race, pre- vs. post-policy",
       y = "Preterm birth prevalence (%)") +
  scale_fill_manual(values = c("Pre-policy" = "#a6bddb" ,
                               "Post-policy" = "#0E315F")) +
  theme(text=element_text(size=15), #change font size of all text
        axis.text=element_text(size=15), #change font size of axis text
        axis.title=element_text(size=15), #change font size of axis titles
        plot.title=element_text(size=15), #change font size of plot title
        legend.text=element_text(size=15), #change font size of legend text
        legend.title=element_text(size=15)) + #change font size of legend title  
  theme(aspect.ratio = 2/1) + 
  theme(plot.title = element_text(hjust = 0.5)) + #center title
  theme(legend.position="none") #remove legend
 
#CREATE TOTAL STILLBIRTH DATASET
stillbirth_bw <- stillbirth %>%
  filter(exposure == "White" | exposure == "Black")
  
stillbirth <- stillbirth %>% 
  filter(outcome == "Live Birth" | outcome == "Stillbirth") %>%
  group_by(outcome, policy) %>%
  summarize(n=n())

#group by policy, get denominator (number of observations per policy)
stillbirth <- stillbirth %>%
  group_by(policy) %>%
  mutate(total = sum(n),
         prev = (n/total)*100) %>%
  rename(Outcome = outcome,
         Prevalence = prev,
         Policy = policy)

#CREATE  STILLBIRTH B-W DATASET
stillbirth_bw <- stillbirth_bw %>% 
  group_by(outcome, policy, exposure) %>%
  summarize(n=n())

#group by policy, get denominator (number of observations per policy)
stillbirth_bw <- stillbirth_bw %>%
  group_by(policy, exposure) %>%
  mutate(total = sum(n),
         prev = (n/total)*100) %>%
  rename(Outcome = outcome,
         Prevalence = prev,
         Policy = policy,
         Race = exposure)

#PLOT TOTAL STILLBIRTH PREVALENCE 
stillbirth <- stillbirth %>%
  filter(Outcome =="Stillbirth")

stillbirth_prev<- ggplot(stillbirth, aes(x=Policy,
                            y=Prevalence,
                            fill = Policy)) + 
  geom_bar(stat = "identity",
           width = 0.5) + 
  labs(title = "Stillbirth birth prevalence in GA, pre- vs. post-policy",
       y = "Stillbirth prevalence (%)") +
  geom_text(aes(label = round(Prevalence, digits = 2),
                vjust = -0.2,
                size = 15)) + 
  scale_fill_manual(values = c("Pre-policy" = "#dfc27d" ,
                               "Post-policy" = "#a6611a")) +
  theme(text=element_text(size=15), #change font size of all text
        axis.text=element_text(size=15), #change font size of axis text
        axis.title=element_text(size=15), #change font size of axis titles
        plot.title=element_text(size=15), #change font size of plot title
        legend.text=element_text(size=15), #change font size of legend text
        legend.title=element_text(size=15)) + #change font size of legend title
  theme(aspect.ratio = 2/1) + 
  theme(plot.title = element_text(hjust = 0.5)) + #center title
  theme(legend.position="none") #remove legend

#PLOT TOTAL STILLBIRTH B-W PREVALENCE 
stillbirth_bw <- stillbirth_bw %>%
  filter(Outcome =="Stillbirth")

stillbirth_prev_race<- ggplot(stillbirth_bw, aes(x=Policy,
                                    y=Prevalence,
                                    fill = Policy)) +
  geom_bar(stat = "identity",
           width = 0.5) +
  facet_wrap("Race") +
  geom_text(aes(label = round(Prevalence, digits = 2),
                vjust = -0.2),
            size= 6) +
  labs(title = "Stillbirth prevalence in GA by race, pre- vs. post-policy",
       y = "Stillbirthprevalence (%)") +
  scale_fill_manual(values = c("Pre-policy" = "#dfc27d" ,
                               "Post-policy" = "#a6611a")) +
  theme(text=element_text(size=15), #change font size of all text
        axis.text=element_text(size=15), #change font size of axis text
        axis.title=element_text(size=15), #change font size of axis titles
        plot.title=element_text(size=15), #change font size of plot title
        legend.text=element_text(size=15), #change font size of legend text
        legend.title=element_text(size=15)) + #change font size of legend title
  theme(aspect.ratio = 2/1) + 
  theme(plot.title = element_text(hjust = 0.5)) + #center title
  theme(legend.position="none") #remove legend

#saving plot 1
ggsave(
  here::here("figures/ptb_prev.png"),
  plot = ptb_prev,
  device = "png",
  scale = 1,
  width = 10,
  height = 6,
)

#saving plot 2
ggsave(
  here::here("figures/ptb_prev_race.png"),
  plot = ptb_prev_race,
  device = "png",
  scale = 1,
  width = 10,
  height = 6,
)

#saving plot 3
ggsave(
  here::here("figures/stillbirth_prev.png"),
  plot = stillbirth_prev,
  device = "png",
  scale = 1,
  width = 10,
  height = 6,
)

#saving plot 4
ggsave(
  here::here("figures/stillbirth_prev_race.png"),
  plot = stillbirth_prev_race,
  device = "png",
  scale = 1,
  width = 10,
  height = 6,
)

#check to see if script ran 
print("prevalence plot step complete")
