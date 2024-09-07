#packages
pacman::p_load(tidyverse, #general data handling
               here,      #easy file referencing
               gtsummary, #Table 1
               labelled ) #labeling data


#loading data
here::i_am("code/00_clean_data.R")
absolute_path_to_births <- here::here("raw_data", "births-2016-18.csv")
absolute_path_to_deaths <- here::here("raw_data", "fetal-death-2016-18.csv")

births <- read.csv(absolute_path_to_births, header = TRUE)
deaths <- read.csv(absolute_path_to_deaths, header = TRUE)
  

#subsetting births dataset to variables of interest (use ls(data) to see list of variables)
#creating variables to indicate if observation has outcome of interest (death) or censored (birth)

births_subset <- births %>%
  select(PUBLICID,
         EVENT_DATE_OF_BIRTH,
         EVENT_YEAR,
         GESTATION_WEEKS, 
         MOTHER_RACE) %>%
  filter(GESTATION_WEEKS > 19) #20 weeks gestation or higher

births_subset$outcome = 2 #competing risk, for Grey's CIF
births_subset$start = 20 #"start" at 20 weeks gestation


#subsetting deaths dataset to variables of interest. creating variables to indicate if observation has outcome of interest (death) or censored (birth)

deaths_subset <- deaths %>%
  select(PUBLICID,
         EVENT_DATE_OF_BIRTH, #assuming this means date of fetal death? no date of delivery 
         EVENT_YEAR,
         GESTATION_WEEKS,
         MOTHER_RACE) %>%
  filter(GESTATION_WEEKS > 19) #20 weeks gestation or higher

deaths_subset$outcome = 1 #outcome, for Grey's CIF
deaths_subset$start = 20 #"start" at 20 weeks gestation


#concatenating births, deaths
data<-rbind(births_subset, deaths_subset) 


#cleaning variables, adding policy indicator  
#addmargins(table(df$newvar, df$oldvar useNA = c("always")))
data <- data %>%
  mutate(stop = GESTATION_WEEKS,
         exposure = factor(MOTHER_RACE, c(-1,1,2,3,4,5,6), labels = c("Missing",
                                                         "White",
                                                         "Black",
                                                         "Asian",
                                                         "AI/AN",
                                                         "NHOPI",
                                                         ">1 Race")), #format for survival analysis 
         outcome = factor(outcome, 0:2, labels = c("Censor", "Stillbirth", "Live Birth")), #format for survival analysis
         event_date = as.Date(EVENT_DATE_OF_BIRTH),
         policy = ifelse(EVENT_DATE_OF_BIRTH < "2016-06-30", 0, 1),
         policy = factor(policy, 0:1, labels = c("Pre-policy", "Post-policy"))) #policy in effect 2016/07/01
         

#labeling data
var_label(data) <- list(
  PUBLICID = "ID",
  EVENT_DATE_OF_BIRTH = "Event Date",
  EVENT_YEAR = "Event year",
  GESTATION_WEEKS = "Weeks Gestation",
  MOTHER_RACE = "Maternal Race",
  outcome = "Outcome",
  exposure = "Maternal Race",
  start = "Start",
  stop = "Weeks Gestation",
  policy = "Policy"
)

#saving object
saveRDS(
  data, 
  file = here::here("derived_data/data_clean.rds")
)


#check to see if script ran 
print("cleaning data step complete")

