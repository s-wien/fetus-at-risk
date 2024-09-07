#packages
pacman::p_load(tidyverse, #general data handling
               here,      #easy file referencing
               gtsummary, #Table 1
               labelled ) #labeling data


#loading data
here::i_am("code/01_make_table1.R")

data <- readRDS(
  file = here::here("derived_data/data_clean.rds")
)

#removing censoring label for table 1
data <- data %>%
  mutate(Outcome = factor(outcome, labels = c("Stillbirth", "Live Birth")),
         "Birth outcome" = ifelse(GESTATION_WEEKS < 37, "Preterm birth", "Term"))


#making table 1, stratifying by policy only 
table_one <- data %>%
  select("policy", "Outcome", "Birth outcome", "exposure") %>% 
  tbl_summary(by = policy,
              sort = all_categorical() ~ "frequency",
              digits = c("Outcome" ~ 2,  "Birth outcome" ~ 1)) %>% 
  add_p() %>%
  add_overall() %>%
  as_gt() %>%
  gt::tab_options(table.font.names = "Arial")

#saving object 1
saveRDS(
  table_one,
  file = here::here("tables/table_one.rds")
)

#check to see if script ran 
print("table 1 step complete")