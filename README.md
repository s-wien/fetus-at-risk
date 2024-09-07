# About this repository

This repository includes all code used to estimate stillbirth by gestational age using the fetus-at-rsk approach vs. the conventional estimate for stillbirth. If this is your first time using the fetus-at-risk method, please read [The fetuses-at-risk approach: survival analysis from a fetal perspective](https://pubmed.ncbi.nlm.nih.gov/28742216/) by Joseph and Kramer (2017). 
 
Created by Simone Wien and presented at the CityMatCH annual conference, September 2024. 

# Data structure 

You will need to supply your own data to use this code. If using vital records, you will need live birth and fetal death records to do this. At minimum, your data should have the following variables: 

| Variable Name                      | Description                                                       |
|------------------------------------|-------------------------------------------------------------------|
| ID                                 | Individual event ID                                               |
| Date of event                      | Date of livebirth or fetal death (typically separate data)        |
| Gestational week                   | Gestational week at which the event occurred                      |
| Outcome                            | Stillbirth or livebirth                                           |
| Stratifcation variable (opotional) | Variable to strafiy on (e.g., social or clinical characteristics) |

# File structure

This repository contains the following file structure: 

-`code/`

-`derived_data/`

-`figures/`

-`raw_data/`

-`tables/`

-`README` (you are reading this now)

# Code description

The files below perform the following: 

`code/00_clean_data.R`
- read birth and fetal death data from `raw_data/` folder
- combine birth and fetal death data
- format data to use the `cuminc` package
- save clean data in `derived_data/` folder

`code/01_make_table1.R`
- read clean data from `derived_data/` folder
- save table 1 in `tables/` folder

`code/02_make_prev_plots.R`
- read clean data from `derived_data/` folder
- create prevalence plots for preterm birth and stillbirth
- save plots in `figures/` folder

`code/03_make_stillbirth_far_table.R`
- read clean data from `derived_data/` folder
- create stillbirth by gestational age tables using the fetus-a-risk aproach

`code/04_make_stillbirth_far_plot.R`
- read clean data from `derived_data/` folder
- create stillbirth by gestational age plots using the fetus-a-risk aproach 
- save plots in `figures/` folder

`code/05_make_conventiona_stillbirth_plot.R`
- read clean data from `derived_data/` folder
- create stillbirth by gestational age plots using the conventional aproach 
- save plots in `figures/` folder
