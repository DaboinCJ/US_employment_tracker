# HEADER --------------------------------------------
#
# Authors: Carlos J. Daboin
# Copyright (c) Carlos J. Daboin, 2023
# Email:  cdaboin2@gmail.com
# GitHub: https://github.com/DaboinCJ/US_employment_tracker
# Date: 2023-07-19
# Last Update: 
#
# Script Name: 2023_bls_api_etl.R
#
# Script Description: Update former code base to pull national employment estimates by sector
#
# Notes:
## We use this package https://github.com/keberwein/blscrapeR/tree/master
## We connect with bls api 2.0 keys. You cant get yours here https://www.bls.gov/developers/
## we have curated lists of series id.
## PROGRESS AND RESOURCES IN NOTION

# TO DO:
## BASIC REVISION AND CLEANING: DONE
## DO IT FOR STATES: DONE
## CREATE GITHUB PROJECT: DONE 
## THINK ABOUT HOW TO HANDLE API KEYS IN GITHUB: DONE 
## SET UP GITHUB ACTION (https://rfortherestofus.com/2023/05/github-actions/)
### HANDLE PACKAGES: DONE 
## STREAMLINE CODE SO: 
### NAITONAL CHARTS ARE GENERATED AND SAVED: DONE
### NATIONAL DATASET IS GENERATED AND SAVED: DONE
### STATES DATASETS ARE GENERATED: 
## CREATE SHINY APP
### HOST IT IN daboanalytics, AWS, SATURN CLOUD
## PUBLISH SOMEWHERE (BLOG?)

# 0.1 Load Libraries -----------------------------
library(dplyr)
library(readr)
# library(devtools)
# devtools::install_package("keberwein/blscrapeR")
library(blscrapeR)
library(ggplot2)
library(stringr)
options(scipen=999)# to see large numbers without scientific notation


# First time, set up your BLS key to connect the API
## I'm not sharing my key. Neither you should. It's stored in a sepparate script where
## with line of code like this: set_bls_key("ccccxxxcccss", overwrite = TRUE)
source("set_bls_key.R")

# First time, reload your enviornment so you can use the key without restarting R.
## readRenviron("~/.Renviron")

# You can check it with:
Sys.getenv("BLS_KEY")


# 0.2 Load Data ----------------------------------

# County composition of MSAs - Merge this with county level estimates
## It is important because the API procesess FIPS+MSA+COUNTY CODES
county_msa<-read_delim("raw/Crosswalk/regions/Census CBP/cbp_msa_county_reference12.txt", delim=",") %>% 
  mutate(county=paste0(fipstate,fipscty)) 
county_msa$state_title<-stringr::str_remove(county_msa$name_county,pattern="(.*?), ")

# 1.0 Define data Series Codes ------------------------------------

##1.1 National CES by SECTOR---------------------------------------

### General sector
naics_1_codes_api <- read_csv("raw/naics_codes_api.csv") %>%
  filter(display_level==1) %>%
  mutate(code=paste0("CEU",industry_code,"001"))

### Broad sector
naics_2_codes_api <- read_csv("raw/naics_codes_api.csv") %>% 
  filter(display_level==2) %>% 
  mutate(code=paste0("CEU",industry_code,"01")) 

##1.2 State-wise CES by SECTOR---------------------------------------

all_states<- unique(county_msa$state_title)
selected_state_name<-"Florida"

# search the state name in the official state fips list
selected_state<-county_msa %>% 
  filter(state_title==selected_state_name) %>% 
  pull(fipstate) %>% 
  paste0("00000")

series_state_1<-paste("SMU",
                      selected_state,
                      stringr::str_remove(naics_1_codes_api$code,"CEU"),sep ="" )

##1.3 City-wise CES by SECTOR---------------------------------------
all_cities<-unique(county_msa$name_msa)
selected_city_name<-all_cities[all_cities=="Miami-Fort Lauderdale-West Palm Beach, FL Metro Area"] # Other cities we've worked with:# Kansas City, MO-KS 


# search the city name in the official county-msa-state dataset
selected_city<-county_msa %>% 
  group_by(msa,name_msa,fipstate) %>% 
  ## if the city has many states, county how many counties it has per state
  summarise(count=n()) %>% 
  mutate(code=paste0(fipstate,msa)) %>% 
  filter(stringr::str_detect(name_msa,selected_city_name)) %>%
  # keep the state code with the largest number of counties
  filter(count==max(count)) %>% 
  pull(code)

# "New York-Newark-Jersey City, NY-NJ-PA" code doesn't follow the filtering logic above, in this case do the next
# Same for St. Louis, MO-IL
# Be careful for the same thing happening in other multi-state metro areas 
##CD on July 2020: this could be solved by looking at how much each state has of a metro area population. Im sure the state with the largest share will be the one in the BLS API
if (str_detect(selected_city_name,"New York")){
  selected_city<-"3635620"
} else if (str_detect(selected_city_name,"St. Louis")){
  selected_city<-"2941180"
} 

## critical
# Remember that this is not a MSA code, its an API specific code to identify cities
# The system is State FIPS (ej. 13) + first 2 MSA digits (12) + 3rd and 4th MSA digits (06 for Atlanta and 26 for Augusta-Richmond) 
# This is how you build a city-sector code for the API
series_city_1<-paste("SMU",
                     selected_city,
                     str_remove(naics_1_codes_api$code,"CEU"),sep ="" )

# 2.0 Run ETL --------------------------------------------------------

##2.1 National CES by SECTOR---------------------------------------

df_1 <- bls_api(naics_1_codes_api$code,
                startyear = 2018, endyear = 2023, Sys.getenv("BLS_KEY"),catalog = TRUE) %>%
  # Add time-series dates
  dateCast()

df_1_clean<-df_1 %>%
  left_join(select(naics_1_codes_api,code,naics_code,industry_name,display_level),by=c("seriesID"='code'))

## SAFETY CHECK: Data is squared
df_1_clean %>% 
  group_by(seriesID, industry_name,naics_code) %>% 
  count()


## 2.2 State-wise CES by SECTOR------------------------------------------------------


###This is how you build a state-sector code for the API
df_state_1 <- bls_api(series_state_1,
                       startyear = 2018, endyear = 2023, Sys.getenv("BLS_KEY"),catalog = TRUE) %>%
  # Add time-series dates
  dateCast()

df_state_1_clean<-df_state_1 %>%
  mutate(code=as.numeric(substr(seriesID,str_length(seriesID)-9,str_length(seriesID)-3))) %>%
  left_join(naics_1_codes_api,by=c("code"="industry_code")) %>% 
  mutate(value=value*1000)


## 2.3 City-wise CES by SECTOR------------------------------------------------------


df_cities_1 <- bls_api(series_city_1,
                       startyear = 2018, endyear = 2023, Sys.getenv("BLS_KEY"),catalog = TRUE) %>%
  # Add time-series dates
  dateCast()

df_cities_1_clean<-df_cities_1 %>%
  mutate(code=as.numeric(substr(seriesID,str_length(seriesID)-9,str_length(seriesID)-3))) %>%
  left_join(naics_1_codes_api,by=c("code"="industry_code")) %>% 
  mutate(value=value*1000)



#2.4 Save the data -----------------------------------------------------------
write_rds(df_1_clean,"data/df_1_clean.rds")
write_rds(df_state_1_clean,"data/df_state_1_clean.rds")
write_rds(df_cities_1_clean,"data/df_cities_1_clean.rds")


# 2.5 RUN ETLS at two digits level-------------------
run_all<-FALSE

if(run_all==TRUE){
  ## National level 2 digits-------------------------------------------------------------------
  df_2 <- bls_api(naics_2_codes_api$code,
                  startyear = 2018, endyear = 2023, Sys.getenv("BLS_KEY"),catalog = TRUE) %>%
    # Add time-series dates
    dateCast()
  
  df_2_clean<-df_2 %>% 
    left_join(select(naics_2_codes_api,code,naics_code,industry_name,display_level),by=c("seriesID"='code'))
  
  
  ### SAFETY CHECK: Data is squared
  df_2_clean %>% 
    group_by(seriesID, industry_name,naics_code) %>% 
    count()
  
  ## State level 2 digits-------------------------------------------------------------------
  series_state_2<-paste("SMU",
                        selected_state,
                        str_remove(naics_2_codes_api$code,"CEU"),sep ="" )
  
  df_state_2 <- bls_api(series_state_2,
                        startyear = 2018, endyear = 2023, Sys.getenv("BLS_KEY"),catalog = TRUE) %>%
    # Add time-series dates
    dateCast()
  
  df_state_2_clean<-df_state_2 %>%
    mutate(code=as.numeric(substr(seriesID,str_length(seriesID)-9,str_length(seriesID)-2))) %>%
    left_join(naics_2_codes_api,by=c("code"="industry_code")) %>% 
    mutate(short_name=industry_name,
           value=value*1000)
  
  ## City level 2 digits-------------------------------------------------------------------
  series_city_2<-paste("SMU",
                       selected_city,
                       str_remove(naics_2_codes_api$code,"CEU"),sep ="" )
  
  df_cities_2 <- bls_api(series_city_2,
                         startyear = 2018, endyear = 2023, Sys.getenv("BLS_KEY"),catalog = TRUE) %>%
    # Add time-series dates
    dateCast()
  
  df_cities_2_clean<-df_cities_2 %>%
    mutate(code=as.numeric(substr(seriesID,str_length(seriesID)-9,str_length(seriesID)-2))) %>%
    left_join(naics_2_codes_api,by=c("code"="industry_code")) %>% 
    mutate(short_name=industry_name,
           value=value*1000)
}

#3.1  Series identified manually -----------------------------------------

first_level<-"Total Nonfarm All Employees"
second_level_sec<-c("Goods Producing All Employees","Service-Providing All Employees","Private Service Providing All Employees")
third_level_sec<-c("Retail Trade All Employees","Wholesale Trade All Employees","Non-Durable Goods All Employees","Durable Goods All Employees",
                   "Mining, Logging and Construction All Employees")



#3.1.1 Charts with manual clasiffication---------------------------------------
set_1_no_yellow<-c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "black", "#A65628", "#F781BF", "#999999","green","gold")

source("functions.R")

plots_1<-ces_plots(data=df_1_clean, title_text = "- U.S Total")

ggsave(plot = plots_1[[2]],
       filename = paste0("plots/national_chart_1_",
              substr(Sys.Date(),0,7),".png"))

plots_state_1<-ces_plots(data=df_state_1_clean, title_text = "- Florida")

ggsave(plot = plots_state_1[[2]],
       filename = paste0("plots/state_florida_chart_1_",
              substr(Sys.Date(),0,7),".png"))

plots_cities_1<-ces_plots(data=df_cities_1_clean, title_text = "- Miami")

ggsave(plot = plots_cities_1[[2]],
       filename = paste0("plots/city_miami_chart_1_",
              substr(Sys.Date(),0,7),".png"))


