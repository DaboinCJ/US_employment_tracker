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
### STATES DATASETS ARE GENERATED: FIX
## CREATE REPORT: DONE
## CREATE SHINY: DONT want to
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
library(lubridate)
options(scipen=999)# to see large numbers without scientific notation


# First time, set up your BLS key to connect the API
## I'm not sharing my key. Neither you should. It's stored in a sepparate script where
## with line of code like this: set_bls_key("ccccxxxcccss", overwrite = TRUE)
source("set_bls_key.R")

# First time, reload your enviornment so you can use the key without restarting R.
## readRenviron("~/.Renviron")

# You can check it with:
Sys.getenv("BLS_KEY")



# 0.1 Define which ETLs to run --------------------------------------------

PREVIOUS_DATA=TRUE
NAT_1d=TRUE
NAT_2d=TRUE
STA_1d=TRUE
STA_2d=TRUE

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
all_states_codes<-county_msa %>% 
  distinct(fipstate,state_title) %>% 
  mutate(fipstate=paste0(fipstate,"00000")) 

all_series_state_1<-paste0("SMU",
                           as.vector(outer(all_states_codes$fipstate,
                                           stringr::str_remove(naics_1_codes_api$code,"CEU"),
                                           paste,sep="")))

all_series_state_2<-paste0("SMU",
                           as.vector(outer(all_states_codes$fipstate,
                                           stringr::str_remove(naics_2_codes_api$code,"CEU"),
                                           paste,sep="")))


# 2.0 Run ETL --------------------------------------------------------


##2.1 National CES by SECTOR---------------------------------------
nat_latest_year<-read_rds("data/nat_latest_year_updatefromhere.rds")
if(NAT_1d==TRUE){

df_1 <- bls_api(naics_1_codes_api$code,
                startyear = nat_latest_year, endyear = year(Sys.Date()), Sys.getenv("BLS_KEY"),catalog = TRUE) %>%
  # Add time-series dates
  dateCast()

df_1_clean<-df_1 %>%
  left_join(select(naics_1_codes_api,code,naics_code,industry_name,display_level),by=c("seriesID"='code'))

  if(PREVIOUS_DATA==FALSE){
    # its the first time you save this data
    write_rds(df_1_clean,"data/df_1_clean.rds")
  } else{
    # you've already save a chunk of data an want to add a new one
    write_rds(df_1_clean,"data/df_1_clean_new.rds")
  } 

}

if(NAT_2d==TRUE){
  
  df_2 <- bls_api(naics_2_codes_api$code,
                  startyear = nat_latest_year, endyear = year(Sys.Date()), Sys.getenv("BLS_KEY"),catalog = TRUE) %>%
    # Add time-series dates
    dateCast()
  
  df_2_clean<-df_2 %>%
    left_join(select(naics_2_codes_api,code,naics_code,industry_name,display_level),
              by=c("seriesID"='code'))

  if(PREVIOUS_DATA==FALSE){
      # its the first time you save this data
     write_rds(df_2_clean,"data/df_2_clean.rds")
    } else{
      # you've already save a chunk of data an want to add a new one
    write_rds(df_2_clean,"data/df_2_clean_new.rds")
    }
  # a record of the latest period in your data
  write_rds(max(df_2_clean$year),"data/nat_latest_year_updatefromhere.rds")  
}


## 2.2 State-wise CES by SECTOR------------------------------------------------------
sta_latest_year<-read_rds("data/sta_latest_year_updatefromhere.rds")
# sta_latest_year<-2019
if(STA_1d==TRUE){
# the api procesess 50 series at a time, so we must split requeest in batches
  
df_states_1<-list() # list storing the results of each batch
index_list<-0       # index for reference

for (i in list(c(1:50),
               c(1:50)+50,
               c(1:50)+50*2,
               c(1:50)+50*3,
               c(201:204) # 5 batches because there are 204 series
               ) ){
  index_list<-index_list+1
  print(paste("round",index_list))
  print(paste("downloading series",all_series_state_1[i]))
  
  df_states_1[[index_list]]<-bls_api(all_series_state_1[i],
          startyear = sta_latest_year, endyear = year(Sys.Date()),
          Sys.getenv("BLS_KEY"),catalog = TRUE) %>%
    # Add time-series dates
    dateCast()
  
}  
  
df_state_1 <- do.call(rbind,df_states_1)

df_state_1_clean<-df_state_1 %>%
  mutate(code=as.numeric(substr(seriesID,str_length(seriesID)-9,str_length(seriesID)-3))) %>%
  left_join(naics_1_codes_api,by=c("code"="industry_code")) %>% 
  mutate(fipstate=substr(seriesID,4,10)) %>% 
  left_join(all_states_codes, by="fipstate") %>% 
  mutate(value=value*1000)

  if(PREVIOUS_DATA==FALSE){
    # its the first time you save this data
    write_rds(df_state_1_clean,"data/df_state_1_clean.rds")
  } else{
    # you've already save a chunk of data an want to add a new one
    write_rds(df_state_1_clean,"data/df_state_1_clean_new.rds")
  } 

}

if(STA_2d==TRUE){
  # the api procesess 50 series at a time, so we must split requeest in batches
  
  df_states_2<-list() # list storing the results of each batch
  index_list<-0       # index for reference
  
  for (i in list(c(1:50),
                 c(1:50)+50,
                 c(1:50)+50*2,
                 c(1:50)+50*3,
                 c(1:50)+50*4,
                 c(1:50)+50*5,
                 c(1:50)+50*6,
                 c(1:50)+50*7,
                 c(1:50)+50*8,
                 c(1:50)+50*9,
                 c(1:50)+50*10,
                 c(551:561) # 12 batches because there are 561 series
  ) ){
    index_list<-index_list+1
    print(paste("round",index_list))
    print(paste("downloading series",all_series_state_2[i]))
    
    df_states_2[[index_list]]<-bls_api(all_series_state_2[i],
                                       startyear = sta_latest_year,
                                       endyear = year(Sys.Date()),
                                       Sys.getenv("BLS_KEY"),catalog = TRUE) %>%
      # Add time-series dates
      dateCast()
    
  }  
  
  df_state_2 <- do.call(rbind,df_states_2)
  
  df_state_2_clean<-df_state_2 %>%
    mutate(code=as.numeric(substr(seriesID,str_length(seriesID)-9,str_length(seriesID)-2))) %>%
    left_join(naics_2_codes_api,by=c("code"="industry_code")) %>% 
    mutate(fipstate=substr(seriesID,4,10)) %>% 
    left_join(all_states_codes, by="fipstate") %>% 
    mutate(value=value*1000) 
  
  if(PREVIOUS_DATA==FALSE){
    # its the first time you save this data
    write_rds(df_state_2_clean,"data/df_state_2_clean.rds")
  } else{
    # you've already save a chunk of data an want to add a new one
    write_rds(df_state_2_clean,"data/df_state_2_clean_new.rds")
  } 
  
write_rds(max(df_state_2_clean$year),"data/sta_latest_year_updatefromhere.rds")
  
}



