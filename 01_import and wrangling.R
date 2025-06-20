#This project aims to analyse data on the 111 service for mental health......... 
#Author: Stuti Bagri............................................................

#Install and load relevant packages.............................................
#Set library path
.libPaths("~/packages")

#Check libPath
.libPaths()

install.packages("tidyverse")
install.packages("ggiraph")
install.packages("classInt")
install.packages("RColorBrewer")
install.packages("writexl")
install.packages("sf")

library(tidyverse)
library(lubridate)
library(sf)
library(ggiraph)
library(htmlwidgets)
library(classInt)
library(RColorBrewer)
library(readxl)
library(writexl)

#Import the data................................................................

mar25 <- read_csv("~/nhs111_mh_March_2025.csv")
feb25 <- read_csv("~/nhs111_mh_February_2025.csv")
jan25 <- read_csv("~/nhs111_mh_January_2025.csv")
dec24 <- read_csv("~/nhs111_mh_December_2024.csv")
nov24 <- read_csv("~/nhs111_mh_November_2024_v2.csv")
oct24 <- read_csv("~/nhs111_mh_October_2024.csv")
sep24 <- read_csv("~/nhs111_mh_September_2024.csv")
aug24 <- read_csv("~/nhs111_mh_August_2024_v2.csv")
jul24 <- read_csv("~/nhs111_mh_July_2024.csv")
jun24 <- read_csv("~/nhs111_mh_June_2024.csv")
may24 <- read_csv("~/nhs111_mh_May_2024.csv")
apr24 <- read_csv("~/nhs111_mh_April_2024.csv")

# Why are there so many more observations in the Jan dataset than the others? Let's investigate. 
metjan25 <- unique(jan25$METRIC_NAME)
metdec24 <- unique(dec24$METRIC_NAME) 

diff2 <-setdiff(metjan25, metdec24) #what's in the first dataset that isn't in the second?
view(diff2)

#Let's re-import Jan and exclude the new breakdowns/metrics so that all 6 datasets match
jan25_v2 <- read_csv("~/nhs111_mh_January_2025.csv") %>% 
  filter(!grepl("^Prop", METRIC_NAME) &
          !grepl("^Aver", METRIC_NAME) &
           ORG_NAME != 'England' &
           ORG_NAME != 'All')

#Importing data on population need
popneed <- read_excel("~/d-mental-health-need-2023-24-v1.xlsx", sheet="Region_index", skip=2) %>% 
  filter(Year == "2024-25") %>% 
  rename_all(tolower) %>% 
  rename_with(~gsub(" ", "_", .x, fixed=TRUE)) %>% 
  mutate(across(contains("population"), as.numeric)) %>% 
  mutate(weighted_pop = round(normalised_weighted_population,0))

#Formatting the 111 dataset 
all_months <- bind_rows(mar25, feb25, jan25_v2, dec24, nov24, oct24, sep24, aug24, jul24, jun24, may24, apr24) %>% #(use joins only if you are working with datasets with different variables)
  mutate(REPORTING_PERIOD_START = format(REPORTING_PERIOD_START,"%b%y")) %>% 
  select(-REPORTING_PERIOD_END) %>% 
  pivot_wider(names_from = METRIC_NAME, values_from = METRIC_VALUE) %>% 
  rename_with(~gsub(" ", "_", .x, fixed=TRUE)) %>% #replaces spaces with underscores
  rename_all(tolower) %>% #makes all variable names lower case
  rename(month = reporting_period_start,
         calls_received = number_of_calls_received,
         calls_routed = calls_routed_through_ivr,
         calls_answered = number_of_answered_calls,
         calls_answered_60s = number_of_calls_answered_within_60_seconds,
         calls_abandoned = number_of_calls_abandoned,
         calls_abandoned_under30s = calls_abandoned_in_30_seconds_or_less,
         calls_abandoned_30to60s = calls_abandoned_in_over_30_seconds_and_up_to_and_including_60_seconds,
         calls_abandoned_over60s = calls_abandoned_after_60_seconds) %>% 
  mutate(across(starts_with("calls_"), as.numeric)) %>% 
  filter(!grepl("South London MH Partnership - Oxleas MH Crisis Line (SLP)", contract_name, fixed = TRUE) &
        !grepl("South West London and St George's Mental Health NHS Trust (SLP)", contract_name, fixed = TRUE)) #There are 3 contracts which have the same entries for each month because they work as a partnership. Removing two to avoid duplication.

#Look at regional variation
regional_variation <- all_months %>% 
  select(month, region, contract_name, calls_received:calls_abandoned_over60s) %>% 
  group_by(region) %>% 
  summarise(
    total_calls_received = sum(calls_received, na.rm =TRUE),
    total_calls_answered = sum(calls_answered, na.rm = TRUE),
    total_calls_abandoned = sum(calls_abandoned, na.rm =TRUE),
    total_calls_answeredin60 = sum(calls_answered_60s, na.rm =TRUE)) %>% 
  left_join(popneed %>% select(weighted_pop, region), by = "region") %>% 
  mutate(prop_callsanswered = round(total_calls_answered/total_calls_received*100,1),
         prop_callsabandoned = round(total_calls_abandoned/total_calls_received*100,1),
         prop_callsansweredin60 = round(total_calls_answeredin60/total_calls_received*100,1),
         vol_rate = round(total_calls_received/weighted_pop*100000,0))

#How do the number of calls received by contracts vary between and within regions?
contract_variation <- all_months %>% 
  select(region, contract_name,calls_received:calls_abandoned_over60s) %>% 
  group_by(region, contract_name) %>% 
  summarise(
    total_calls_received = sum(calls_received, na.rm =TRUE),
    total_calls_answered = sum(calls_answered, na.rm = TRUE)) %>% 
  mutate(prop_callsanswered = round(total_calls_answered/total_calls_received*100,1))

#Do all the contracts under each region submit entries each month? 
check1 <- all_months %>% 
  group_by(region, contract_name) %>% 
  summarize(count=n()) %>% 
  mutate(flag = case_when(
    count == 132 ~ "All months",
    count <= 121 & count >= 99 ~ "9-11 months",
    count <= 88 & count >= 66 ~ "6-8 months",
    count <= 55 & count >= 33 ~ "3-5 months",
    count == 22 ~ "2 months",
    TRUE ~ "1 month"))

#Across all regions, what proportion of contracts submit entries for n months?
prop.table(table(check1$count))

#Timeseries trends
timeseries <- all_months %>% 
  select(month, region, contract_name, calls_received:calls_abandoned_over60s) %>% 
  group_by(month) %>% 
  summarise(
    total_calls_received = sum(calls_received, na.rm =TRUE),
    total_calls_answered = sum(calls_answered, na.rm = TRUE),
    total_calls_abandoned = sum(calls_abandoned, na.rm =TRUE),
    total_calls_answeredin60 = sum(calls_answered_60s, na.rm =TRUE)) %>% 
  mutate(prop_callsanswered = round(total_calls_answered/total_calls_received*100,1),
         prop_callsabandoned = round(total_calls_abandoned/total_calls_received*100,1),
         prop_callsansweredin60 = round(total_calls_answeredin60/total_calls_received*100,1),
         month = my(month)) %>% #converts to a date variable
  arrange(month) #arrange by earliest to latest month

#Save to excel files
write_xlsx(regional_variation, "~/regional_variation_1yr.xlsx")
write_xlsx(timeseries, "~/timeseries_1yr.xlsx")

