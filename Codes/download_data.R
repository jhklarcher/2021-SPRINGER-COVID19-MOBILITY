# install.packages(c('tidyverse', 'openxlsx', 'zoo'))
library(tidyverse)
library(jsonlite)
library(zoo)
library(openxlsx)
library(httr)

# setando diretório
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Baixando os casos do Brasil.IO
temp <- tempfile()
download.file("https://data.brasil.io/dataset/covid19/caso_full.csv.gz", temp)
con <- gzfile(temp, "caso_full.csv")
data <- read.csv(con)
unlink(temp)
close(con)

# Criando o dataframe com o "caso_full.csv"
covid_data <- data %>%
  filter(place_type == "state") %>%
  group_by(city_ibge_code) %>%
  summarise(n=max(last_available_confirmed)) %>%
  top_n(10) %>%
  left_join(data, by = "city_ibge_code") %>%
  mutate(date = as.Date(date, "%Y-%m-%d"), city_ibge_code=floor(city_ibge_code)) %>%
  select(localidade=city_ibge_code, state, date, cum_confirmed=last_available_confirmed, cum_deaths = last_available_deaths, new_confirmed, new_deaths)

states <- read_csv("https://gist.githubusercontent.com/jhklarcher/2ca6a29ad655330db43c2ad11e2472fb/raw/e4836e33e6499c0cd0993eda1858457c447a8a39/brasil_uf.csv")

# Mobilidade do google
google_mob_raw <- read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")

google_mob <- google_mob_raw %>% 
  filter(country_region_code == "BR") %>%
  drop_na(sub_region_1) %>%
  mutate(state = iso_3166_2_code, date = as.Date(date, "%Y-%m-%d")) %>%
  select(-c("metro_area", "country_region_code", "country_region", "sub_region_1", "sub_region_2", "census_fips_code", "iso_3166_2_code")) %>%
  mutate(state = substr(state, 4, 5)) %>%
  rename(mob1 = retail_and_recreation_percent_change_from_baseline,
         mob2 = grocery_and_pharmacy_percent_change_from_baseline,
         mob3 = parks_percent_change_from_baseline,
         mob4 = transit_stations_percent_change_from_baseline,
         mob5 = workplaces_percent_change_from_baseline,
         mob6 = residential_percent_change_from_baseline) 

covid_data <- covid_data %>% left_join(google_mob, by=c("date", "state"))

# Remove linhas com NA
covid_data <- covid_data %>%
  drop_na(mob1)

# Exportando em csv
covid_data %>% write.csv("../Data/covid19-mobility-data.csv", row.names=FALSE)
covid_data %>% write_rds("../Data/covid19-mobility-data.rds")
