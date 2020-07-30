# install.packages(c('tidyverse', 'openxlsx', 'zoo'))
library(tidyverse)
library(jsonlite)
library(zoo)
library(openxlsx)
library(httr)

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
  summarise(n=mean(estimated_population_2019)) %>%
  top_n(10) %>%
  left_join(data, by = "city_ibge_code") %>%
  mutate(date = as.Date(date, "%Y-%m-%d"), city_ibge_code=floor(city_ibge_code)) %>%
  select(localidade=city_ibge_code, state, date, cum_confirmed=last_available_confirmed, cum_deaths = last_available_deaths, new_confirmed, new_deaths)

# ---- Obtendo dados do ministério da saúde ----- #
# Headers necessários para obter o json
httpResponse <- GET('https://xx9p7hp1p7.execute-api.us-east-1.amazonaws.com/prod/PortalGeral',
                    add_headers('authority' = 'xx9p7hp1p7.execute-api.us-east-1.amazonaws.com',
                                'accept' = 'application/json, text/plain, */*',
                                'sec-fetch-dest' = 'empty',
                                'x-parse-application-id' = 'unAFkcaNDeXajurGB7LChj8SgQYS2ptm',
                                'origin' = 'https://covid.saude.gov.br',
                                'sec-fetch-site' = 'cross-site',
                                'sec-fetch-mode' = 'cors',
                                'referer' = 'https://covid.saude.gov.br/',
                                'accept-language' = 'en-US,en;q=0.9,pt;q=0.8'),
                    accept_json())

mins_url <- fromJSON(content(httpResponse, "text"), flatten = TRUE)$results$arquivo.url

mins_data <- openxlsx::read.xlsx(mins_url) %>%
  filter(is.na(municipio), (estado != is.na(estado))) %>%
  mutate(data = openxlsx::convertToDate(data), coduf = as.numeric(coduf)) %>%
  select(localidade = coduf,
         date = data,
         cum_cases_mins = casosAcumulado,
         cum_deaths_mins = obitosAcumulado,
         new_cases_mins = casosNovos,
         new_deaths_mins = obitosNovos)

# Agrupando com os dados do brasilIO
covid_data <- covid_data %>%
  left_join(mins_data, by = c("date", "localidade")) %>%
  replace_na(list(cum_cases_mins = 0,
                  cum_deaths_mins = 0,
                  new_cases_mins = 0,
                  new_deaths_mins = 0))
states <- read_csv("https://gist.githubusercontent.com/jhklarcher/2ca6a29ad655330db43c2ad11e2472fb/raw/e4836e33e6499c0cd0993eda1858457c447a8a39/brasil_uf.csv")

# Mobilidade do google
google_mob_raw <- read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")

google_mob <- google_mob_raw %>% 
  filter(country_region_code == "BR") %>%
  drop_na(sub_region_1) %>%
  mutate(state = iso_3166_2_code, date = as.Date(date, "%Y-%m-%d")) %>%
  select(-c("metro_area", "country_region_code", "country_region", "sub_region_1", "sub_region_2", "census_fips_code", "iso_3166_2_code")) %>%
  mutate(state = substr(state, 4, 5)) %>%
  rename_at(vars(setdiff(names(.), c("date", "state"))), 
            function(x) paste0("gl_", x)) 

covid_data <- covid_data %>% left_join(google_mob, by=c("date", "state"))

# Mobilidade da Apple
apple_mob_raw <- read_csv("https://covid19-static.cdn-apple.com/covid19-mobility-data/2013HotfixDev10/v3/en-us/applemobilitytrends-2020-07-27.csv")

apple_mob <- apple_mob_raw %>%
  filter(geo_type == "sub-region", country == "Brazil") %>%
  rename(name = region) %>%
  select(-geo_type, -country, -alternative_name, -`sub-region`) %>%
  gather(key = "date", value = "valor", -name, -transportation_type ) %>%
  spread(transportation_type, valor) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  rename_at(vars(setdiff(names(.), c("name", "date"))), 
            function(x) paste0("ap_", x))%>%
  left_join(states, by="name") %>%
  select(-name)


covid_data <- covid_data %>% left_join(apple_mob, by=c("date", "state"))

covid_data <- covid_data %>%
  group_by(state) %>%
  arrange(state, date) %>%
  mutate(ap_driving = replace(ap_driving, is.na(ap_driving), mean(ap_driving, na.rm=TRUE))) %>%
  ungroup()


# Exportando em csv
covid_data %>% write.csv("./data/covid19-mobility-data.csv", row.names=FALSE)

