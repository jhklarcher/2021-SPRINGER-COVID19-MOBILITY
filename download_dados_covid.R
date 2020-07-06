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
  filter(place_type == "city") %>%
  group_by(city_ibge_code) %>%
  summarise(n=mean(estimated_population_2019)) %>%
  top_n(10) %>%
  left_join(data, by = "city_ibge_code") %>%
  mutate(date = as.Date(date, "%Y-%m-%d"), city_ibge_code=floor(city_ibge_code/10)) %>%
  select(city, localidade=city_ibge_code, state, date, cum_confirmed=last_available_confirmed, cum_deaths = last_available_deaths, new_confirmed, new_deaths)

# Coletando dados climáticos do INMET
today <- Sys.Date()
base_url <- 'https://apitempo.inmet.gov.br/estacao/2020-01-01/'
estacoes <- read_csv('https://gist.githubusercontent.com/jhklarcher/a3ed03fc0e83ff37cfb83e890e6d1626/raw/d418fa210a8ab51e946de59506aa4b07ed763330/estacoes-automaticas.csv')

climate_data_raw <- tibble()

for(estacao in estacoes$estacao) {
  station_data <- fromJSON(paste(base_url, today, "/", estacao, sep = ""), flatten=T)
  climate_data_raw <- bind_rows(climate_data_raw, station_data)
}

# Obtendo valores médios de temperatura e umidade por dia
climate_data <- climate_data_raw %>%
  mutate(TEM_INS = as.numeric(TEM_INS),
         UMD_INS = as.numeric(UMD_INS)) %>%
  group_by(estacao = CD_ESTACAO, date = DT_MEDICAO) %>%
  summarise(temp_media = round(mean(TEM_INS, na.rm = TRUE), digits = 2),
            umid_media = round(mean(UMD_INS, na.rm = TRUE), digits = 2)) %>%
  left_join(estacoes, by = "estacao") %>%
  mutate(date = as.Date(date, "%Y-%m-%d"),
         localidade = floor(localidade/10))

# Agrupando com os dados do COVID
covid_data <- covid_data %>%
  left_join(climate_data, by = c("date", "localidade", "city")) %>%
  select(-estacao) %>%
  mutate(umid_media = na.approx(umid_media)) # interpolando dados faltantes

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
  filter(municipio != is.na(municipio)) %>%
  mutate(data = openxlsx::convertToDate(data), codmun = as.numeric(codmun)) %>%
  select(localidade = codmun,
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


# ----- Dados de registro de mortes do RegistroCivil.org.br ----- #
headers <- # headers para fazer a request do json
  add_headers(
    'Accept' = 'application/json, text/plain, */*',
    'Accept-Encoding' = 'gzip, deflate, br',
    'Accept-Language' = 'pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7',
    'Connection' = 'keep-alive',
    'Cookie' = '_ga=GA1.3.1820811173.1592944442; _hjid=378c326a-cc6f-4d45-aae4-2c54461ac57b; XSRF-TOKEN=eyJpdiI6IndIcmpRbmQ4SjE0ZmVLMGoyN1JCWnc9PSIsInZhbHVlIjoiXC9FQ1RYdzlLRHRRb3NRZlk2NHZnUjBKWVhzMFFyS0hPXC9tWlhTK2tvaFdjaVJETGQ0MlBMVGFmSUczd1JUVlF5IiwibWFjIjoiZjJkMjNkOGI1MDFlYzAwZDE4ZjMxNjQzNDM3OTNiMzliYjg1MGIyMTMwMWZlNzY4NDA5OTdhZTA5YmIxODUwOSJ9; _session=eyJpdiI6ImZaNkFjcFJyRnI5WWVLRDVmMU5Yd3c9PSIsInZhbHVlIjoicGEwdmY2Z1NWSThHc3RyVlpFNDQ2SHBjd2hqcHlialF0MG9EbUM0ODhiVUJMa3dQZTRtUWRJaTY2YTBiNkc0NyIsIm1hYyI6IjliNDg2MzBmYjczOGU0ZjY4MTVhMTFhMGU5N2Q3ZWJhYjYxZDMyYzVmNTdmOTc1NmJiMjNiMDA3YTNhYzc5MzgifQ%3D%3D; _gid=GA1.3.921194741.1593094851; _gat=1',
    'Host' = 'transparencia.registrocivil.org.br',
    'recaptcha' = '03AGdBq26s48OSf8LTmzUknOH_gqKL4MFt0dqSe1UsfDNashE7pm6ZFTMdNh-q3Jxly-K6-952dXsSAUmdrk9OaHO9LiHSkdY-tKcITnU2KoShBrAXWd8VXyB8XAPHqC2DgibF2yT0yuYbpjBPrWBxLeZRvUbHNVDQV_b0GeJSfugLCgqnXwup3ihDY0vrNkDXb_1OrZHTA7_W5U0XUbQl6-RR4sOGsNHFi2yK7-06uacAJkVT0Xkp5BqxWGZFRvcDyrrvHMzYdeKmw-r1Xs5iafFTIs5UQndA-8MviNtMmFu5eFJNp5t6GMm0SURRRVc8L3hRrVgz5rNTrEYVBaRWYye7b5F42faqkUfsU9yg9wu8VaE6c1PxGKITNggV6xZRApx-yDM1QTW_orwtnND3eFJLhQ9HfLsgiA',
    'Referer' = 'https://transparencia.registrocivil.org.br/especial-covid',
    'Sec-Fetch-Dest' = 'empty',
    'Sec-Fetch-Mode' = 'cors',
    'Sec-Fetch-Site' = 'same-origin',
    'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko), Chrome/81.0.4044.138 Safari/537.36',
    'X-CSRF-TOKEN' = 'GK0AlL84lLjlMRmpNjVXZQaUUdrySHU7xnvwDMMO',
    'X-Requested-With' = 'XMLHttpRequest',
    'X-XSRF-TOKEN' = 'eyJpdiI6IndIcmpRbmQ4SjE0ZmVLMGoyN1JCWnc9PSIsInZhbHVlIjoiXC9FQ1RYdzlLRHRRb3NRZlk2NHZnUjBKWVhzMFFyS0hPXC9tWlhTK2tvaFdjaVJETGQ0MlBMVGFmSUczd1JUVlF5IiwibWFjIjoiZjJkMjNkOGI1MDFlYzAwZDE4ZjMxNjQzNDM3OTNiMzliYjg1MGIyMTMwMWZlNzY4NDA5OTdhZTA5YmIxODUwOSJ9'
  )

# URLs de request para cada uma das 10 maiores cidades
urls <- c('https://transparencia.registrocivil.org.br/api/covid?data_type=data_ocorrido&start_date=2020-03-16&end_date=2020-06-25&search=death-covid&city_id=3009&state=PR',
          'https://transparencia.registrocivil.org.br/api/covid?data_type=data_ocorrido&start_date=2020-03-16&end_date=2020-06-25&search=death-covid&city_id=1&state=SP',
          'https://transparencia.registrocivil.org.br/api/covid?data_type=data_ocorrido&start_date=2020-03-16&end_date=2020-06-25&search=death-covid&city_id=4646&state=RJ',
          'https://transparencia.registrocivil.org.br/api/covid?data_type=data_ocorrido&start_date=2020-03-16&end_date=2020-06-25&search=death-covid&city_id=855&state=BA',
          'https://transparencia.registrocivil.org.br/api/covid?data_type=data_ocorrido&start_date=2020-03-16&end_date=2020-06-25&search=death-covid&city_id=1280&state=CE',
          'https://transparencia.registrocivil.org.br/api/covid?data_type=data_ocorrido&start_date=2020-03-16&end_date=2020-06-25&search=death-covid&city_id=4750&state=MG',
          'https://transparencia.registrocivil.org.br/api/covid?data_type=data_ocorrido&start_date=2020-03-16&end_date=2020-06-25&search=death-covid&city_id=771&state=AM',
          'https://transparencia.registrocivil.org.br/api/covid?data_type=data_ocorrido&start_date=2020-03-16&end_date=2020-06-25&search=death-covid&city_id=2603&state=PE',
          'https://transparencia.registrocivil.org.br/api/covid?data_type=data_ocorrido&start_date=2020-03-16&end_date=2020-06-25&search=death-covid&city_id=1574&state=GO',
          'https://transparencia.registrocivil.org.br/api/covid?chart=chart4&data_type=data_ocorrido&search=death-covid&state=DF&start_date=2020-03-16&end_date=2020-06-25')

localidades <- c(410690, 355030, 330455, 292740, 230440, 310620, 130260, 261160, 520870, 530010)

registro_data_raw  <- tibble()

for(i in 1:10) {
  
  httpResponse <-  GET(urls[i],
                       headers,
                       accept_json())
  
  registro <-  fromJSON(content(httpResponse, "text"), flatten = TRUE)$chart
  
  registros <- tibble(date = names(registro),
                      deaths_reg = registro,
                      localidade = localidades[i]) %>%
    mutate(date = paste0(date, '/2020'), deaths_reg = as.numeric(deaths_reg)) %>%
    mutate(date = as.Date(date, "%d/%m/%Y"))
  
  registro_data_raw <- bind_rows(registro_data_raw, registros)
}

# Agrupando com os dados diários
covid_data <- covid_data %>%
  left_join(registro_data_raw, by = c("date", "localidade")) %>%
  mutate(deaths_reg = replace_na(deaths_reg, 0))


#------- Dados socioeconômicos -------#
# Densidade populacional
dens_pop <- fromJSON('https://servicodados.ibge.gov.br/api/v1/pesquisas/33/periodos/2010/indicadores/29168/ranking', flatten=T)$res[[1]]%>%
  select(localidade, dens_pop=res) %>%
  mutate(localidade=as.numeric(localidade))

# IDH dos municípios
idh <- fromJSON('https://servicodados.ibge.gov.br/api/v1/pesquisas/37/periodos/2010/indicadores/30255/ranking', flatten=T)$res[[1]] %>%
  select(localidade, idh=res) %>%
  mutate(localidade=as.numeric(localidade))

# PIP dos Municípios
pib <- fromJSON('https://servicodados.ibge.gov.br/api/v1/pesquisas/38/periodos/2017/indicadores/46997/ranking', flatten=T)$res[[1]] %>%
  select(localidade, pib=res) %>%
  mutate(localidade=as.numeric(localidade))

# Índice de Gini
ind_gini_2003 <- fromJSON('https://servicodados.ibge.gov.br/api/v1/pesquisas/36/periodos/2003/indicadores/30252/ranking', flatten=T)$res[[1]] %>%
  select(localidade, ind_gini=res) %>%
  mutate(localidade=as.numeric(localidade))

# Esgotamento sanitário
esg_sanitario <- fromJSON('https://servicodados.ibge.gov.br/api/v1/pesquisas/10058/periodos/2010/indicadores/60030/ranking/', flatten=T)$res[[1]] %>%
  select(localidade, esg_sanitario=res) %>%
  mutate(localidade=as.numeric(localidade))

# Mortalidade infantil
mort_infantil <- fromJSON('https://servicodados.ibge.gov.br/api/v1/pesquisas/10058/periodos/2012-2014/indicadores/60033/ranking', flatten=T)$res[[1]] %>% # por mil nascidos vivos
  select(localidade, mort_infantil=res) %>%
  mutate(localidade=as.numeric(localidade))

# Taxa de Escolaridade
tx_escol <- fromJSON('https://servicodados.ibge.gov.br/api/v1/pesquisas/10058/periodos/2010/indicadores/60045/ranking', flatten=T)$res[[1]] %>% # % 6 a 14 anos de idade 
  select(localidade, tx_escol=res) %>%
  mutate(localidade=as.numeric(localidade))

# Agregando os dados
city_data <- data  %>%
  filter(place_type == "city") %>%
  group_by(localidade=city_ibge_code) %>%
  summarise(city=first(city), estimated_population_2019=mean(estimated_population_2019)) %>%
  arrange(desc(estimated_population_2019)) %>%
  mutate(localidade=floor(localidade/10)) %>%
  top_n(10) %>%
  left_join(idh, by='localidade') %>%
  left_join(pib, by='localidade') %>%
  left_join(dens_pop, by='localidade') %>%
  left_join(ind_gini_2003, by='localidade') %>%
  left_join(esg_sanitario, by='localidade') %>%
  left_join(mort_infantil, by='localidade') %>%
  left_join(tx_escol, by='localidade')

# Exportando em csv
covid_data %>% write.csv("./dados/covid-municipios.csv", row.names=FALSE)
city_data %>% write.csv("./dados/dados-municipios.csv", row.names=FALSE)

