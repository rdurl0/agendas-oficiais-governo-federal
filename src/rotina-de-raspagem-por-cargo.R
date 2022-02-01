# carrega a função que faz o trabalho de raspagem:
library(tidyverse)
library(here)
library(glue)
library(xml2)
library(httr)
source(here::here("src/raspa-compromissos-da-agenda-oficial.R"), encoding = "utf-8")

# define um conjunto de urls sequenciadas por data:
url_base <- "https://www.gov.br/planalto/pt-br/nova-vice-presidencia/agenda-vice-presidente"
data <- seq.Date(lubridate::dmy("01-01-2019"), lubridate::today(), by = "days")

urls <- glue::glue("{url_base}/{data}")

# Após criar as URLS eu vou testar quais delas de fato existem:
pega_datas_validas <- function(url) {
  
  calendario <- httr::GET(url)
  return(calendario$status_code)
  
}

# guarda somente aquelas URLs que existem
urls_validas <- map_int(urls, pega_datas_validas) %>% 
  set_names(urls) %>%
  enframe(name = "url", value = "status") %>% 
  filter(status == 200L)

# essa tabela vazia é para o método purrr::safely, onde o loop é performado
# com relatório e debug de erros (caso existam).
agenda_tb <- tibble(
  raw_data      = NA,
  agenda_titulo = NA,
  agenda_cargo  = NA,
  agenda_nome   = NA,
  agenda_data   = NA,
  compromissos_publicado_em    = NA,
  compromissos_atualizado_em   = NA,
  compromissos_titulos         = NA,
  compromissos_locais          = NA,
  compromissos_horarios_inicio = NA,
  compromissos_horarios_fim    = NA,
  compromissos_detalhes        = NA
)

# criando o metodo purrr:safely
raspa_compromissos_da_agenda_oficial_safely <- safely(raspa_compromissos_da_agenda_oficial, otherwise = agenda_tb)

# a raspagem da agenda
agenda_full <- map(urls_validas$url, raspa_compromissos_da_agenda_oficial_safely) 

# atribui ID's às agendas e cria um dataframe.
# "result" é onde está o dado raspado corretamente
# "error" guarda eventuais erros, quando aparece algum eu faço a manutenção da função de raspagem
agenda_full <- agenda_full %>% 
  set_names(urls_validas$url) %>%
  enframe(name = "url") %>% 
  mutate(result = map(value, pluck, "result"),
         error = map(value, pluck, "error")) %>% 
  select(-value)

# verifica se há erros
agenda_full %>%
  select(-result) %>% 
  print(n = Inf)

# entrega a base de dados com a agenda 
agenda_result <- agenda_full %>%
  select(-error) %>% 
  unnest(result) %>% 
  select(-metadados)

link_planilha <- "https://docs.google.com/spreadsheets/d/1fLdVj6_jMU4bbEABwPOAZp1qQ58n5CrpdCt2S3XbJec/edit#gid=0"
googlesheets4::write_sheet(agenda_result, link_planilha, sheet = "Vice-presidente")
