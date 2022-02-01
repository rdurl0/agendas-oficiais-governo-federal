source(here::here("src/raspa-compromissos-da-agenda-oficial.R"), encoding = "utf-8")
library(tidyverse)
library(here)
library(glue)

# url base
url_base <- "https://www.gov.br/planalto/pt-br/conheca-a-vice-presidencia/agenda-vice-presidente/@@busca-agenda?portal_type:list=AgendaDiaria"

# inicia um iterador por página
pagina <- 0
termo <- "noruega"

url_busca <- glue::glue("{url_base}&b_start:int={pagina}&SearchableText={termo}")

# 
url_resultado_pagina <- url_busca %>% 
  GET() %>% 
  content() %>% 
  xml_find_all('//dt/a[@class="state-published"]') %>% 
  xml_attr("href")

url_resultado <- tibble(
  pagina = pagina,
  url_busca = url_busca,
  url_resultado_pagina = list(url_resultado_pagina)
)

url_resultado %>% unnest(url_resultado_pagina)

while (!rlang::is_empty(url_resultado_pagina)) {
  
  pagina <- pagina + 10
  
  url_busca <- glue::glue("{url_base}&b_start:int={pagina}&SearchableText={termo}")
  
  url_resultado_pagina <- url_busca %>% 
    GET() %>% 
    content() %>% 
    xml_find_all('//dt/a[@class="state-published"]') %>% 
    xml_attr("href")
  
  url_resultado_parcial <- tibble(
    pagina = pagina,
    url_busca = url_busca,
    url_resultado_pagina = list(url_resultado_pagina)
  )
  
  url_resultado <- bind_rows(url_resultado, url_resultado_parcial)
  print(url_resultado)
}


resultado <- url_resultado %>% 
  unnest(url_resultado_pagina) %>% 
  pull(url_resultado_pagina)

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

agendas_noruega <- resultado %>% 
  map(raspa_compromissos_da_agenda_oficial_safely) %>% 
  set_names(resultado) %>%
  enframe(name = "url") %>% 
  mutate(result = map(value, pluck, "result"),
         error = map(value, pluck, "error")) %>% 
  select(-value)

agendas_noruega  %>%
  select(-result) %>% print(n = Inf)

# entrega a base de dados com a agenda 
agenda_result <- agendas_noruega  %>%
  select(-error) %>% 
  unnest(result) %>% 
  select(-metadados)

link_planilha <- "https://docs.google.com/spreadsheets/d/1mdHwtmR37DyvzcjljQ9i-i5nj3BbO7bZvebb9dIpsoI/edit#gid=0"
googlesheets4::write_sheet(agenda_result, link_planilha, sheet = "Resultados da busca 'noruega'")
