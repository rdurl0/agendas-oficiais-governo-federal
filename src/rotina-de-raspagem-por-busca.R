# carrega a função que faz o trabalho de raspagem:
library(tidyverse)
library(here)
library(glue)
library(xml2)
library(httr)
source(here::here("src/raspa-compromissos-da-agenda-oficial.R"), encoding = "utf-8")

# url base
url_base <- "https://www.gov.br/planalto/pt-br/conheca-a-vice-presidencia/agenda-vice-presidente/@@busca-agenda?portal_type:list=AgendaDiaria"

raspa_resultado_busca <- function(url_base, pagina, termo) {
  
  # pagina <- 40
  # termo = "noruega"
  url_busca <- glue::glue("{url_base}&b_start:int={pagina}&SearchableText={termo}")
  
  resultado_pagina <- url_busca %>% 
    GET() %>% 
    content()

  atualizado_por <- resultado_pagina %>% 
    xml_find_all('//dd/span/span[@class="documentAuthor"]') %>% 
    xml_text() %>% 
    str_remove("^por ")

  # print(rlang::is_empty(atualizado_por))
  # stopifnot("Acabaram os resultados" = !rlang::is_empty(atualizado_por))
    
  localizado_em <- resultado_pagina %>% 
    xml_find_all('//cite') %>% 
    xml_text() %>% 
    str_split("\n") %>% 
    map(str_squish) %>%
    map_chr(paste0, collapse = ";") %>% 
    str_remove_all("^;Localizado em;+|;+$") %>% 
    str_replace_all(";+", "; ")
  
  url_resultado_pagina <- resultado_pagina %>% 
    xml_find_all('//dt/a[@class="state-published"]') %>% 
    xml_attr("href")
  
  url_resultado <- tibble(
    pagina = (pagina + 10)/10,
    termo_buscado = termo,
    url_busca = url_busca,
    atualizado_por = list(atualizado_por),
    localizado_em = list(localizado_em),
    url_resultado_pagina = list(url_resultado_pagina)
  )
  
  return(url_resultado)
  
}

pagina <- 0
url_resultado <- raspa_resultado_busca(url_base = url_base, pagina = pagina, termo = "noruega")
url_resultado_pagina <- url_resultado$url_resultado_pagina

while (!rlang::is_empty(url_resultado_pagina)) {
  
  pagina <- pagina + 10
  
  url_resultado_parcial <- raspa_resultado_busca(url_base = url_base, pagina = pagina, termo = "noruega")
  url_resultado_pagina <- unlist(url_resultado_parcial$url_resultado_pagina)

  url_resultado <- bind_rows(url_resultado, url_resultado_parcial)
  print(url_resultado)
  
}

url_resultado <- url_resultado %>% 
  unnest(cols = c(atualizado_por, localizado_em, url_resultado_pagina))

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

agendas_buscadas <- url_resultado$url_resultado_pagina %>% 
  map(raspa_compromissos_da_agenda_oficial_safely) %>% 
  set_names(url_resultado$url_resultado_pagina) %>%
  enframe(name = "url_resultado_pagina") %>% 
  mutate(result = map(value, pluck, "result"),
         error = map(value, pluck, "error")) %>% 
  select(-value) %>% 
  left_join(url_resultado, .)

agendas_buscadas


link_planilha <- "https://docs.google.com/spreadsheets/d/1mdHwtmR37DyvzcjljQ9i-i5nj3BbO7bZvebb9dIpsoI/edit#gid=0"
agendas_buscadas %>% 
  select(-error) %>% 
  unnest(result) %>% 
  select(-metadados) %>% 
  googlesheets4::write_sheet(link_planilha, sheet = "Resultados da busca 'noruega'")
