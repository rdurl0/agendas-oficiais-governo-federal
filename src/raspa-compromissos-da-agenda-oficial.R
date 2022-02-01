#' Raspagem de dados da agenda oficial de cargos do Governo Federal
#' 
#' @description Código para raspar dados das agendas oficiais do Governo Federal
#'
#' @param url url da agenda oficial
#'
#' @return `data.frame` com os dados completos da agenda.
#' @export
#'
#' @examples 
#' 
#' \dontrun{
#' 
#' url = "https://www.gov.br/planalto/pt-br/acompanhe-o-planalto/agenda-do-presidente-da-republica/2022-01-31"
#' raspa_compromissos_da_agenda_oficial(url = url)
#' 
#' }
raspa_compromissos_da_agenda_oficial <- function(url) {
  
  # extrai o conteúdo da url com todos os compromissos
  agenda <- url %>% 
    httr::GET() %>% 
    httr::content()

  # lista compromissos separadamente
  compromissos <- agenda %>%
    xml_find_all('//*[@id="content-core"]/div/ul') %>%
    xml_children()
  
  # título da agenda com nome e cargo
  agenda_titulo <- compromissos %>%
    map(xml_find_first, '//*[@id="content"]/h1') %>%
    map_chr(xml_text)
  
  # em dias em que não existe compromisso oficial não tem dado para raspar
  # condição pode ser verificada quando `agenda_titulo`retorna um objeto vazio:
  if (rlang::is_empty(agenda_titulo)) {
    
    compromissos  <- read_xml("<aviso> Não há registro de compromisso </aviso>")
    agenda_titulo <- NA
    agenda_cargo  <- NA
    agenda_nome   <- NA
    agenda_data   <- NA
    compromissos_publicado_em    <- NA
    compromissos_atualizado_em   <- NA
    compromissos_titulos         <- NA
    compromissos_locais          <- NA
    compromissos_horarios        <- tibble(inicio = NA, fim = NA)
    compromissos_detalhes        <- NA
    
  } else {
    
    # se existe compromisso oficial, então existe dado para raspar
    agenda_cargo <- compromissos %>%
      map(xml_find_first, '//*[@id="content"]/h1') %>%
      map_chr(xml_text) %>%
      str_remove("^Agenda ")
    
    agenda_nome <- compromissos %>%
      map(xml_find_first, '//*[@id="breadcrumbs-current"]') %>%
      map_chr(xml_text) %>%
      gsub("(^Agenda de )(.+)( para \\d{2}\\/\\d{2}\\/\\d{4})", "\\2",. )
    
    agenda_data <- compromissos %>%
      map(xml_find_first, '//*[@id="breadcrumbs-current"]') %>%
      map_chr(xml_text) %>%
      str_extract("\\d{2}\\/\\d{2}\\/\\d{4}") %>%
      lubridate::dmy()
    
    compromissos_publicado_em <- compromissos %>%
      map(xml_find_first, '//*[@id="plone-document-byline"]/span[1]/span[2]') %>%
      map_chr(xml_text) %>%
      lubridate::dmy_hm()
    
    compromissos_atualizado_em <- compromissos %>%
      map(xml_find_first, '//*[@id="plone-document-byline"]/span[2]/span[2]') %>%
      map_chr(xml_text) %>%
      lubridate::dmy_hm()
    
    # existem casos em que a ausência de compromisso é registrada no template da agenda
    # nesse caso, a agenda não terá informação de horários, descrição do compromisso, locais ou detalhes.
    agenda_vazia <- agenda %>% 
      xml_find_all('//li[@class="sem-compromisso item-compromisso"]') %>% 
      length(.) != 0
    
    if (agenda_vazia) {
      
      sem_compromisso <- agenda %>% 
        xml_find_all('//li[@class="sem-compromisso item-compromisso"]') %>% 
        xml_text() %>% 
        str_remove_all("\n") %>% 
        str_squish()
      
      compromissos_horarios <- tibble(inicio = NA, fim = NA)
      compromissos_titulos <- sem_compromisso
      compromissos_locais <- NA
      compromissos_detalhes <- sem_compromisso
      
    } else {
      
      # horário de inicio e fim do compromisso
      compromissos_horarios <- agenda %>%
        xml_find_all('//*[@class="horario"]') %>%
        map(xml_text) %>%
        str_remove_all("\n|\\s") %>%
        if_else(str_detect(., "-"), ., paste0(., "-")) %>% 
        str_split("-", simplify = T) %>%
        as_tibble(.name_repair = "unique") %>%
        rename(inicio = ...1, fim = ...2) %>%
        mutate(across(everything(), ~ if_else(. == "", NA_character_, .)))
      
      # título do compromisso agendado
      compromissos_titulos <- compromissos %>%
        xml_find_all('//*[contains(@class, "compromisso-titulo")]') %>%
        xml_text()
      
      # Local
      compromissos_locais <- compromissos %>%
        xml_find_all('//*[@class="compromisso-footer"]') %>%
        xml_text() %>% 
        str_remove_all("\n|Adicionar ao meu calendário") %>% 
        str_squish()
     
      # campo de detalhes possui qualquer outra iformação a respeito do compromisso
      `%notin%` <- function(x, y) !(x %in% y)
      compromissos_detalhes <- compromissos %>%
        map(xml_children) %>%
        map(xml_text) %>%
        map(paste0, collapse = "") %>%
        map(str_split, "\n", simplify = TRUE) %>%
        map(str_squish) %>%
        map(as_tibble_col, column_name = "detalhes") %>%
        map(filter, detalhes != "",
                           detalhes != "-",
                           detalhes != "Adicionar ao meu calendário",
                           detalhes %notin% compromissos_horarios$fim,
                           detalhes %notin% compromissos_horarios$inicio,
                           detalhes %notin% compromissos_titulos,
                           detalhes %notin% compromissos_locais) %>%
        map(pull, var = detalhes) %>%
        map_chr(paste, collapse = ";")
      
    }
   
  }
  
  # nesa tag são inseridas observações sobre fuso horário ou outras coisas. 
  compromissos_observacao <- agenda %>% 
    xml_find_all('//div[@class="portalMessage info"]') %>% 
    xml_text() %>% 
    str_remove_all("\n") %>% 
    str_squish()

  compromissos_observacao <- ifelse(rlang::is_empty(compromissos_observacao),
                                           "", compromissos_observacao)
  
  agenda_tb <- tibble(
    metadados = as_list(compromissos),
    agenda_titulo = agenda_titulo,
    agenda_cargo = agenda_cargo,
    agenda_nome = agenda_nome,
    agenda_data = agenda_data,
    compromissos_publicado_em = compromissos_publicado_em,
    compromissos_atualizado_em = compromissos_atualizado_em,
    compromissos_titulos = compromissos_titulos,
    compromissos_locais = compromissos_locais,
    compromissos_horarios_inicio = compromissos_horarios$inicio,
    compromissos_horarios_fim = compromissos_horarios$fim,
    compromissos_detalhes = compromissos_detalhes,
    compromissos_observacao = compromissos_observacao
  )

  return(agenda_tb)
  
}
