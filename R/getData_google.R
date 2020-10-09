getData_google <- function(nome){

  profile_data %>%
    dplyr::filter(docente %in% nome) -> perfis_selecionados

  teste <- has_profile(nome = nome)

  if (teste == FALSE){
    rlang::warn(message = paste(nome, "n�o possui  perfil no Google Scholar"))

    dat_retorno <- tibble::tibble(
      docente = nome,
      tipo = c("citacoes","h-index","i10-index"),
      total = NA,
      ultimos_5_anos = NA)

    return(dat_retorno)


  } else{
    rlang::inform(glue::glue("Iniciando coleta do perfil de {nome}"))
  }


  xml2::read_html(perfis_selecionados$link) %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(fill = TRUE) %>%
    .[[1]] -> dat


  dat_retorno <- tibble::tibble(
    docente = nome,
    tipo = c("citacoes","h-index","i10-index"),
    total = dat$All,
    ultimos_5_anos = dat$`Since 2015`)

  rlang::inform(message = glue::glue("Coleta do perfil de {nome} finalizada"))

  return(dat_retorno)

}
