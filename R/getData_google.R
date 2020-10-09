#' Title
#'
#' @param nome
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#'
getData_google <- function(nome){

  if(nome %in% profile_data$docente == FALSE){
    rlang::abort(message = "Scholar not found. Please check  'profile_data' for available names.",
                 class = "Name not found")
  }


  teste <- has_profile(nome = nome)

  if (teste == FALSE){
    rlang::warn(message = paste(nome, " Doesn't have a profile at Google Scholar"))

    dat_retorno <- tibble::tibble(
      docente = nome,
      tipo = c("citacoes","h-index","i10-index"),
      total = NA,
      ultimos_5_anos = NA)

    return(dat_retorno)


  } else{
    rlang::inform(glue::glue("Iniciando coleta do perfil de {nome}"))
  }


  profile_data %>%
    dplyr::filter(docente %in% nome) -> perfis_selecionados

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

