#' Title
#'
#' @param nome
#' @param ano
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#'
getData_google <- function(Nome, Ano){

  if(Nome %in% profile_data$docente == FALSE){
    rlang::abort(message = "Scholar not  found. Please check  'profile_data' for available names.",
                 class = "Name not found")
  }


  teste <- has_profile(Nome = Nome)

  if (teste == FALSE){
    rlang::warn(message = paste(Nome, "doesn't have a profile at Google Scholar"))

    dat_retorno <- tibble::tibble(
      docente = Nome,
      tipo = c("citacoes","h-index","i10-index"),
      total = NA,
      ultimos_5_anos = NA)

    return(dat_retorno)


  } else{

    rlang::inform(glue::glue("Iniciando coleta do perfil de {Nome}"))

    profile_data %>%
      dplyr::filter(docente %in% Nome &
                      ano %in% Ano) -> perfis_selecionados

    xml2::read_html(perfis_selecionados$link) %>%
      rvest::html_nodes("table") %>%
      rvest::html_table(fill = TRUE) %>%
      .[[1]] -> dat


    todos <- dat$All
    h5 <- dat$`Since 2015`


    dat_retorno <- tibble::tibble(
      docente = Nome,
      tipo = c("citacoes","h-index","i10-index"),
      total = todos,
      ultimos_5_anos = h5)

    rlang::inform(message = glue::glue("Coleta do perfil de {Nome} finalizada"))

    return(dat_retorno)
  }




}

