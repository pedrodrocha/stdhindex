has_profile <- function(nome){

  profile_data %>%
    dplyr::filter(docente %in% nome) -> para_teste

  if(is.na(para_teste$link)){
    return(FALSE)
  } else {

    return(TRUE)
  }

}
