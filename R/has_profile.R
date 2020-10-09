#' Title
#'
#' @param nome
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
has_profile <- function(Nome){

  profile_data %>%
    dplyr::filter(docente %in% Nome) -> para_teste

  if(is.na(para_teste$link[1])){
    return(FALSE)
  } else {

    return(TRUE)
  }

}
