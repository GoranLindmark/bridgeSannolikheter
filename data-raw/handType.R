#' handType
#'
#' @param hand
#'
#' @return hand
#' @export
#' @importFrom magrittr '%>%'
#' @import dplyr tidyr
#'
#' @examples
handType <- function(hand) {
  x <- c("S", "H", "R", "K")
  hand <-
    hand %>%
    dplyr::group_by(color) %>%
    dplyr::summarize(antal = n()) %>%
    dplyr::slice(match(x, color)) %>%
  tidyr::pivot_wider(names_from = color, values_from = antal)
  return(hand)
}

