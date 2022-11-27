#' shuffle_deck
#'
#' @return deck {a shuffeled deck of cards}
#' @param card_deck {a carddeck created by create_deck()}
#' @export
#'
#' @import dplyr
#' @importFrom magrittr %>%
shuffle_deck <- function(card_deck) {
  deck <-
    dplyr::bind_cols(card_deck, order = sample(1:52, 52, replace = F)) %>%
    dplyr::arrange(order)

  deck
}
