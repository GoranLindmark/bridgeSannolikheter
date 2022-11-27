#' simHand
#'
#' @return hand  {a deck of cards}
#' @export
#' @import tidyr dplyr
#'
#'
simHand <- function() {
  CardType <- function() {
    cards <- tidyr::tibble(cardNo = 1:52)
    cards$color <- "K"
    cards$color[14:26] <- "R"
    cards$color[27:39] <- "H"
    cards$color[40:52] <- "S"
    cards$value = as.character(rep(1:13, 4))
    cards$value[1] <- c("A")
    cards$value[10:14] <- c("T", "Kn", "Q", "K", "A")
    cards$value[23:27] <- c("T", "Kn", "Q", "K", "A")
    cards$value[36:40] <- c("T", "Kn", "Q", "K", "A")
    cards$value[49:52] <- c("T", "Kn", "Q", "K")
    return(cards)
  }
  x <- tidyr::tibble(x = sample.int(52, 13))
  hand <- dplyr::inner_join (CardType(), x, by = c("cardNo" = "x"))

  hand
}
