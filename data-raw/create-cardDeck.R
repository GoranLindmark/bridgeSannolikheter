
#' create_deck
#'
#' @return deck {this is a complete deck of cards}
#' @export
#'
#' @import tidyr dplyr
#' @importFrom magrittr %>%
#'
create_deck <- function(){



spades <-
  tidyr::tibble(color = rep("Sp", 13), value = 1:13)
harts <-
  tidyr::tibble(color = rep("Ha", 13), value = 1:13)
diamonds <-
  tidyr::tibble(color = rep("Di", 13), value = 1:13)
clubs <-
  tidyr::tibble(color = rep("Cl", 13), value = 1:13)

deck <- dplyr::bind_rows(spades, harts, diamonds, clubs)
rm(spades, harts, diamonds, clubs)

deck
}
