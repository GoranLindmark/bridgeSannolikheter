
#' create_deck
#'
#' @return deck
#' @export
#'
#' import tibble
create_deck <- function(){



spades <-
  tibble::tibble(color = rep("Sp", 13), value = 1:13)
harts <-
  tibble::tibble(color = rep("Ha", 13), value = 1:13)
diamonds <-
  tibble::tibble(color = rep("Di", 13), value = 1:13)
clubs <-
  tibble::tibble(color = rep("Cl", 13), value = 1:13)

deck <- tibble::bind_rows(spades, harts, diamonds, clubs)
rm(spades, harts, diamonds, clubs)

deck
}
