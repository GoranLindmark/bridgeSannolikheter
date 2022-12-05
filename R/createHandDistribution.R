
#' @title  createHandDistribution
#'
#' @param NoSimulations { number of simulations to perform}
#'
#' @return simhandTibble { a tibble with the simulations}
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import tidyr dplyr ggplot2
#'
#'
createHandDistribution <- function(NoSimulations){


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

  handCode <- function( simhandTibble){

    handCode <- list()

    for (i in 1:nrow(simhandTibble)){
      hand<- sort(as.integer(simhandTibble[i,]), decreasing = T)

      handCode[[i]] <- paste0(hand[1], hand[2], hand[3], hand[4])
    }

    return(data.frame(handCode = unlist(handCode)))

  }


# ------------------------  MAIN



  for (i in 1:NoSimulations) {
    x <-  handType(simHand())
    if (ncol(x) == 3) {
      x <- cbind(x, K = 0)
      names(x) = c("S", "H", "R", "K")
      x[4] <- as.integer(x[4])
    }
    if (ncol(x) == 2) {
      x <- cbind(x, R = 0, K = 0)
      names(x) = c("S", "H", "R", "K")
      x[3] <- as.integer(x[3])
      x[4] <- as.integer(x[4])
    }

    if (i == 1) {
      simhandTibble <- x
    } else {
      simhandTibble <- rbind(simhandTibble, x)
    }
  }

  handCode(simhandTibble) %>%

    dplyr::group_by(handCode) %>%
    dplyr::summarize(antal = n()) %>%
    dplyr::mutate(handCode = as.factor(handCode)) %>%
    ggplot2::ggplot(  ) +
    ggplot2::geom_col( aes(x = reorder(handCode, -antal), y = antal) )

}
