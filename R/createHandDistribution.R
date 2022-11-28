
#' @title  createHandDistribution
#'
#' @param NoSimulations { number of simulations to perform}
#'
#' @return simhandTibble { a tibble with the simulations}
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import tidyr dplyr
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


  generateHandCodes <- function( simhandTibble ){

    simhandTibble$handCode <- ""
    for (i in 1:nrow(simhandTibble)){
      x <- c(  simhandTibble$S[i], simhandTibble$H[i], simhandTibble$R[i], simhandTibble$K[i])
      if (all(x %in% c(4, 3, 3, 3))){
        simhandTibble$handCode[i] <- 4333
      }
      if (all(x %in% c(4, 4, 3, 2))){
        simhandTibble$handCode[i] <- 4432
      }
      if (all(x %in% c(4, 5, 2, 2))){
        simhandTibble$handCode[i] <- 4432
      }
      if (all(x %in% c(4, 4, 4, 1))){
        simhandTibble$handCode[i] <- 4432
      }
      if (all(x %in% c(5, 3, 3, 2))){
        simhandTibble$handCode[i] <- 5332
      }
      if (all(x %in% c(5, 4, 3, 1))){
        simhandTibble$handCode[i] <- 5431
      }
      if (all(x %in% c(5, 5, 2, 1))){
        simhandTibble$handCode[i] <- 5521
      }
      if (all(x %in% c(6, 3, 3, 1))){
        simhandTibble$handCode[i] <- 6331
      }
      if (all(x %in% c(6, 3, 2, 2))){
        simhandTibble$handCode[i] <- 6322
      }
      if (all(x %in% c(6, 4, 2, 1))){
        simhandTibble$handCode[i] <- 6421
      }
      if (all(x %in% c(6, 5, 1, 1))){
        simhandTibble$handCode[i] <- 6511
      }
      if (all(x %in% c(7, 4, 1, 1))){
        simhandTibble$handCode[i] <- 7411
      }
      if (all(x %in% c(7, 3, 2, 1))){
        simhandTibble$handCode[i] <- 7321
      }
      if (all(x %in% c(7, 2, 2, 2))){
        simhandTibble$handCode[i] <- 7222
      }
      if (all(x %in% c(1,2,2,8))){
        simhandTibble$handCode[i] <- 8221
      }
      if (all(x %in% c(1,1,3,8))){
        simhandTibble$handCode[i] <- 8311
      }


      if (all(x %in% c(5,7,1,08))){
        simhandTibble$handCode[i] <- 5710
      }
      if (all(x %in% c(5,6,2,0))){
        simhandTibble$handCode[i] <- 5620
      }
      if (all(x %in% c(5,5,3,0))){
        simhandTibble$handCode[i] <- 5530
      }
      if (all(x %in% c(5,4,4,0))){
        simhandTibble$handCode[i] <- 5440
      }
      if (all(x %in% c(6,6,1,0))){
        simhandTibble$handCode[i] <- 6610
      }
      if (all(x %in% c(6,5,2,0))){
        simhandTibble$handCode[i] <- 6520
      }
      if (all(x %in% c(6,4,3,0))){
        simhandTibble$handCode[i] <- 6430
      }
      if (all(x %in% c(9,3,1,0))){
        simhandTibble$handCode[i] <- 9310
      }
      if (all(x %in% c(9,2,2,0))){
        simhandTibble$handCode[i] <- 9220
      }
      if (all(x %in% c(9,4,0,0))){
        simhandTibble$handCode[i] <- 9400
      }








    }
    return(simhandTibble)
  }


# ------------------------  MAIN



  for (i in 1:NoSimulations) {
    x <-  handType(simHand())
    if (ncol(x) == 3) {
      x <- cbind(x, K = 0)
      names(x) = c("S", "H", "R", "K")
    }
    if (ncol(x) == 2) {
      x <- cbind(x, R = 0, K = 0)
      names(x) = c("S", "H", "R", "K")
    }

    if (i == 1) {
      simhandTibble <- x
    } else {
      simhandTibble <- rbind(simhandTibble, x)
    }
  }

  generateHandCodes(simhandTibble) %>%
    dplyr::select(handCode)
}
