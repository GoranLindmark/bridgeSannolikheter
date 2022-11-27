#' Title filterRealisticHandTypes
#'
#' @param simhandTibble {list of hand distributions from handType1}
#'
#' @return simhandTibble
#' @export
#'
#'
filterRealisticHandTypes <- function( simhandTibble ){

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
  }
  return(simhandTibble)
}
