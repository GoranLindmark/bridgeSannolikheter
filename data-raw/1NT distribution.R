#  Balanced hand is characterized by 5332, 4432, 4333

#  1NT opening is balanced hand and 15 - 17 hp

#  We will shuffle the deck and deal cards in a randomized way. All hands will be analyzed to see the criteria. This implies that we will get  4 outcomes on every deal.
#  This implies that there can be some hands that has more than one 1NT opening hand. 

#  How common is :
#  a) even hands
#  b) how commom is 1NT even hands
#  c) Opening hand distributions

load_card_deck <- function(){
  library(readxl)
  card_deck <<- read_excel("card_deck.xlsx")
  card_prio <<- read_excel("card_deck.xlsx",
                          sheet = "suite_prio")
  
}

library(tidyverse)

shuffle_deck <- function(){
  bind_cols(card_deck, order = sample(1:52, 52, replace=F)) %>%
    arrange(order)
  
}
deal_cards <- function(deck){
  bind_cols(deck, hand = rep(c('N', 'Ö', 'S', 'V') , 13)) 
  
}

fördelning_alla_händer <- function(bricka){
  bricka %>% 
    group_by(hand, suite) %>% 
    summarize(antal = n(),
              .groups = 'drop') %>%
    pivot_wider(names_from = suite, values_from = antal, values_fill = 0) %>%
    select(-hand) %>%
    mutate(fördeln = paste0(.$clubs, .$dimonds, .$hearts, .$spades)) %>%
    select(fördeln)
}

points_all_hands <- function(){
  shuffle_deck <- function(){
    bind_cols(card_deck, order = sample(1:52, 52, replace=F)) %>%
      arrange(order)
    
  }
  deal_cards <- function(deck){
    bind_cols(deck, hand = rep(c('N', 'Ö', 'S', 'V') , 13)) 
    
  }
  deal_cards(shuffle_deck()) %>% 
    mutate(value = ifelse(value < 11, 0 , value-10 ) ) %>%
    group_by(hand) %>% 
    summarize(points = sum(value),
              .groups = 'drop') %>% 
    pivot_wider(names_from = hand, values_from = points)
}


dist_points_all_hands <- function() {
  shuffle_deck <- function() {
    bind_cols(card_deck, order = sample(1:52, 52, replace = F)) %>%
      arrange(order)
    
  }
  deal_cards <- function(deck) {
    bind_cols(deck, hand = rep(c('N', 'Ö', 'S', 'V') , 13))
    
  }
  bricka <-  deal_cards(shuffle_deck())
  
  points <-
    bricka %>%
    mutate(value = ifelse(value < 11, 0 , value-10 ) ) %>%
    group_by(hand) %>%
    summarize(points = sum(value),
              .groups = 'drop') %>%
    pivot_wider(names_from = hand, values_from = points)
  
  dist <- 
  bricka %>% 
    group_by(hand, suite) %>% 
    summarize(antal = n(),
              .groups = 'drop') %>% 
    pivot_wider(names_from = suite, values_from = antal, values_fill = 0) %>%
    select(-hand) %>%
    mutate(fördeln = paste0(.$clubs, .$dimonds, .$hearts, .$spades)) %>%
    select(fördeln)
  
  return(list(dist = dist, points = points))
  
}

dist_points_all_hands()

simulate_brickor <- function(antal) {
  x <- list(list(dist = NA, points = NA))
  for (i in 1:antal) {
    res  <- dist_points_all_hands()
    x[[i]] <- res
  }
  x
}

load_card_deck()

strSort <- function(x) sapply(lapply(strsplit(x, NULL), sort, decreasing = TRUE), paste, collapse="")

x <- simulate_brickor(10000)



# Likelihood of points on hand...  redo

tibble(points = unlist(lapply( x, function( i) i$points ))) %>%
  group_by(points) %>%
  summarize(probability = n()/nrow(.)*100) %>%
  ggplot(aes(x=points, y = probability)) +
  geom_col() +
  geom_text(aes(
    label = round(probability, digits = 1),
    vjust = -0.25)
  )




even <- c("4333", "5332", "4432")

tibble(förd  = unlist(lapply(x, function(i) i$dist))) %>%
  mutate(sort_förd = strSort(förd)) %>%
  group_by(sort_förd) %>%
  summarize(antal = n()) %>%
  mutate(pct = antal / sum(antal) * 100) %>%
  ggplot(aes (x = reorder(sort_förd,-pct), y = pct)) +
  geom_col() +
  geom_text(aes(
    label = round(pct, digits = 1),
    vjust = -0.25)
    )
