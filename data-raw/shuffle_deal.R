library(readxl)
library(tidyverse)
card_deck <- read_excel("card_deck.xlsx")
card_prio <- read_excel("card_deck.xlsx",
                        sheet = "suite_prio")



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
    pivot_wider(names_from = suite, values_from = antal) %>%
    select(-hand) %>%
    mutate(fördeln = paste0(.$clubs, .$dimonds, .$hearts, .$spades)) %>%
    select(fördeln)
}

points_alla_händer <- function(bricka){
  bricka %>% 
  filter(value > 10) %>%
  group_by(hand) %>% 
  summarize(points = sum(value-10),
            .groups = 'drop') %>% 
  pivot_wider(names_from = hand, values_from = points)
}

bricka <- deal_cards(shuffle_deck()) 
bricka




fördelning_alla_händer(bricka)
points_alla_händer(bricka)
list(fördelning = fördelning_alla_händer(bricka), poäng = points_alla_händer(bricka))

for (i in 1:10){
  bricka <- deal_cards(shuffle_deck()) 
  print(list(fördelning = fördelning_alla_händer(bricka), poäng = points_alla_händer(bricka)))
}

bricka %>% 
  group_by(hand, suite) %>% 
  summarize(antal = n(),
            .groups = 'drop') %>%
  pivot_wider(names_from = suite, values_from = antal) %>%
  select(-hand) %>%
  mutate(fördeln = paste0(.$clubs, .$dimonds, .$hearts, .$spades)) %>%
  select(fördeln)
