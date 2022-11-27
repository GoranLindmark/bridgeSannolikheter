create_deck()

shuffle_deck(create_deck())
deal_cards(shuffle_deck(create_deck()))
handType1(deal_cards(shuffle_deck(create_deck())))



simhandTibble <- handType(simHand())
for (i in 1:1000) {
  x <-  handType(simHand())
  if (ncol(x) == 4){
    simhandTibble <- rbind(simhandTibble,x)
  }
}

simhandTibble <- filterRealisticHandTypes(simhandTibble)
