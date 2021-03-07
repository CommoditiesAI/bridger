#' @title suitSplit
#'
#' @description The probabilities of how a number of cards  will split between two hands, given a number of unknown cards in each hand.  Unknown hands
#'   are assumed to be West and East.
#'
#' @note If the unknown cards in both hands are the same, then symmetrical probabilities will be returned.
#'
#' @param missingCards The number of cards held by the two hands
#' @param cards_W Cards in West hands, e.g. If bidding indicates another 5 card suit, for example by overcalling, then reduce by cards_W by 5
#' @param cards_E Cards in East hands
#'
#' @return Tibble of probabilities
#'
#' @examples
#' suitSplit(missingCards = 6, cards_W = 13, cards_E = 8)
#'
#' @export

suitSplit <- function(missingCards = 5, cards_W = 13, cards_E = 13) {

  ifelse(cards_W != cards_E, symmetrical <- FALSE, symmetrical <- TRUE)

  outTable <- tibble::tibble("Cards held by West" = 0:missingCards, "Cards held by East" = missingCards:0, Probability = 0)

  for(i in outTable$"Cards held by West") {
    unknown_W = i
    unknown_E = missingCards-i

    temp <- factorial(unknown_W + unknown_E)/(factorial(unknown_W)*factorial(unknown_E)) *
      (factorial(cards_W)*factorial(cards_E) * factorial(cards_W + cards_E - unknown_W - unknown_E)) /
      (factorial(cards_W + cards_E)*factorial(cards_W-unknown_W)*factorial(cards_E-unknown_E)) *
      ifelse(symmetrical, (2-abs(unknown_W == unknown_E)), 1) # Adding this and the slice function for symmetrical hands

    outTable[i+1, "Probability"] <- round(temp,2)

  }

  if(symmetrical) {
    outTable <- dplyr::slice(outTable, 0:ceiling(nrow(outTable)/2))
    colnames(outTable) <- c("Cards in one hand", "Cards in other hand", "Probability")
  }

  xaxis <- outTable[,1] %>%
    unname() %>%
    unlist()

  graph <- ggplot(outTable) +
    geom_col(aes(x = xaxis, y = Probability, fill = factor(xaxis+1))) +
    geom_label(aes(x = xaxis, y = Probability, label = scales::percent(Probability),
                   vjust = ifelse(Probability > 0.1, 2, -0.8)), fill = "white", fontface = "bold", label.size = NA) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = xaxis) +
    scale_fill_manual(values = c("#c72e29", "#016392", "#be9c2e", "#098154", "#fb832d", "red", "steelblue",
                                 "purple", "darkred", "lightblue", "darkgrey", "darkgreen", "lightgrey")) +
    labs(title = glue::glue("Probable distribution of {missingCards} cards between two hands"),
         subtitle = glue::glue("{sym}",
                             sym = ifelse(symmetrical, 'Symmetrical probabilities',
                                          'Asymmetrical probabilities reflecting {cards_W} unknown cards in West and {cards_E} unknown in East')),
         x = colnames(outTable[1]), y = NULL) +
    guides(fill = FALSE) +
    theme_minimal() +
    theme(title = element_text(size = rel(1.2)),
          axis.text = element_text(size = rel(1)))

  plot(graph)

  outTable$Probability <- scales::percent(outTable$Probability, accuracy = 0.1)

  return(outTable)
}
