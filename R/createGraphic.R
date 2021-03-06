#' @title createGraphic
#'
#' @description Create the graphic of the hand
#'
#' @return ggplot graphic object
#'
#' @param handNo The id of the hand
#' @param handN The North hand generated by bridgeHand
#' @param handE The East hand generated by bridgeHand
#' @param handS The South hand generated by bridgeHand
#' @param handW The West hand generated by bridgeHand
#' @param dealer The hand to become South, the designated dealer
#' @param vuln The hand's vulnerability
#' @param points The hand's points

createGraphic <- function(handNo, handN, handE, handS, handW, dealer, vuln, points) {

  # Themes
  handTheme <- gridExtra::ttheme_minimal(
    core = list(
      bg_params = list(fill = c("grey85", "white", "grey85", "white"), col = NA),
      fg_params = list(fontface = 3L)
    ),
    base_size = 8,
    colhead = list(fg_params = list(col = "darkblue", fontface = "bold"), hjust = 0, x = 0),
    rowhead = list(fg_params = list(col = c("black", "red", "red", "black"), fontface = "bold"), hjust = 0, x = 0),
    padding = unit(c(2, 2), "mm"),
    parse = TRUE
  )

  boxTheme <- gridExtra::ttheme_minimal(
    core = list(
      bg_params = list(fill = c("grey85", "white", "grey85", "white"), col = NA),
      fg_params = list(fontface = 3L)
    ),
    base_size = 6,
    colhead = list(fg_params = list(col = "darkblue", fontface = "bold"), hjust = 0, x = 0),
    rowhead = list(fg_params = list(col = c("black", "red", "red", "black"), fontface = "bold"), hjust = 0, x = 0),
    padding = unit(c(2, 2), "mm"),
    parse = TRUE
  )

  dealerPos <- case_when(
    dealer == "N" ~ c(dealerX = 0, dealerY = 1),
    dealer == "E" ~ c(dealerX = 1, dealerY = 0),
    dealer == "S" ~ c(dealerX = 0, dealerY = -1),
    dealer == "W" ~ c(dealerX = -1, dealerY = 0)
  )

  header <- tibble(
    "Hand ID" = sub("\\s+$", "", gsub("(.{3})", "\\1 ", handNo)),
    "Dealer" = dealer,
    "Vuln." = vuln
  )

  header <- as_tibble(cbind(" " = names(header), t(header)), .name_repair = "minimal") %>%
    rename("   " = 2)

  # Create central compass rose
  compass <- ggplot() +
    geom_rect(aes(xmin = -0.3, xmax = 0.3, ymin = -0.3, ymax = 0.3), fill = "white") +
    geom_point(aes(x = dealerPos[1] * 0.25, y = dealerPos[2] * 0.25),
      size = 6, pch = 21, stroke = 1.5,
      colour = "steelblue", fill = "white", alpha = 0.8
    ) +
    geom_segment(aes(x = 0, xend = 0, y = -0.18, yend = 0.18),
      arrow = arrow(length = unit(0.03, "npc"), ends = "both"),
      lwd = 0.6, colour = ifelse(vuln %in% c("NS", "Both"), "red", "black")
    ) +
    geom_segment(aes(x = -0.18, xend = 0.18, y = 0, yend = 0),
      arrow = arrow(length = unit(0.03, "npc"), ends = "both"),
      lwd = 0.6, colour = ifelse(vuln %in% c("EW", "Both"), "red", "black")
    ) +
    geom_text(aes(x = 0, y = 0.25, label = "N"), size = 4.5, colour = ifelse(vuln %in% c("NS", "Both"), "red", "black")) +
    geom_text(aes(x = 0.25, y = 0, label = "E"), size = 4.5, colour = ifelse(vuln %in% c("EW", "Both"), "red", "black")) +
    geom_text(aes(x = 0, y = -0.25, label = "S"), size = 4.5, colour = ifelse(vuln %in% c("NS", "Both"), "red", "black")) +
    geom_text(aes(x = -0.25, y = 0, label = "W"), size = 4.5, colour = ifelse(vuln %in% c("EW", "Both"), "red", "black")) +
    theme_void() +
    theme(aspect.ratio = 1)

  # Change column names into suit shapes
  colnames(handN) <- c("\u2660", "\u2665", "\u2666", "\u2663")
  colnames(handE) <- c("\u2660", "\u2665", "\u2666", "\u2663")
  colnames(handS) <- c("\u2660", "\u2665", "\u2666", "\u2663")
  colnames(handW) <- c("\u2660", "\u2665", "\u2666", "\u2663")

  # Change "T"s into "10"
  handN[handN == "T"] <- "10"
  handE[handE == "T"] <- "10"
  handS[handS == "T"] <- "10"
  handW[handW == "T"] <- "10"

  # Build grobs ----
  # TODO Awaiting grob conversion of GT objections for neater table
  grob_header <- gridExtra::tableGrob(header, rows = NULL, theme = boxTheme)
  grob_points <- points %>%
    select(1:4) %>%
    gridExtra::tableGrob(rows = NULL, theme = boxTheme)
  if (is.na(points[1, 5])) {
    grob_ltc <- NULL
  } else {
    grob_ltc <- points %>%
      select(c(1, 5)) %>%
      gridExtra::tableGrob(rows = NULL, theme = boxTheme)
  }

  grob_N <- gridExtra::tableGrob(t(handN), cols = NULL, theme = handTheme)
  grob_E <- gridExtra::tableGrob(t(handE), cols = NULL, theme = handTheme)
  grob_S <- gridExtra::tableGrob(t(handS), cols = NULL, theme = handTheme)
  grob_W <- gridExtra::tableGrob(t(handW), cols = NULL, theme = handTheme)

  # Highlight face cards
  faceCards <- c("A", "K", "Q", "J")

  for (i in faceCards) {
    cards <- which(t(handN) == i)
    if (length(cards) > 0) {
      for (j in cards) {
        grob_N$grobs[j + 8][[1]][["gp"]] <- grid::gpar(fontsize = 9, fontface = "bold", col = "darkblue")
      }
    }

    cards <- which(t(handE) == i)
    if (length(cards) > 0) {
      for (j in cards) {
        grob_E$grobs[j + 8][[1]][["gp"]] <- grid::gpar(fontsize = 9, fontface = "bold", col = "darkblue")
      }
    }

    cards <- which(t(handS) == i)
    if (length(cards) > 0) {
      for (j in cards) {
        grob_S$grobs[j + 8][[1]][["gp"]] <- grid::gpar(fontsize = 9, fontface = "bold", col = "darkblue")
      }
    }

    cards <- which(t(handW) == i)
    if (length(cards) > 0) {
      for (j in cards) {
        grob_W$grobs[j + 8][[1]][["gp"]] <- grid::gpar(fontsize = 9, fontface = "bold", col = "darkblue")
      }
    }
  }

  hand_graphic <- cowplot::plot_grid(
    grob_header, grob_N, NULL,
    grob_W, compass, grob_E,
    grob_points, grob_S, grob_ltc
  )

  # TODO Change from cowplot to patchwork to remove dependency
  # Manual manipulation of grobs in printHands alluded me
  # hand_graphic <- wrap_elements(panel = grob_header) +
  #   grob_N + plot_spacer() +
  #   grob_W + compass + grob_E +
  #   grob_points + grob_S + grob_ltc

  invisible(hand_graphic)
}
