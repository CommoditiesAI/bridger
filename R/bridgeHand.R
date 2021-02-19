#' @title bridgeHand
#'
#' @description Generate a bridge hand
#'
#' @note
#' To change the scoring of hands pass high card values (HCValues) and hand shape values (shapeValues) in the arguments.
#'
#' HCValues is a string of five digits specifying the value of the Ace, King, Queen, Jack and 10.  The default is , 3, 2, 1, 0.
#' shapeValues is a string of eight digits specifying the value of a suit with no cards/"Void", 1-card/"Singleton", ... 7-cards.  The default is c(3, 2, 1, 0, 0, 1, 2, 3)
#'
#' @return List: Hand ID, Dealer, Hand graphic, Hand points, Hand shape
#'
#' @param handNumber An integer for generating a hand, or "auto" to use a random number generator
#' @param seat If not false, makes the specified seat South and dealer, so all bidding starts with South and the specified hand type
#' @param createGraphic Whether the graphic should be created
#' @param ... Other parameters passed to shape the hands
#'
#' @examples
#' # Produce a bridge hand
#' hand <- bridgeHand()
#'
#' # Produce a bridge hand 500 ensuring South as dealer
#' hand500 <- bridgeHand(handNumber = "500", seat = "S") # Seat can be any compass-point
#'
#' # Produce a bridge hand, with South as dealer, but without generating the graphic
#' handIDonly <- bridgeHand(seat = "W", createGraphic = FALSE)
#' @export

bridgeHand <- function(handNumber = "auto", seat = FALSE, createGraphic = TRUE, ...) {

  # Parse out the optional parameters
  args <- as.list(list(...))

  if ("HCValues" %in% names(args)) {
    HCValues <- args$HCValues
    stopifnot(length(HCValues) == 5, all(is.numeric(HCValues)))
  } else {
    HCValues <- c(4, 3, 2, 1, 0)
  }

  if ("shapeValues" %in% names(args)) {
    shapeValues <- args$HCValues
    stopifnot(length(shapeValues) == 8, all(is.numeric(shapeValues)))
  } else {
    shapeValues <- c(3, 2, 1, 0, 0, 1, 2, 3)
  }

  # Constants ----
  suits <- c("S", "H", "D", "C")
  compassPoints <- c("N", "E", "S", "W")

  # Set seed - Use given seed or choose a random one ----
  if (handNumber != "auto") {
    if (length(handNumber) > 1) {
      lapply(handNumber, bridgeHand)
    }
    if (!is.double(handNumber)) {
      if (!is.double(readr::parse_number(handNumber))) {
        print("Only numeric seeds allowed")
        stop()
      } else {
        handNo <- readr::parse_number(handNumber)
      }
    } else {
      handNo <- handNumber
    }
  } else {
    handNo <- round(runif(1) * 10000000, 0)
  }

  set.seed(handNo)

  # Set dealer and vulnerability
  vuln <- c("None", "NS", "EW", "Both")[sample(1:4, 1)]

  # If a specified seat is given, then always set dealer to that seat
  if (seat != FALSE) {
    dealer <- "S"
  } else {
    dealer <- compassPoints[handNo %% 4 + 1]
  }

  # Create pack and shuffle ----
  # Create pack
  pack <- expand.grid(rank = c("A", 2:9, "T", "J", "Q", "K"), suit = suits) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(card = paste(suit, rank, sep = "-"))

  # Divide cards into hands
  for (i in 1:4) {
    temp <- sample(pack$card, 13, replace = FALSE) %>%
      tibble::as_tibble() %>%
      tidyr::separate(value, sep = "-", into = c("suit", "rank")) %>%
      dplyr::mutate(
        suit = factor(suit, levels = c("S", "H", "D", "C")),
        rank = factor(rank, levels = c("A", "K", "Q", "J", "T", 9:2, " "))
      ) %>%
      dplyr::arrange(suit, rank) %>%
      tidyr::unite("card", sep = "-")

    colnames(temp) <- compassPoints[i]

    assign(glue::glue("hand{i}"), temp)

    pack <- pack %>%
      dplyr::filter(!card %in% unname(unlist(temp)))
  }

  # Assemble pack
  pack <- hand1 %>%
    cbind(hand2) %>%
    cbind(hand3) %>%
    cbind(hand4)

  # Extract a hand at a time
  for (j in compassPoints) {
    temp_hand <- pack[j] %>%
      dplyr::bind_cols(order = 1:13, .) %>%
      tidyr::separate(!!j, sep = "-", into = c("suit", "rank")) %>%
      dplyr::mutate(suit = factor(suit, levels = c("S", "H", "D", "C")), rank = factor(rank, levels = c("A", "K", "Q", "J", "T", 9:2, " ", "10"))) %>%
      dplyr::arrange(suit, rank) %>%
      tidyr::pivot_wider(names_from = "suit", values_from = "rank") %>%
      dplyr::select(-order)

    # Add back suits, if missing
    if (all(!colnames(temp_hand) %in% "S")) {
      temp_hand <- cbind(temp_hand, S = c("Void", rep(NA, 12)))
    }

    if (all(!colnames(temp_hand) %in% "H")) {
      temp_hand <- cbind(temp_hand, H = c("Void", rep(NA, 12)))
    }

    if (all(!colnames(temp_hand) %in% "D")) {
      temp_hand <- cbind(temp_hand, D = c("Void", rep(NA, 12)))
    }

    if (all(!colnames(temp_hand) %in% "C")) {
      temp_hand <- cbind(temp_hand, C = c("Void", rep(NA, 12)))
    }

    temp_hand <- temp_hand %>%
      dplyr::select(S, H, D, C)


    # Remove the NAs and then put them back in at the end
    for (i in suits) {
      temp_suit <- na.omit(temp_hand[i])

      while (nrow(temp_suit) < 13) {
        temp_suit <- rbind(temp_suit, NA)
      }

      temp_hand[i] <- temp_suit
    }

    temp_hand <- temp_hand %>%
      dplyr::filter_all(dplyr::any_vars(!is.na(.)))

    temp_hand <- temp_hand %>%
      replace(is.na(.), " ")

    assign(glue::glue("hand{j}"), temp_hand)
  }

  # Rearrange hands so that South is always the dealer
  if (seat != FALSE) {
    otherHands <- setdiff(compassPoints, seat)
    hand_temp_S <- get(glue::glue("hand{seat}"))
    hand_temp_W <- get(glue::glue("hand{otherHands[1]}"))
    hand_temp_N <- get(glue::glue("hand{otherHands[2]}"))
    hand_temp_E <- get(glue::glue("hand{otherHands[3]}"))

    handS <- hand_temp_S
    handW <- hand_temp_W
    handN <- hand_temp_N
    handE <- hand_temp_E
  }

  # Assess hands ----
  # Identify points
  names(HCValues) <- c("A", "K", "Q", "J", "10")

  points <- tibble::tibble(Hand = compassPoints, HC = 0L, Shape = 0L)

  for (i in compassPoints) {
    temp <- get(glue::glue("hand{i}")) %>%
      tibble::rowid_to_column() %>%
      tidyr::pivot_longer(-rowid) %>%
      dplyr::filter(value != " ") %>%
      dplyr::select(value) %>%
      table() %>%
      tibble::as_tibble()

    points[points$Hand == i, "HC"] <- round(sum(
      unname(unlist(temp[temp$. == "A", "n"])) * HCValues[["A"]],
      unname(unlist(temp[temp$. == "K", "n"])) * HCValues[["K"]],
      unname(unlist(temp[temp$. == "Q", "n"])) * HCValues[["Q"]],
      unname(unlist(temp[temp$. == "J", "n"])) * HCValues[["J"]],
      unname(unlist(temp[temp$. == "T", "n"])) * HCValues[["10"]]
    ), 0)
  }

  # Identify voids, singletons and long suits for points
  for (i in compassPoints) {
    hand_shape <- get(glue::glue("hand{i}")) %>%
      tibble::rowid_to_column() %>%
      tidyr::pivot_longer(-rowid) %>%
      dplyr::filter(value != " ") %>%
      dplyr::group_by(name) %>%
      dplyr::summarise(shape = max(rowid), .groups = "drop") %>%
      dplyr::ungroup() %>%
      dplyr::select(shape) %>%
      unname() %>%
      unlist()

    temp_points <-
      sum(shapeValues[2] * (hand_shape == 1)) +
      sum(shapeValues[3] * (hand_shape == 2)) +
      sum(shapeValues[4] * (hand_shape == 3)) +
      sum(shapeValues[5] * (hand_shape == 4)) +
      sum(shapeValues[6] * (hand_shape == 5)) +
      sum(shapeValues[7] * (hand_shape == 6)) +
      sum(shapeValues[8] * (hand_shape == 7))

    # Look for Void and add 2 points (3 - 1 for counted singleton)
    temp_points <- temp_points + sum(get(glue::glue("hand{i}"))[1, ] == "Void") * shapeValues[1]

    points[points$Hand == i, "Shape"] <- temp_points
  }

  # Calculate total points
  points <- points %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Total = sum(HC + Shape))

  # Create the graphic object ----
  if (createGraphic) {
    hand_graphic <- createGraphic(handNo, handN, handE, handS, handW, dealer, vuln, points)
  } else {
    hand_graphic <- "Not requested"
  }

  # Collate the hand shapes
  handShapes <- tibble::tibble(
    N = colSums(handN != " "),
    E = colSums(handE != " "),
    S = colSums(handS != " "),
    W = colSums(handW != " ")
  )

  # Return
  invisible(list(
    id = handNo, # ID
    dealer = dealer, # Dealer
    graphic = hand_graphic, # Hand graphic
    handPoints = points[, c("Hand", "HC")], # Hand points
    handShapes = handShapes
  )) # Hand shapes
}
