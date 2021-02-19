#' @title find_any
#'
#' @description Return any bridge hand - Default
#'
#' @return FALSE if not compliant, or id and seat of compliant hand
#'
#' @param HC_low The minimum number of high-card points

find_any <- function(HC_low = 12) {
  testHand <- bridgeHand(createGraphic = FALSE)

  for (seat in 1:4) {
    HC <- testHand$handPoints[seat, 2]

    # Test hand for conditions
    result <- dplyr::case_when(
      HC < HC_low ~ FALSE, # Test for points
      TRUE ~ TRUE
    )

    # Return the id and seat
    if (result) {
      return(invisible(list(id = testHand$id, seat = c("N", "E", "S", "W")[seat])))
    }
  }

  invisible(list(id = FALSE, seat = FALSE))
}

#' @title find_weakNT
#'
#' @description Find hands that comply with a no trump opening
#'
#' @return FALSE if not compliant, or id and seat of compliant hand
#'
#' @param HC_low The minimum number of high-card points
#' @param HC_high The maximum number of high-card points
#' @param cardLen_low The minimum length of a suit
#' @param cardLen_high The maximum length of a suit

find_weakNT <- function(HC_low = 12, HC_high = 14, cardLen_low = 2, cardLen_high = 4) {
  testHand <- bridgeHand(createGraphic = FALSE)

  for (seat in 1:4) {
    HC <- testHand$handPoints[seat, 2]
    shape <- testHand$handShapes[, seat]

    # Test hand for conditions
    result <- dplyr::case_when(
      HC < HC_low ~ FALSE, # Test for points
      HC > HC_high ~ FALSE, # Test for points
      any(shape < cardLen_low) ~ FALSE, # Test for length
      any(shape[1:2, ] > cardLen_high) ~ FALSE, # Test for length - majors
      any(shape[3:4, ] > cardLen_high + 1) ~ FALSE, # Test for length - minors (allow cardLen_high+1)
      sum(shape == cardLen_low) > 1 ~ FALSE, # Test for only 1 two-card suit
      TRUE ~ TRUE
    )

    # Could test for 5-card major if < 5 points in suit

    # Return the id and seat
    if (result) {
      return(invisible(list(id = testHand$id, seat = c("N", "E", "S", "W")[seat])))
    }
  }

  invisible(list(id = FALSE, seat = FALSE))
}

#' @title find_strongNT
#'
#' @description Find hands that comply with a weak no trump opening
#'
#' @return FALSE if not compliant, or id and seat of compliant hand
#'
#' @param HC_low The minimum number of high-card points
#' @param HC_high The maximum number of high-card points
#' @param cardLen_low The minimum length of a suit
#' @param cardLen_high The maximum length of a suit

find_strongNT <- function(HC_low = 15, HC_high = 17, cardLen_low = 2, cardLen_high = 5) {
  testHand <- bridgeHand(createGraphic = FALSE)

  for (seat in 1:4) {
    HC <- testHand$handPoints[seat, 2]
    shape <- testHand$handShapes[, seat]

    # Test hand for conditions
    result <- dplyr::case_when(
      HC < HC_low ~ FALSE, # Test for points
      HC > HC_high ~ FALSE, # Test for points
      any(shape < cardLen_low) ~ FALSE, # Test for length
      any(shape[1:2, ] > cardLen_high) ~ FALSE, # Test for length - majors
      any(shape[3:4, ] > cardLen_high) ~ FALSE, # Test for length - minors
      sum(shape == cardLen_low) > 1 ~ FALSE, # Test for only 1 two-card suit
      TRUE ~ TRUE
    )

    # Return the id and seat
    if (result) {
      return(invisible(list(id = testHand$id, seat = c("N", "E", "S", "W")[seat])))
    }
  }

  invisible(list(id = FALSE, seat = FALSE))
}

#' @title find_strong
#'
#' @description Find hands that are strong enough to open strong
#'
#' @return FALSE if not compliant, or id and seat of compliant hand
#'
#' @param HC_low The minimum number of high-card points
#' @param HC_high The maximum number of high-card points
#' @param cardLen_low The minimum length of a suit
#' @param cardLen_high The maximum length of a suit

find_strong <- function(HC_low = 19, HC_high = 35, cardLen_low = 1, cardLen_high = 5) {
  testHand <- bridgeHand(createGraphic = FALSE)

  for (seat in 1:4) {
    HC <- testHand$handPoints[seat, 2]
    shape <- testHand$handShapes[, seat]

    # Test hand for conditions
    result <- dplyr::case_when(
      HC < HC_low | HC > HC_high ~ FALSE, # Test for points
      any(shape <= cardLen_low) | any(shape >= cardLen_high) ~ FALSE, # Test for having a long or short suit
      TRUE ~ TRUE
    )

    # Return the id and seat
    if (result) {
      return(invisible(list(id = testHand$id, seat = c("N", "E", "S", "W")[seat])))
    }
  }

  invisible(list(id = FALSE, seat = FALSE))
}


#'  @title find_4441
#'
#' @description Find hands that comply with a 4441 shape and opening point count
#'
#' @return FALSE if not compliant, or id and seat of compliant hand
#'
#' @param HC_low The minimum number of high-card points
#' @param HC_high The maximum number of high-card points
#' @param cardLen_low The minimum length of a suit
#' @param cardLen_high The maximum length of a suit

find_4441 <- function(HC_low = 11, HC_high = 35, cardLen_low = 5, cardLen_high = 13) {
  testHand <- bridgeHand(createGraphic = FALSE)

  for (seat in 1:4) {
    HC <- testHand$handPoints[seat, 2]
    shape <- testHand$handShapes[, seat]

    # Test hand for conditions
    result <- dplyr::case_when(
      HC < HC_low | HC > HC_high ~ FALSE, # Test for points
      sum(shape == 4) != 3 ~ FALSE, # Test for length
      TRUE ~ TRUE
    )

    # Return the id and seat
    if (result) {
      return(invisible(list(id = testHand$id, seat = c("N", "E", "S", "W")[seat])))
    }
  }

  invisible(list(id = FALSE, seat = FALSE))
}

#' @title find_2preempt
#'
#' @description Find hands that are likely to preempt at the 2 level in a major
#'
#' @return FALSE if not compliant, or id and seat of compliant hand
#'
#' @param HC_low The minimum number of high-card points
#' @param HC_high The maximum number of high-card points
#' @param cardLen_low The minimum length of a suit
#' @param cardLen_high The maximum length of a suit

find_2preempt <- function(HC_low = 5, HC_high = 10, cardLen_low = 6, cardLen_high = 7) {
  testHand <- bridgeHand(createGraphic = FALSE)

  for (seat in 1:4) {
    HC <- testHand$handPoints[seat, 2]
    shape <- testHand$handShapes[, seat]

    # Test hand for conditions
    result <- dplyr::case_when(
      HC < HC_low | HC > HC_high ~ FALSE, # Test for points
      sum(shape[1:2, ] == cardLen_low) != 1 | sum(shape == cardLen_high) >= 1 ~ FALSE, # Test for only 1 two-card suit
      TRUE ~ TRUE
    )

    # Return the id and seat
    if (result) {
      return(invisible(list(id = testHand$id, seat = c("N", "E", "S", "W")[seat])))
    }
  }

  invisible(list(id = FALSE, seat = FALSE))
}

#' @title find_3preempt
#'
#' @description Find hands that are likely to preempt at the 3 level
#'
#' @return FALSE if not compliant, or id and seat of compliant hand
#'
#' @param HC_low The minimum number of high-card points
#' @param HC_high The maximum number of high-card points
#' @param cardLen_low The minimum length of a suit
#' @param cardLen_high The maximum length of a suit

find_3preempt <- function(HC_low = 5, HC_high = 10, cardLen_low = 7, cardLen_high = 8) {
  testHand <- bridgeHand(createGraphic = FALSE)

  for (seat in 1:4) {
    HC <- testHand$handPoints[seat, 2]
    shape <- testHand$handShapes[, seat]

    # Test hand for conditions
    result <- dplyr::case_when(
      HC < HC_low | HC > HC_high ~ FALSE, # Test for points
      sum(shape == cardLen_low) < 1 ~ FALSE, # Test for only 1 two-card suit
      TRUE ~ TRUE
    )

    # Return the id and seat
    if (result) {
      return(invisible(list(id = testHand$id, seat = c("N", "E", "S", "W")[seat])))
    }
  }

  invisible(list(id = FALSE, seat = FALSE))
}
