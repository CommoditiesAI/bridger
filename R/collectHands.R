#' @title collectHands
#'
#' @description Return a list of hands that fit a requirement
#'
#' @return List of hand IDs
#'
#' @param handType Type of hands wanted
#' @param num Number of hands requested
#' @param ... Other parameters to be passed to the find_ functions, e.g. HC_low, cardLen_low
#'
#' @note
#' Each of the handTypes is a standard set of parameters.  For example "NT" (alias "balanced") allows 12-14 points, a single doubleton
#' and no 5-card majors and no 6-card minor.  To change these parameters then optional parameters can be passed through the "...".
#' The most common will be to specify the low and high high-card range and the shortest allowed suit and longest allowed.  These are
#' "HC_low" and "HC_high", "cardLen_low" and "cardLen_high" respectively.
#'
#' Existing functions and key parameters are given.
#' \tabular{rccccc}{
#'   \tab Type \tab HC_low \tab HC_high \tab cardLen_low \tab cardLen_high \cr
#'   \tab NT \tab 12 \tab 14 \tab 2 \tab 4 \cr
#'   \tab 4441 \tab 12 \tab 14 \tab 1 \tab 4 \cr
#'   \tab strong \tab 19  \tab 30 \tab 0 \tab 8 \cr
#'   \tab preempt2 \tab 5 \tab 10 \tab 0 \tab 6 \cr
#'   \tab preempt3 \tab 6 \tab 9 \tab 0 \tab 7 \cr
#'  }
#'
#' Other parameters are also used, but individually assigned in the function.
#'
#' More complex arrangements may require additional functions
#'
#' @examples
#' # Collect the ids of 2 hands with any shape
#' hands <- collectHands(num = 2)
#'
#' # Collect the id of a hand with opening points and a "4441" shape
#' hands <- collectHands(handType = "4441", num = 1)
#'
#' # Collect the id of a NT trump hand, with a point range of 11 to 15
#' hands <- collectHands(handType = "weakNT", num = 1, HC_low = 11, HC_high = 15)
#' @export

collectHands <- function(handType = "any", num = 6, ...) {

  # Identify and pass through other parameters, e.g. card length, otherwise need long lest of "find_" functions
  args <- as.list(list(...))

  findParam <- list()
  if ("HC_low" %in% names(args)) findParam <- c(findParam, as.numeric(args["HC_low"]))
  if ("HC_high" %in% names(args)) findParam <- c(findParam, as.numeric(args["HC_high"]))
  if ("cardLen_low" %in% names(args)) findParam <- c(findParam, as.numeric(args["cardLen_low"]))
  if ("cardLen_high" %in% names(args)) findParam <- c(findParam, as.numeric(args["cardLen_high"]))

  # Generate holding tibble
  returnHands <- tibble(id = numeric(num), hand = character(num), type = handType)

  # Which type of hand is waned
  findCall <- dplyr::case_when(
    handType %in% c("weakNT", "weaknt", "weakbalanced") ~ "find_weakNT",
    handType %in% c("strongNT", "strongnt", "strongbalanced") ~ "find_strongNT",
    handType %in% c("1444", "4144", "4414", "4441") ~ "find_4441",
    handType %in% c("strong", "2level", "strong2") ~ "find_strong",
    handType %in% c("2preempt", "weak2", "preempt2") ~ "find_2preempt",
    handType %in% c("3preempt", "weak3", "preempt3") ~ "find_3preempt",
    handType %in% c("any") ~ "find_any",
    handType == TRUE ~ "unknown"
  )

  if (findCall == "unknown") {
    message("Unknown handtype given")
    stop("")
  }

  # Search for requested type of hand
  for (i in 1:num) {
    repeat{
      result <- do.call(findCall, findParam)
      if (result$id != FALSE) {
        break
      }
    }

    returnHands[i, 1:2] <- result
  }

  invisible(returnHands)
}
