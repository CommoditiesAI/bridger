#' @title collectHands
#'
#' @description Returns a list of hands that fit a requirement.  Simple hands will most often give the required bids.
#'   Complex hands, where a subsequent bid is made, may not fit the requirements, as other bids by opponents or partner
#'   may be preferable to the desired bidding pattern.
#'
#' @return List of hand IDs and compass point of the hand making the bid
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
#'   \tab SIMPLE \tab HC_low \tab HC_high \tab cardLen_low \tab cardLen_high \cr
#'   \tab any \tab 12 \tab 19 \tab 0 \tab 10 \cr
#'   \tab 1major \tab 12 \tab 19 \tab 4 (Major) \tab 10 \cr
#'   \tab NT \tab 12 \tab 14 \tab 2 \tab 4 \cr
#'   \tab 4441 \tab 12 \tab 14 \tab 1 \tab 4 \cr
#'   \tab strong \tab 19  \tab 30 \tab 0 \tab 8 \cr
#'   \tab preempt2 \tab 5 \tab 10 \tab 0 \tab 6 \cr
#'   \tab preempt3 \tab 6 \tab 9 \tab 0 \tab 7 \cr
#'   \tab COMPLEX \tab HC_low \tab HC_high \tab cardLen_low \tab cardLen_high \cr
#'   \tab 1NT_LHOdouble \tab \tab \tab \tab \cr
#'   \tab 1NT_LHObid \tab \tab \tab \tab \cr
#'   \tab 1NT_RHObid \tab \tab \tab \tab \cr
#'   \tab 1major_jacoby2NT \tab canape \tab \tab \tab \cr
#'  }
#'
#' Other parameters are also used, but individually assigned in the function.
#'
#' @examples
#' # Collect the ids of 2 hands with any shape
#' hands <- collectHands(num = 2)
#'
#' \dontrun{
#' # Collect a hand with opening points and a "4441" shape
#'     hands <- collectHands(handType = "4441", num = 1)
#'
#' # Collect a NT trump hand, with a point range of 11 to 15
#'     hands <- collectHands(handType = "weakNT", num = 1, HC_low = 11, HC_high = 15)
#' }
#'
#' @export

collectHands <- function(handType = "any", num = 6, ...) {

  # Identify and pass through other parameters, e.g. card length or high-card points
  args <- as.list(list(...))

  findParams <- list()
  if ("HC_low" %in% names(args)) findParam <- c(findParams, as.numeric(args["HC_low"]))
  if ("HC_high" %in% names(args)) findParam <- c(findParams, as.numeric(args["HC_high"]))
  if ("cardLen_low" %in% names(args)) findParam <- c(findParams, as.numeric(args["cardLen_low"]))
  if ("cardLen_high" %in% names(args)) findParam <- c(findParams, as.numeric(args["cardLen_high"]))
  if ("pointsForDouble" %in% names(args)) findParam <- c(findParams, as.numeric(args["pointsForDouble"]))
  if ("canape" %in% names(args)) findParam <- c(findParams, as.numeric(args["canape"]))

  # Generate holding tibble
  returnHands <- tibble(id = numeric(num), hand = character(num), type = handType)

  # Which type of hand is waned
  findCall <- case_when(
  # Simple hand types
    handType %in% c("any") ~ "find_any",
    handType %in% c("weakNT", "weaknt", "weakbalanced") ~ "find_weakNT",
    handType %in% c("1major", "major1") ~ "find_1major",
    handType %in% c("strongNT", "strongnt", "strongbalanced") ~ "find_strongNT",
    handType %in% c("1444", "4144", "4414", "4441") ~ "find_4441",
    handType %in% c("strong", "2level", "strong2") ~ "find_strong",
    handType %in% c("2preempt", "weak2", "preempt2") ~ "find_2preempt",
    handType %in% c("3preempt", "weak3", "preempt3") ~ "find_3preempt",

  # Composite hand types
    handType %in% c("weak1NTdouble", "weak1NTx", "double-after-NT", "1NT_LHOdouble") ~ "find_weak1NT_LHOx",
    handType %in% c("weak1NTbid", "bid-after-NT", "1NT_LHObid") ~ "find_weak1NT_LHObid",
    handType %in% c("weak1NTRHObid", "RHObid-after-NT", "1NT_RHObid") ~ "find_weak1NT_RHObid",
    handType %in% c("jacoby2NT") ~ "find_1major_jacoby2NT",

  # Unknown hand type
    handType == TRUE ~ "unknown"
  )

  if(findCall == "unknown") {
    stop(glue::glue("Unknown handtype - '{handType}' - given"))
  }

  # Search for requested type of hand with any given parameters
  for (i in 1:num) {
    repeat{
      result <- do.call(findCall, findParams)
      if (result$id != FALSE) {
        break
      }
    }

    returnHands[i, 1:2] <- result
  }

  invisible(returnHands)
}
