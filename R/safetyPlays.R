# safety_plays

#' @description Show all combinations of all remaining cards to assess safety plays.
#'   Next stage would be to automatically assessment to achieve required number of tricks.
#'
#' @param cards_n The cards North holds
#' @param cards_s The cards South holds
#' @param smallCards Cards to be considered small and equivalent, default = 9

safety_plays <- function (cards_n = c("AKTxx"), cards_s = c("xxxx"), smallCards = 9) {

# Suit
  suit <- c("A","K", "Q", "J", "T", 9:1) %>%
    factor(ordered = TRUE, levels = c("A","K", "Q", "J", "T", 9:1))


# Replace "x" with numbers from 1 up
  hold = 1

  for(i in 1:nchar(cards_n)) {
    if(str_sub(cards_n, i,i) == "x") {
      str_sub(cards_n, i,i) <- hold
      hold <- hold + 1
    }
  }

  for(i in 1:nchar(cards_s)) {
    if(str_sub(cards_s, i,i) == "x") {
      str_sub(cards_s, i,i) <- hold
      hold <- hold + 1
    }
  }

  # Split into vectors
  n <- str_split(cards_n, pattern = "")  %>%
    unlist() %>%
    factor(levels = rev(suit), ordered = TRUE)

  s <- str_split(cards_s, pattern = "")  %>%
    unlist() %>%
    factor(levels = rev(suit), ordered = TRUE)

  # Build East and West hands
    ew <- setdiff(suit, c(strsplit(cards_n,"")[[1]],strsplit(cards_s,"")[[1]]))
    #  factor(levels = c("A","K", "Q", "J", "T", 9:1, "x"), ordered = TRUE)

    ew <- c(ew, rep("-", length(ew))) %>%
      factor(levels = c("A","K", "Q", "J", "T", 9:1, "-"), ordered = TRUE)

  # Build tibble of East and West hands
    # Generate all combinations
    cards_w <- suppressMessages(
        combinat::combn(x = ew, m = length(ew)/2, simplify = TRUE) %>%
        t() %>%
        as_tibble(.name_repair = "unique") %>%
        distinct()
    )

    colnames(cards_w) <- paste0("W_", 1:ncol(cards_w))

    cards_e <- cards_w %>%
      rowid_to_column() %>%
      arrange(rev(rowid)) %>%
      select(-rowid)

    colnames(cards_e) <- paste0("E_", 1:ncol(cards_e))

    unknown <- cbind(cards_w, cards_e) # Unknown cards

    colnames(unknown) <- c(paste0("W_", 1:ncol(cards_e)), paste0("E_", 1:ncol(cards_e)))

# Simplify by making all cards below a level equivalent
    for(i in smallCards:1) {
      unknown <- unknown %>%
        mutate_all(~ (str_replace(., as.character(i), "x")))
    }

    unknown <- distinct(unknown)

    known <- suppressMessages(
        paste0(cards_n, cards_s) %>%
        str_split(pattern = "")  %>%
        unlist() %>%
        as.matrix() %>%
        t() %>%
        as_tibble(.name_repair = "unique") %>%
        slice(rep(1, nrow(unknown)))
    )

    colnames(known) <- c(paste0("N_", 1:nchar(cards_n)), paste0("S_", 1:nchar(cards_s)))

    for(i in smallCards:1) {
      known <- known %>%
        mutate_all(~(str_replace(., as.character(i), "x")))
    }


  # Combine known and unknown
    safetyPlayOut <- cbind(known, unknown)

  return(safetyPlayOut)
}

temp <- safety_plays()
temp
