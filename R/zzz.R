#' @title zzz
#'
#' @description Runs on loading bridger
#'
#' @param libname Legacy dummy
#' @param pkgname Legacy dummy
#'
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import ggplot2
#' @import patchwork
#' @importFrom grDevices cairo_pdf
#' @importFrom magrittr %>%

.onLoad <- function(libname, pkgname) {
  # Set global variables for non-standard evaluation - Prevents warnings when compiling
  # utils::globalVariables(c(
  #   "compliant_hands", "%>%", ".", "C", "D", "H", "HC", "S", "Shape", "aes", "any_vars", "arrange", "arrow",
  #   "as_tibble", "bind_cols", "card", "case_when", "filter", "filter_all", "geom_rect", "geom_segment",
  #   "geom_text", "getSeat", "ggplot", "ggsave", "group_by", "hand1", "hand2", "hand3", "hand4",
  #   "mutate", "na.omit", "name", "parse_number", "pivot_longer", "pivot_wider", "rowid",
  #   "rowid_to_column", "rowwise", "runif", "select", "separate", "shape", "suit", "summarise",
  #   "theme", "theme_void", "tibble", "ungroup", "unit", "unite", "value", "plot_annotation", "plot_layout",
  #   "printHand_1", "printHand_2", "printHand_3", "printHand_4", "printHand_5", "printHand_6", "handNo", "points",
  #   "LTC", "Total", "Probability"
  # ))
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\n        --- bridger --- \n For all your bridge hand needs\n")
}
