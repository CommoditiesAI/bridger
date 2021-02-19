#' @title printHands
#'
#' @description Produce a graphic of hands on a single page, useful as a hand-out.  Each page can hold upto 6 hands.
#'
#' @param handType The type of hand required, default is 'any'.  Alternatives include, '4441', 'strong', ...
#' @param num The number of hands wanted - Make divisible by 6
#' @param ... Other variables that may be passed when selecting compliant hands
#'
#' @examples
#' # Produce 2 pages of 6 hands of bridge hands
#' printHands()
#'
#' # Produce a page of 6 hands likely to open with a 3-level preempt
#' printHands(num = 6, handType = "preempt3")
#' @export

printHands <- function(handType = "any", num = 12, ...) {

  # Extract parameters passed in ... for use in modifying default handTypes
  #  inArgs <- as.list(list(...))

  # Adjust number so divisible by 6, alternatives would be to adjust creation statement, to accommodate blanks
  # if(num < 6) {
  #   print("Number of requested hands set to minimum of 6")
  #   num <- 6
  # } else if(num %% 6 != 0) {
  #   num <- 6 * floor(num / 6)
  #   print(glue::glue("Number of requested hands not divisible by 6 - reduced to {num}"))
  # } else {
  #   num <- num # Not needed
  # }

  # Collect set of hands fitting ID
  handIDs <- collectHands(handType = handType, num = num, ...)

  # Use IDs to generate compliant hands
  hands <- mapply(bridgeHand, handIDs$id[1:num], seat = handIDs$hand[1:num]) %>%
    rbind(type = handIDs$type)

  # Use handType for a better title
  handTypeName <- dplyr::case_when(
    handType == "any" ~ "an unspecified opening bid",
    handType %in% c("weakNT", "weakbalanced", "weaknt") ~ "a weak, balanced shape",
    handType %in% c("strongNT", "strongbalanced", "strongnt") ~ "a strong balanced shape",
    handType %in% c("strong", "strong2", "2level") ~ "a strong or 2-level opening bid",
    handType %in% c("2preempt", "preempt2", "weak2") ~ "a weak 2-level bid",
    handType %in% c("3preempt", "preempt3", "weak3") ~ "a weak 3-level bid",
    handType %in% c("1444", "4144", "4414", "4441") ~ "a '4441' shape",
    # Add any more, with aliases, here
    handType == TRUE ~ handType
  )

  # Build list of plots, to allow less than 6
  # Could use custom makeUP with Null if a graphic is missing
  # https://patchwork.data-imaginist.com/articles/guides/layout.html#moving-beyond-the-grid

  # How many complete groups/pages of 6 hands
  chunks <- split(1:num, ceiling(seq_along(1:num) / 6))

  # Extract the individual hands and save to temporary "printHand_"
  for (i in 0:(length(chunks) - 1)) {
    for (j in 1:6) {
      if (ncol(hands) >= j + i * 6) {
        assign(glue::glue("printHand_{j}"), hands[, j + i * 6]$graphic)
      } else {
        assign(glue::glue("printHand_{j}"), grid::textGrob("--- | ---"))
      }
    }

    # Might get better alignments by having a blank grob of the right size

    # Build graphic
    hand_plot <- printHand_1 + printHand_2 + printHand_3 + printHand_4 + printHand_5 + printHand_6 +
      patchwork::plot_layout(ncol = 2) +
      patchwork::plot_annotation(
        title = glue::glue("Bridge hands generated to open with {handTypeName}"),
        subtitle = glue::glue("Page: {i+1}"), caption = glue::glue("Page: {i+1}"),
        theme = theme(plot.title = element_text(colour = "darkblue", size = 14, face = "bold"),
                      plot.subtitle = element_text(colour = "darkblue", size = 10, face = "italic"))
      ) +
      ggplot2::theme(plot.margin = ggplot2::unit(c(2, 2, 2, 2), "pt")) &
      ggplot2::theme(
        plot.tag = ggplot2::element_text(size = 9, colour = "black"),
        panel.background = ggplot2::element_rect(colour = "lightgrey", size = 0.5, fill = NA)
      )

    # Tag each hand
    hand_plot <- hand_plot +
      patchwork::plot_annotation(tag_levels = "1", tag_prefix = "Hand ", tag_suffix = ":") &
      theme(plot.tag.position = c(0.001, 0.95),
            plot.tag = element_text(size = 8, hjust = 0, vjust = 0, colour = "darkblue"))

    # Save to temp location
      page_location <- tempfile(pattern = "", fileext = ".pdf")

    # Compile list of saved pages
    if(!exists("page_list")) {
      page_list <- page_location
    } else {
      page_list <- c(page_list, page_location)
    }

    # Save individual pages to temporary location
      ggplot2::ggsave(filename = page_location, plot = hand_plot,
                      device = cairo_pdf, width = 210 * 0.8, height = 297 * 0.8, units = "mm", dpi = 200
      )
    }

    # Combine temporary pages into final output
      pdftools::pdf_combine(page_list, glue::glue("{Sys.getenv('HOME')}/bridger/bridgeHands_combined.pdf"))

    # Remove temp pages
      unlink(page_list)

  return(glue::glue("{length(chunks)} pages of hands saved to {dir}",
    dir = glue::glue("{Sys.getenv('HOME')}/bridger/bridgeHands_combined.png")))
}
