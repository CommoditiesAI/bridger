#' @title printHands
#'
#' @description Produce a graphic of hands as a hand-out.  Each page can hold up to 6 hands.
#'
#'   Pages of all the hands, or only the south or north hands can be selected.  In output the presence of "a" will
#'   produce pages with all hands, while "n", "e", "s" or "w" will include only the specified hands.  "all" is equivalent
#'   to "anews".
#'
#'   The output will be a PDF for the selected hand sets.  These will be saved to the c:/bridger/ directory, if saveOutput
#'   is set to TRUE, or to temporary, notified files if set to false.
#'
#' @param ids The ids of hands to be generated
#' @param seats The seats of the hands in ids, i.e. the seat which gives the requested conditions
#' @param handType The type of hand required, default is 'any'.  Alternatives include, '4441', 'strong', ...
#' @param num The number of hands wanted
#' @param output Character code to indicates required outputs, "N/"S" for the North or South hands, "a" for all hands
#' @param saveOutput If true (default) saves PDF to the c:/temp/bridger directory, otherwise produces only a temp file
#' @param ... Other variables that may be passed when selecting compliant hands
#'
#' @examples
#' # Produce 1 hand showing only one seat
#' printHands(handType = "any", num = 1, output = "news")
#'
#' # Produce a page of 6 South hands likely to open with a 3-level preempt
#' printHands(handType = "preempt3", num = 6, output = "s")
#'
#' # Produce specified hands
#' printHands(ids = c(500, 501, 502), seats = c("E", "W", "S"))
#'
#' @export

printHands <- function(ids = FALSE, seats = FALSE, handType = "any", num = 12, output = "a", saveOutput = TRUE, ...) {

  # Check output complies
    output <- tolower(output)
    if(output == "all") {
      output <- "aswne"
    }

  # Check bridger directory exists, if requested to save there
  if(saveOutput == TRUE & !dir.exists("c:/temp/bridger/")) {
    print("Creating directory c:/temp/bridger/ to save output")
    dir.create("c:/temp/bridger/", recursive = TRUE)
  }

# Build graphic function
  buildGraphic <- function(i, handLabel, pHand_1, pHand_2, pHand_3, pHand_4, pHand_5, pHand_6) {
    hand_plot <- pHand_1 + pHand_2 + pHand_3 + pHand_4 + pHand_5 + pHand_6 +
      patchwork::plot_layout(ncol = 2) +
      patchwork::plot_annotation(
        title = glue::glue("Bridge hands generated to open with {handTypeName}"),
        subtitle = glue::glue("Page: {i+1} - {handLabel}"), caption = glue::glue("Page: {i+1}"),
        tag_levels = "1", tag_prefix = "Hand ", tag_suffix = ":", ##
        theme = theme(plot.title = element_text(colour = "darkblue", size = 14, face = "bold"),
                      plot.subtitle = element_text(colour = "darkblue", size = 10, face = "italic"))
      ) +
      ggplot2::theme(plot.margin = ggplot2::unit(c(2, 2, 2, 2), "pt")) &
      ggplot2::theme(
        plot.tag.position = c(0.001, 0.95), ##
        plot.tag = element_text(size = 8, hjust = 0, vjust = 0, colour = "darkblue"), ##
        panel.background = ggplot2::element_rect(colour = "lightgrey", size = 0.5, fill = NA)
      )

    # Save to temp location
    page_location <- tempfile(pattern = "", fileext = ".pdf")

    # Save individual pages to temporary location
    ggplot2::ggsave(filename = page_location, plot = hand_plot,
                    device = cairo_pdf, width = 210 * 0.8, height = 297 * 0.8, units = "mm", dpi = 200)

    invisible(page_location)
  }

  # Use handType to form the title
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

  # Change num to the length of ids/seats and build handIDs, if given
  if(ids[1] != FALSE) {
    stopifnot(length(ids) == length(seats) | length(seats) == 1)
    num <- length(ids)
    handIDs <- tibble::tibble(id = ids, hand = seats, type = "handType")
  } else {
  # Or build set of hands fitting ID
    handIDs <- collectHands(handType = handType, num = num, ...)
  }


  # Use IDs to generate graphics ----
  hands <- mapply(bridgeHand, handIDs$id[1:num], seat = handIDs$hand[1:num]) %>%
    rbind(type = handIDs$type)

  # How many complete groups/pages of 6 hands
  chunks <- split(1:num, ceiling(seq_along(1:num) / 6))

  # If all hands requested ----
  if(grepl(pattern = "a", x = output)) {

    # Extract the individual hands and save to temporary "printHand_"
    for (i in 0:(length(chunks) - 1)) {
      for (j in 1:6) {
        if (ncol(hands) >= j + i * 6) {
          assign(glue::glue("printHand_{j}"), hands[, j + i * 6]$graphic)
        } else {
          assign(glue::glue("printHand_{j}"), grid::textGrob("--- | ---"))
        }
      }

      # Build graphic
        page_location <- buildGraphic(i, "All hands", printHand_1, printHand_2, printHand_3, printHand_4, printHand_5, printHand_6)

      # Compile list of saved pages
      if(!exists("page_list")) {
        page_list <- page_location
      } else {
        page_list <- c(page_list, page_location)
      }
    }

    # Combine temporary pages into final output
    if(saveOutput) {
      pdftools::pdf_combine(page_list, glue::glue("c:/temp/bridger/bridgeHands_combined_all.pdf"))
    } else {
      combined_location <- tempfile(pattern = "bridger_all_", fileext = ".pdf")
      pdftools::pdf_combine(page_list, combined_location)
      message(glue::glue("Hands saved to temporary file: {combined_location}"))
    }

    # Clean up temp pages
    unlink(page_list)
    rm(page_list)
  }

  # If South requested ----
  if(grepl(pattern = "s", x = output)) {
    adjustTable <- c(13, 14, 16, 17, 18, 20, 21, 22, 24)

    # How many complete groups/pages of 6 hands
    chunks <- split(1:num, ceiling(seq_along(1:num) / 6))

    # Remove hands and points not to be shown
      for(i in 1:num) {
        # Reduce font of entries from points table to zero
        for(j in adjustTable) {
          hands[, i]$graphic$layers[[6]][["geom_params"]][["grob"]]$grobs[[j]]$label <- " "
        }

        # Remove extra hands
        hands[, i]$graphic <- hands[, i]$graphic %>%
          ggedit::remove_geom("grob", 5) %>% # Remove East
          ggedit::remove_geom("grob", 3) %>% # Remove West
          ggedit::remove_geom("grob", 2) # Remove North
      }

    # Extract the individual hands and save to temporary "printHand_"
    for (i in 0:(length(chunks) - 1)) {
      for (j in 1:6) {
        if (ncol(hands) >= j + i * 6) {
          assign(glue::glue("printHand_{j}"), hands[, j + i * 6]$graphic)
        } else {
          assign(glue::glue("printHand_{j}"), grid::textGrob("--- | ---"))
        }
      }

      # Build graphic
      page_location <- buildGraphic(i, "South hands", printHand_1, printHand_2, printHand_3, printHand_4, printHand_5, printHand_6)

      # Compile list of saved pages
      if(!exists("page_list")) {
        page_list <- page_location
      } else {
        page_list <- c(page_list, page_location)
      }
    }

    # Combine temporary pages into final output
    if(saveOutput) {
      pdftools::pdf_combine(page_list, glue::glue("c:/temp/bridger/bridgeHands_combined_south.pdf"))
    } else {
      combined_location <- tempfile(pattern = "south_", fileext = ".pdf")
      pdftools::pdf_combine(page_list, combined_location)
      message(glue::glue("South hands saved to - {combined_location}"))
    }

    # Remove temp pages
    unlink(page_list)
    rm(page_list)
  }

  # If West requested ----
  if(grepl(pattern = "w", x = output)) {

    # Use IDs to generate compliant hands - Need to recompile as manipulation for South removed elements
    hands <- mapply(bridgeHand, handIDs$id[1:num], seat = handIDs$hand[1:num]) %>%
      rbind(type = handIDs$type)

    adjustTable <- c(13, 14, 15, 17, 18, 19, 21, 22, 23)

    # How many complete groups/pages of 6 hands
    chunks <- split(1:num, ceiling(seq_along(1:num)/6))

    # Remove hands and points not to be shown
    for(i in 1:num) {
      # Reduce font of entries from points table to zero
      for(j in adjustTable) {
        hands[, i]$graphic$layers[[6]][["geom_params"]][["grob"]]$grobs[[j]]$label <- " "
      }

      # Remove extra hands
      hands[, i]$graphic <- hands[, i]$graphic %>%
        ggedit::remove_geom("grob", 7) %>% # Remove South
        ggedit::remove_geom("grob", 5) %>% # Remove East
        ggedit::remove_geom("grob", 2) # Remove North
    }

    # Extract the individual hands and save to temporary "printHand_"
    for (i in 0:(length(chunks) - 1)) {
      for (j in 1:6) {
        if (ncol(hands) >= j + i * 6) {
          assign(glue::glue("printHand_{j}"), hands[, j + i * 6]$graphic)
        } else {
          assign(glue::glue("printHand_{j}"), grid::textGrob("--- | ---"))
        }
      }

      # Build graphic
      page_location <- buildGraphic(i, "West hands", printHand_1, printHand_2, printHand_3, printHand_4, printHand_5, printHand_6)

      # Compile list of saved pages
      if(!exists("page_list")) {
        page_list <- page_location
      } else {
        page_list <- c(page_list, page_location)
      }
    }

    # Combine temporary pages into final output
    if(saveOutput) {
      pdftools::pdf_combine(page_list, glue::glue("c:/temp/bridger/bridgeHands_combined_west.pdf"))
    } else {
      combined_location <- tempfile(pattern = "bridger_west_", fileext = ".pdf")
      pdftools::pdf_combine(page_list, combined_location)
      message(glue::glue("All hands saved to - {combined_location}"))
    }

    # Remove temp pages
    unlink(page_list)
    rm(page_list)
  }

  # If North requested ----
  if(grepl(pattern = "n", x = output)) {

    # Use IDs to generate compliant hands - Need to recompile as manipulation for South removed elements
    hands <- mapply(bridgeHand, handIDs$id[1:num], seat = handIDs$hand[1:num]) %>%
      rbind(type = handIDs$type)

    adjustTable <- c(14, 15, 16, 18, 19, 20, 22, 23, 24)

    # How many complete groups/pages of 6 hands
    chunks <- split(1:num, ceiling(seq_along(1:num)/6))

    # Remove hands and points not to be shown
    for(i in 1:num) {
      # Reduce font of entries from points table to zero
      for(j in adjustTable) {
        hands[, i]$graphic$layers[[6]][["geom_params"]][["grob"]]$grobs[[j]]$label <- " "
      }

      # Remove extra hands
      hands[, i]$graphic <- hands[, i]$graphic %>%
        ggedit::remove_geom("grob", 7) %>% # Remove South
        ggedit::remove_geom("grob", 5) %>% # Remove East
        ggedit::remove_geom("grob", 3) # Remove West
    }

    # Extract the individual hands and save to temporary "printHand_"
    for (i in 0:(length(chunks) - 1)) {
      for (j in 1:6) {
        if (ncol(hands) >= j + i * 6) {
          assign(glue::glue("printHand_{j}"), hands[, j + i * 6]$graphic)
        } else {
          assign(glue::glue("printHand_{j}"), grid::textGrob("--- | ---"))
        }
      }

      # Build graphic
      page_location <- buildGraphic(i, "North hands", printHand_1, printHand_2, printHand_3, printHand_4, printHand_5, printHand_6)

      # Compile list of saved pages
      if(!exists("page_list")) {
        page_list <- page_location
      } else {
        page_list <- c(page_list, page_location)
      }
    }

    # Combine temporary pages into final output
    if(saveOutput) {
      pdftools::pdf_combine(page_list, glue::glue("c:/temp/bridger/bridgeHands_combined_north.pdf"))
    } else {
      combined_location <- tempfile(pattern = "bridger_north_", fileext = ".pdf")
      pdftools::pdf_combine(page_list, combined_location)
      message(glue::glue("All hands saved to - {combined_location}"))
    }

    # Remove temp pages
    unlink(page_list)
    rm(page_list)
  }

  # If Easts requested ----
  if(grepl(pattern = "e", x = tolower(output))) {

    # Use IDs to generate compliant hands - Need to recompile as manipulation for South removed elements
    hands <- mapply(bridgeHand, handIDs$id[1:num], seat = handIDs$hand[1:num]) %>%
      rbind(type = handIDs$type)

    adjustTable <- c(13, 15, 16, 17, 19, 20, 21, 23, 24)

    # How many complete groups/pages of 6 hands
    chunks <- split(1:num, ceiling(seq_along(1:num)/6))

    # Remove hands and points not to be shown
    for(i in 1:num) {
      # Reduce font of entries from points table to zero
      for(j in adjustTable) {
        hands[, i]$graphic$layers[[6]][["geom_params"]][["grob"]]$grobs[[j]]$label <- " "
      }

      # Remove extra hands
      hands[, i]$graphic <- hands[, i]$graphic %>%
        ggedit::remove_geom("grob", 7) %>% # Remove South
        ggedit::remove_geom("grob", 3) %>% # Remove West
        ggedit::remove_geom("grob", 2) # Remove North
    }

    # Extract the individual hands and save to temporary "printHand_"
    for (i in 0:(length(chunks) - 1)) {
      for (j in 1:6) {
        if (ncol(hands) >= j + i * 6) {
          assign(glue::glue("printHand_{j}"), hands[, j + i * 6]$graphic)
        } else {
          assign(glue::glue("printHand_{j}"), grid::textGrob("--- | ---"))
        }
      }

      # Build graphic
      page_location <- buildGraphic(i, "East hands", printHand_1, printHand_2, printHand_3, printHand_4, printHand_5, printHand_6)

      # Compile list of saved pages
      if(!exists("page_list")) {
        page_list <- page_location
      } else {
        page_list <- c(page_list, page_location)
      }
    }

    # Combine temporary pages into final output
    if(saveOutput) {
      pdftools::pdf_combine(page_list, glue::glue("c:/temp/bridger/bridgeHands_combined_east.pdf"))
    } else {
      combined_location <- tempfile(pattern = "bridger_east_", fileext = ".pdf")
      pdftools::pdf_combine(page_list, combined_location)
      message(glue::glue("All hands saved to - {combined_location}"))
    }

    # Remove temp pages
    unlink(page_list)
    rm(page_list)
  }

  return(glue::glue("{nchar(output)} sets of {length(chunks)} {pages} of hands saved to {outputDir}",
                    pages = ifelse(length(chunks) == 1, "page", "pages"),
                    outputDir = ifelse(saveOutput, "c:/temp/bridger/", "temporary files")))
}
