#' Converts a single grob to a pdf, or combines multiple grobs into a multiple page PDF document. Saves to the working directory. Uses the returned lists form grob_layout(), in order to properly size each page of the PDF document.
#'
#' @param ... The single result from grob_layout() or series of results from grob_layout() which will be converted to a PDF document.
#' @param file_name The desired file name of the resulting PDF document in character format.
#' @return A PDF document of the grob(s) which will be saved to the working directory.
#' @export

grob_to_pdf <- function(..., file_name = character()){

  mm_to_in <- function(x) x/25.4

  stopifnot(length(file_name) == 1)
  file_name <- gsub('.pdf', '', file_name)

  grob_layout_lists <- list(...)

  grDevices::pdf(
    file = paste0(file_name, '.pdf'),
    onefile = TRUE,
    height = mm_to_in(as.numeric(grob_layout_lists[[1]]$total_height)),
    width = mm_to_in(as.numeric(grob_layout_lists[[1]]$total_width)))

  for(gll in grob_layout_lists) grid.arrange(gll$grob)

  closed <- grDevices::dev.off()

}
