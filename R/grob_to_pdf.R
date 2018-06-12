#' Converts a single grob to a pdf, or combines multiple grobs into a multiple page PDF document. Saves to the working directory. Only takes grobs with heights and widths in units of mm.
#'
#' @param ... The single grob or series of grobs which will be converted to a PDF document.
#' @param file_name The desired file name of the resulting PDF document in character format.
#' @return A PDF document of the grob(s) which will be saved to the working directory.
#' @export

grob_to_pdf <- function(..., file_name = character()){

  mm_to_in <- function(x) x/25.4

  stopifnot(length(file_name) == 1)
  file_name <- gsub('.pdf', '', file_name)

  grob_list <- list(...)

  grob_units <- gsub(as.numeric(grob_list[[1]]$heights[1]), '', grob_list[[1]]$heights[1])
  stopifnot(grob_units == 'mm')

  grDevices::pdf(
    file = paste0(file_name, '.pdf'),
    onefile = TRUE,
    height = mm_to_in(sum(as.numeric(grob_list[[1]]$heights))),
    width = mm_to_in(sum(as.numeric(grob_list[[1]]$widths))))

  for(gll in grob_list){
    if(!grid::is.grob(gll)){
      closed <- grDevices::dev.off()
      stop('All inputs must be grobs.')
    }
    gridExtra::grid.arrange(gll)
  }

  closed <- grDevices::dev.off()

}
