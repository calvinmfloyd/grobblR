#' Converts a single grob to a pdf, or combines multiple grobs into a multiple page PDF document. Saves to the working directory. Only takes grobs with heights and widths in units of mm.
#'
#' @param ... The single grob or series of grobs which will be converted to a PDF document.
#' @param file_name The desired file name of the resulting PDF document in character format.
#' @return A PDF document of the grob(s) which will be saved to the working directory.
#' @export

grob_to_pdf <- function(..., file_name = character(), add_page_numbers = F){

  mm_to_in <- function(x) x/25.4

  if(length(file_name) != 1) stop(
    "file_name in grob_to_pdf() must be a single character value.",
    call. = F)

  if(!is.logical(add_page_numbers)) if(length(add_page_numbers) != 1) stop(
    "add_page_numbers in grob_to_pdf() must a single logical value.",
    call. = F)

  grob_list <- unlist(list(...))
  if(!all(unlist(lapply(grob_list, class)) %in% c('R6', 'grob_layout'))) stop(
    "Only objects outputted by the grob_layout() function are permitted in grob_to_pdf().",
    call. = FALSE)

  file_name <- gsub('.pdf', '', file_name)

  if(add_page_numbers){
    for(i in 1:length(grob_list)){
      grob_list[[i]]$page_number <- i
    }
  }

  grDevices::pdf(
    file = paste0(file_name, '.pdf'),
    height = mm_to_in(grob_list[[1]]$height),
    width = mm_to_in(grob_list[[1]]$width))

  for(g in grob_list) gridExtra::grid.arrange(g$grob)

  closed <- grDevices::dev.off()

}
