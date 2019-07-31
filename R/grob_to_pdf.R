#' Converts a single grob-layout to a PDF, or combines multiple grob-layouts into a multiple page PDF document.
#'
#' @param ... The single grob or series of grobs which will be converted to a PDF document.
#' @param file_name The desired file name of the resulting PDF document in character format.
#' @param add_page_numbers If TRUE, page numbers will be added to the bottom right corners of the pages of the document, based on the order of the grob-layouts listed.
#' @param meta_data_title Title string to embed as the /Title field in the file. If not provided, it will default to the \code{file_name} provided.
#' @return A PDF document of the grob-layout(s) which will be saved to the working directory.
#' @details In the case of multiple page documents, the dimensions of the overall document will be determined by the dimensions of the first grob-layout listed.
#' @export

grob_to_pdf = function(...,
                       file_name = character(),
                       add_page_numbers = FALSE,
                       meta_data_title = character()) {

  grob_list = unlist(list(...))
  if (length(file_name) != 1) {
    stop("file_name in grob_to_pdf() must be a single character value.", call. = FALSE)
    }
  if (!is.logical(add_page_numbers)) if(length(add_page_numbers) != 1) {
    stop("add_page_numbers in grob_to_pdf() must a single logical value.", call. = FALSE)
  }
  if (!all(unlist(lapply(grob_list, class)) %in% c('R6', 'grob_layout'))) {
    stop("Only objects outputted by the grob_layout() function are permitted in grob_to_pdf().", call. = FALSE)
  }

  file_name = gsub('.pdf', '', file_name)

  meta_data_title = ifelse(
    length(meta_data_title) == 0,
    paste0(file_name, '.pdf'),
    as.character(meta_data_title)
  )
  
  if (add_page_numbers) {
    for(i in 1:length(grob_list)){
      grob_list[[i]]$page_number = i
    }
  }

  grDevices::pdf(
    file = paste0(file_name, '.pdf'),
    height = units_convert(x = grob_list[[1]]$height, from_units = grob_list[[1]]$units, to_units = 'inches'),
    width = units_convert(x = grob_list[[1]]$width, from_units = grob_list[[1]]$units, to_units = 'inches'),
    title = meta_data_title
    )

  for (g in grob_list) gridExtra::grid.arrange(g$grob)

  closed = grDevices::dev.off()

}
