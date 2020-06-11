
#' Grob Row
#' 
#' The grob-row function which helps gives the grob from the
#' \code{\link{grob_layout}} function its shape. Encompasses \code{\link{grob_col}}
#' within the overall grob-layout.
#'
#' @param ... A series of \code{\link{grob_col}}'s.
#' 
#' @param p The numeric proportion of the given height which should be given
#' to sub-grobs outputted in the grob-row. Defaults to 1.
#' 
#' Overridden if a \code{height} is supplied.
#' 
#' @param height The numeric height of the grob-row in millimeters.
#' 
#' Overrides the \code{p} parameter. 
#' 
#' @param border A TRUE/FALSE argument corresponding to whether or not a
#' border around the outputted grob-row is desired.
#' 
#' Defaults to FALSE.
#' 
#' @param border_aes_list The list outputted by \code{\link{ga_list}}, which
#' controls aesthetics of the borders.
#' 
#' Ignored if \code{border} is set to FALSE.
#' 
#' @param title A character string which will be displayed as the title of the grob-row.
#' 
#' @param title_aes_list The list outputted by \code{\link{ga_list}}, which
#' controls aesthetics of the title of the grob-row.
#' 
#' @param title_height The numeric height in mm within the grob_column which will
#' be used by the title grob. Will override \code{title_p} if provided.
#' 
#' @param title_p The numeric proportion of height within the grob-row which
#' will be used by the title grob.
#' 
#' @param caption A character string which will be displayed as the caption of the grob-row.
#' 
#' @param caption_aes_list The list outputted by \code{\link{ga_list}}, which
#' controls aesthetics of the caption of the grob-row.
#' 
#' @param caption_height The numeric height in mm within the grob_column which will
#' be used by the caption grob. Will override \code{caption_p} if provided.
#' 
#' @param caption_p The numeric proportion of height within the grob-row which will be used by the caption grob.
#' 
#' 
#' @param padding_p The proportion of the minimum of the height and width which
#' will be used for the padding around the edge of the grob-row.
#' 
#' Overridden by any numeric value provided in the \code{padding} parameter.
#' 
#' @param padding The numeric amount of padding around the edge of the grob-row
#' in millimeters.
#' 
#' Overrides the \code{padding_p} parameter.
#' 
#' @return An R6 class object which contains all the information needed to
#' carry on to its grob-columns and create the grob-row.
#' 
#' @export
#' 
#' @examples 
#' 
#' grob_row(
#'   grob_col(1, border = TRUE),
#'   grob_col(2, border = TRUE)
#'   ) %>%
#'   view_grob(100, 100)
#' 

grob_row = function(...,
                    p = 1,
                    height = NA_real_,
                    border = FALSE,
                    border_aes_list = ga_list(),
                    title = '',
                    title_aes_list = ga_list(),
                    title_p = 0.15,
                    title_height = NA_real_,
                    caption = '',
                    caption_aes_list = ga_list(),
                    caption_p = 0.15,
                    caption_height =  NA_real_,
                    padding_p = 0.05,
                    padding = NA_real_) {

  if (!isTRUE(is.numeric(p) & p > 0)) {
    
    stop("p in grob_row() must be a non-zero positive numeric value.", call. = FALSE)
    
  }
  
  grob_row_class$new(
    contents = unlist(list(...)),
    proportion = p,
    height = height,
    padding = padding,
    padding_proportion = padding_p,
    border = border,
    border_aes_list = border_aes_list,
    title = title,
    title_p = title_p,
    title_aes_list = title_aes_list,
    title_height = title_height,
    caption = caption,
    caption_p = caption_p,
    caption_aes_list = caption_aes_list,
    caption_height = caption_height
    )

}
