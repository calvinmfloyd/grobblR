#' The grob-row function which helps gives the grob from the \code{\link{grob_layout()}} function its shape. Works hand-in-hand with the \code{\link{grob_col()}} function.
#'
#' @param ... A series of \code{grob_col}'s.
#' @param p The numeric proportion of the given height which should be given to sub-grobs outputted in the grob-row. Defaults to 1. Overridden if a \code{height} is supplied.
#' @param border A TRUE/FALSE argument corresponding to whether or not a border around the outputted grob-row is desired. Defaults to FALSE.
#' @param border_sides Controls the borders around the total grob-row. The input is a string with the possible words "top", "bottom", "left", "right" separated by ", ". For example, "top, left, right" will put borders on the top, left and right side of the grid cell, but not the bottom. Default is "top, bottom, left, right", or all borders.
#' @param border_aes_list The list outputted by \code{\link{ga_list}}, which controls aesthetics of the borders. Only two aesthetics that can be tweaked for borders are \code{border_color}, \code{border_width} and \code{border_sides}. Ignored if \code{border} is set to FALSE.
#' @param title A character string which will be displayed as the title of the grob-row.
#' @param title_aes_list The list outputted by \code{\link{ga_list}}, which controls aesthetics of the title of the grob-row.
#' @param title_p The numeric proportion of height within the grob-row which will be used by the title grob. A numeric value between 0 and 0.25.
#' @param caption A character string which will be displayed as the caption of the grob-row.
#' @param caption_aes_list The list outputted by \code{\link{ga_list}}, which controls aesthetics of the caption of the grob-row.
#' @param caption_p The numeric proportion of height within the grob-row which will be used by the caption grob. A numeric value between 0 and 0.25.
#' @param padding_p The proportion of the minimum of the height and width which will be used for the padding around the edge of the grob-row. Overridden by any numeric value provided in the \code{padding} parameter.
#' @param padding The numeric amount of padding around the edge of the grob-row in the \code{units} supplied by the grob-layout. Overrides the \code{padding_p} parameter.
#' @param height The numeric height of the grob-row in the \code{units} supplied by the grob-layout. Overrides the \code{p} parameter. 
#' @return An R6 class object which contains all the information needed to carry on to its grob-columns and create the grob-row.
#' @details The individual grob-row is obtained with \code{grob_row()$grob}.
#' @export

grob_row = function(...,
                    p = 1,
                    border = F,
                    border_aes_list = ga_list(),
                    title = '',
                    title_aes_list = ga_list(),
                    title_p = 0.1,
                    caption = '',
                    caption_aes_list = ga_list(),
                    caption_p = 0.05,
                    padding_p = 0.05,
                    padding = NA_real_,
                    height = NA_real_) {

  if (!is.numeric(p)) if (p < 0) stop('p in grob_row() must be a positive numeric value.', call. = FALSE)
  
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
    caption = caption,
    caption_p = caption_p,
    caption_aes_list = caption_aes_list
    )
}
