#' The main \code{grobblR} function which contains and organizes \code{\link{grob_col}}'s and \code{\link{grob_row}}'s, giving the overall grob-layout its shape.
#'
#' @param ... The combination of grob-rows and grob-columns which will help give the main grob-layout outputted its shape and look.
#' @param height The numeric height of the grob-layout in the \code{units} supplied. Default is 280 mm - which is the height of an upright 8.5 x 11 inches piece of copy paper.
#' @param width The numeric width of the grob in the \code{units} supplied. Default is 216 mm - which is the width of an upright 8.5 x 11 inches piece of copy paper.
#' @param units The units of the given height and width for the grob. Options are 'mm', 'cm' or 'inches'. Default is 'mm'.
#' @param padding_p The proportion of the minimum of the height and width which will be used for the padding around the edge of the grob-layout. Overridden by any numeric value provided in the \code{padding} parameter.
#' @param padding The numeric amount of padding around the edge of the grob-layout in the \code{units} supplied.
#' @param title A character string which will be displayed as the title of the grob-layout.
#' @param title_aes_list The list outputted by \code{\link{ga_list}}, which controls aesthetics of the title of the grob-layout.
#' @param title_p The numeric proportion of height within the grob-layout and its allotted space which will be used by the title grob. A numeric value between 0 and 0.25.
#' @param caption A character string which will be displayed as the caption at the bottom of the grob-layout.
#' @param caption_aes_list The list outputted by \code{\link{ga_list}}, which controls aesthetics of the caption of the grob-layout.
#' @param caption_p The numeric proportion of height within the grob-layout and its allotted space which will be used by the caption grob. A numeric value between 0 and 0.25.
#' @param page_number A single value that can be converted to an integer for the page number in the bottom right of the grob-layout within the padding. If it cannot be converted to an integer, the page number will not appear.
#' @return An R6 class object containing all information necessary to create the overall grob-layout. The grob itself is called with \code{grob_layout()$grob}.
#' @examples
#' gl = grob_layout(
#'   grob_row(grob_col(1), grob_col(2)),
#'   grob_row(grob_col(3))
#'   )
#' # to retrieve the grob-layout
#' gl$grob
#' @export

grob_layout = function(...,
                       height = 280,
                       width = 216,
                       units = c('mm', 'cm', 'inches'),
                       title = '',
                       title_aes_list = ga_list(),
                       title_p = 0.1,
                       caption = '',
                       caption_aes_list = ga_list(),
                       caption_p = 0.05,
                       padding_p = 0.05,
                       padding = NA_real_,
                       page_number = '') {

  units = match.arg(units)
  
  grob_layout_class$new(
    contents = unlist(list(...)),
    height = height,
    width = width,
    padding = padding,
    padding_proportion = padding_p,
    page_number = page_number,
    units = units,
    title = title,
    title_p = title_p,
    title_aes_list = title_aes_list,
    caption = caption,
    caption_p = caption_p,
    caption_aes_list = caption_aes_list

    )

}
