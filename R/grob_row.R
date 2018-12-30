#' The grob row function which helps gives the grob from the \code{grobblR::grob_layout()} function its shape. Works hand in hand with the \code{grobblR::grob_col()} function.
#'
#' @param ... A series of \code{grob_col}'s.
#' @param p The numeric proportion of the given height which should be given to sub-grob's outputted in this grob row. Defaults to 1.
#' @param border A TRUE/FALSE argument corresponding to whether or not a border around the outputted grob row is desired. Defaults to FALSE.
#' @param border_sides Controls the borders around the total grob row. The input is a string with the possible words "top", "bottom", "left", "right" separated by ", ". For example, "top, left, right" will put borders on the top, left and right side of the grid cell, but not the bottom. Default is "top, bottom, left, right", or all borders.
#' @param border_aes_list A list which contains desired aesthetics for the border around the outputted the grob row. Ignored if \code{border} is set to FALSE. Elements of list inputted directly into \code{grid::gpar()}.
#' @param title A character string which will be displayed as the title of the grob row.
#' @param title_aes_list A list which contains desired aesthetics for the title of the grob row. Elements of this list are treated the same way as \code{aes_list} - see \code{\link{grob_matrix}} for more details on its possible elements.
#' @param title_p The numeric proportion of height within the grob row and its allotted space which will be used by the title grob. A numeric value between 0 and 1.
#' @return An R6 class which contains all the information needed to carry on to its grob columns and create the grob row.
#' @export

grob_row <- function(...,
                     p = 1,
                     border = F,
                     border_sides = 'top, bottom, left, right',
                     border_aes_list = ga_list(),
                     title = '',
                     title_aes_list = ga_list(),
                     title_p = 0.1){

  if(!is.numeric(p)) if(p < 0) stop(
    'p in grob_row() must be a positive numeric value.',
    call. = F)

  grob_row_class$new(
    contents = list(...),
    proportion = p,
    border = border,
    border_sides = border_sides,
    border_aes_list = border_aes_list,
    title = title,
    title_p = title_p,
    title_aes_list = title_aes_list)
}
