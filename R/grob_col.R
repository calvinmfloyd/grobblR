#' The grob-column function where an object is converted a grob. Works within \code{\link{grob_row}} and \code{\link{grob_layout}}.
#'
#' @param ... Either the object to be converted to a grob, or a combination of grob row's which need to be converted to sub-grob's.
#' @param p The numeric proportion of the width given to the outer grob_row which should be given to the grob column outputted by this function. Defaults to 1.
#' @param aes_list A list which contains desired aesthetics of the grob column and its object. Default is an empty list.
#' @param border A TRUE/FALSE argument corresponding to whether or not a border around the outputted grob column is desired. Defaults to FALSE.
#' @param border_sides Controls the borders around the total grob column. The input is a string with the possible words "top", "bottom", "left", "right" separated by ", ". For example, "top, left, right" will put borders on the top, left and right side of the grid cell, but not the bottom. Default is "top, bottom, left, right", or all borders.
#' @param border_aes_list A list which contains desired aesthetics for the border around the outputted the grob column. Ignored if \code{border} is set to FALSE. Elements of list inputted directly into \code{grid::gpar()}.
#' @param hjust A numeric value between 0 and 1 which will determine the alignment of the grob horizontally within its designated area. A value of 0 means moving the grob all the way to the left, a value of 1 means moving the grob all the way to the right and a value of 0.5 means keeping the grob in the middle. Defaults to 0.5.
#' @param title A character string which will be displayed as the title of the grob column.
#' @param title_aes_list A list which contains desired aesthetics for the title of the grob column. Elements of this list are treated the same way as \code{aes_list} - see \code{\link{grob_matrix}} for more details on its possible elements.
#' @param title_p The numeric proportion of height within the grob column and its allotted space which will be used by the title grob. A numeric value between 0 and 1.
#' @param vjust A numeric value between 0 and 1 which will determine the alignment of the grob vertically within its designated area. A value of 0 means moving the grob all the way to the bottom, a value of 1 means moving the grob all the way to the top and a value of 0.5 means keeping the grob in the middle. Defaults to 0.5.
#' @return An R6 class which contains all the information needed to create the grob column. The grob column is obtained with grob_col$grob.
#' @export

grob_col <- function(...,
                     p = 1,
                     aes_list = ga_list(),
                     border = F,
                     border_sides = 'top, bottom, left, right',
                     border_aes_list = ga_list(),
                     title = '',
                     title_aes_list = ga_list(),
                     title_p = 0.2,
                     hjust = 0.5,
                     vjust = 0.5){

  if(!is.numeric(p)) if(p < 0) stop(
    "p in grob_col() must be a positive numeric value.",
    call. = F)

  grob_col_class$new(
    contents = list(...),
    aes_list = aes_list,
    proportion = p,
    border = border,
    border_sides = tolower(border_sides),
    border_aes_list = border_aes_list,
    title = title,
    title_p = title_p,
    title_aes_list = title_aes_list,
    hjust = hjust,
    vjust = vjust)
}
