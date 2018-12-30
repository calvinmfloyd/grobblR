#' The main \code{grobblR} function which contains and organizes \code{\link{grob_col}}'s and \code{\link{grob_row}}'s, giving the overall grob-layout its shape.
#'
#' @param ... The combination of grob_row's and grob_col's which will help give the main grob outputted its shape and look.
#' @param height The numeric height of the grob in mm. Default is 280 mm - which is the height of an upright 8.5 x 11 in piece of copy paper.
#' @param width The numeric width of the grob in mm. Default is 216 mm - which is the width of an upright 8.5 x 11 in piece of copy paper.
#' @param padding The numeric amount of padding around the edge of the grob  in mm. Default is 10 mm.
#' @param grob_padding The numeric amount of padding for each individual grob within their designated grid of area in mm. Default is 2 mm.
#' @param title A character string which will be displayed as the title of the grob layout.
#' @param title_aes_list A list which contains desired aesthetics for the title of the grob layout. Elements of this list are treated the same way as \code{aes_list} - see \code{\link{grob_matrix}} for more details on its possible elements.
#' @param title_p The numeric proportion of height within the grob layout and its allotted space which will be used by the title grob. A numeric value between 0 and 1. Defaults to 0.1.
#' @param row_heights If the user wants to designate specific row heights instead relying on the proportions within the outermost grob_row functions, set this parameter equal to a vector of numeric values corresponding to the individual row heights in mm. Length must be equal to the number of grob_row function's on the upper most level of the grob.
#' @return An R6 class containing all information necessary to create the overall grob-layout. The grob itself is called with \code{grob_layout()$grob}.
#' @examples
#' gl <- grob_layout(
#'   grob_row(grob_col(1), grob_col(2)),
#'   grob_row(grob_col(3)))
#' # to retrieve the grob-layout
#' gl$grob
#' @export

grob_layout <- function(...,
                        height = 280,
                        width = 216,
                        padding = 10,
                        grob_padding = 2,
                        title = '',
                        title_aes_list = ga_list(),
                        title_p = 0.1,
                        page_number = '',
                        row_heights = c()){

  grob_layout_class$new(
    contents = unlist(list(...)),
    height = height,
    width = width,
    padding = padding,
    grob_padding = grob_padding,
    page_number = page_number,
    row_heights = row_heights,
    title = title,
    title_p = title_p,
    title_aes_list = title_aes_list)

}
