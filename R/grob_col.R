
#' Grob Column
#' 
#' The grob-column function where an object is converted a grob. Works within \code{\link{grob_row}}.
#'
#' @param ... Either the object to be converted to a grob, or a combination of 
#' grob-rows which need to be converted to sub-grobs.
#' 
#' @param p The numeric proportion of the width given to the outer grob-row which 
#' should be given to the grob-column outputted by this function. Defaults to 1.
#' 
#' @param width The numeric width of the grob-column in millimeters.
#' 
#' Overrides the \code{p} parameter. 
#' 
#' @param aes_list The list outputted by \code{\link{ga_list}}, which controls
#' aesthetics object within the grob-column.
#' 
#' @param border A TRUE/FALSE argument corresponding to whether or not a border 
#' around the outputted grob-column is desired. Defaults to FALSE.
#' 
#' @param border_aes_list The list outputted by \code{\link{ga_list}}, which 
#' controls aesthetics of the borders. Only two aesthetics that can be tweaked
#' for borders are \code{border_color}, \code{border_width} and \code{border_sides}.
#' 
#' Ignored if \code{border} is set to FALSE.
#' 
#' @param title A character string which will be displayed as the title of the grob-column.
#' 
#' @param title_aes_list The list outputted by \code{\link{ga_list}}, which
#' controls aesthetics of the title of the grob-column.
#' 
#' @param title_p The numeric proportion of height within the grob-column which
#' will be used by the title grob.
#' 
#' @param title_height The numeric height in mm within the grob-column which will
#' be used by the title grob. Will override \code{title_p} if provided.
#' 
#' @param caption A character string which will be displayed as the caption of the grob-column.
#' 
#' @param caption_aes_list The list outputted by \code{\link{ga_list}}, which
#' controls aesthetics of the caption of the grob-column.
#' 
#' @param caption_p The numeric proportion of height within the grob-column
#' which will be used by the caption grob.
#' 
#' @param caption_height The numeric height in mm within the grob-column which will
#' be used by the caption grob. Will override \code{caption_p} if provided.
#' 
#' @param padding_p The proportion of the minimum of the height and width which
#' will be used for the padding around the edge of the grob-column.
#' 
#' Overridden by any numeric value provided in the \code{padding} parameter.
#' 
#' @param padding The numeric amount of padding around the edge of the grob-column in millimeters.
#' 
#' Overrides the \code{padding_p} parameter.
#' 
#' @param hjust A numeric value which will determine the alignment of the grob
#' horizontally within its designated area. A value of 0 means moving the grob
#' all the way to the left, a value of 1 means moving the grob all the way to
#' the right and a value of 0.5 means keeping the grob in the middle.
#' Defaults to 0.5.
#' 
#' The grob-column is moved around within its padding, so if
#' there is no padding, then \code{hjust} will be rendered useless.
#' 
#' @param vjust A numeric value which will determine the alignment of the grob 
#' vertically within its designated area. A value of 0 means moving the grob all
#' the way to the bottom, a value of 1 means moving the grob all the way to the
#' top and a value of 0.5 means keeping the grob in the middle. Defaults to 0.5.
#' 
#' The grob-column is moved around within its padding, so if there is no padding,
#' then \code{vjust} will be rendered useless.
#' 
#' @return An R6 class object containing all the information needed to create the grob-column.
#' 
#' @export
#' 
#' @examples 
#' 
#' grob_col(
#'   "grob-column",
#'   aes_list = ga_list(
#'     text_color = "red",
#'     background_color = "gray90"
#'     )
#'   ) %>%
#'  view_grob(100, 100)
#' 

grob_col = function(...,
                    p = 1,
                    width = NA_real_,
                    aes_list = ga_list(),
                    border = FALSE,
                    border_aes_list = ga_list(),
                    title = "",
                    title_aes_list = ga_list(),
                    title_p = 0.15,
                    title_height = NA_real_,
                    caption = "",
                    caption_aes_list = ga_list(),
                    caption_p = 0.15,
                    caption_height = NA_real_,
                    padding_p = 0.05,
                    padding = NA_real_,
                    hjust = 0.5,
                    vjust = 0.5){

  if (!isTRUE(is.numeric(p) & p > 0)) {
    
    stop("p in grob_col() must be a non-zero positive numeric value.", call. = FALSE)
    
  }
  
  grob_col_class$new(
    contents = list(...),
    aes_list = aes_list,
    proportion = p,
    width = width,
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
    caption_height = caption_height,
    hjust = hjust,
    vjust = vjust
    )
}
