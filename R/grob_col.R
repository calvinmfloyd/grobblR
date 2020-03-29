#' The grob-column function where an object is converted a grob. Works within \code{\link{grob_row}} and \code{\link{grob_layout}}.
#'
#' @param ... Either the object to be converted to a grob, or a combination of grob-rows which need to be converted to sub-grobs.
#' @param p The numeric proportion of the width given to the outer grob-row which should be given to the grob-column outputted by this function. Defaults to 1.
#' @param aes_list The list outputted by \code{\link{ga_list}}, which controls aesthetics object within the grob-column.
#' @param border A TRUE/FALSE argument corresponding to whether or not a border around the outputted grob-column is desired. Defaults to FALSE.
#' @param border_aes_list The list outputted by \code{\link{ga_list}}, which controls aesthetics of the borders. Only two aesthetics that can be tweaked for borders are \code{border_color}, \code{border_width} and \code{border_sides}. Ignored if \code{border} is set to FALSE.
#' @param title A character string which will be displayed as the title of the grob-column.
#' @param title_aes_list The list outputted by \code{\link{ga_list}}, which controls aesthetics of the title of the grob-column.
#' @param title_p The numeric proportion of height within the grob-column which will be used by the title grob. A numeric value between 0 and 0.25.
#' @param caption A character string which will be displayed as the caption of the grob-column.
#' @param caption_aes_list The list outputted by \code{\link{ga_list}}, which controls aesthetics of the caption of the grob-column.
#' @param caption_p The numeric proportion of height within the grob-column which will be used by the caption grob. A numeric value between 0 and 0.25.
#' @param hjust A numeric value which will determine the alignment of the grob horizontally within its designated area. A value of 0 means moving the grob all the way to the left, a value of 1 means moving the grob all the way to the right and a value of 0.5 means keeping the grob in the middle. Defaults to 0.5. The grob-column is moved around within its padding, so if there is no padding, then \code{hjust} will be rendered useless.
#' @param vjust A numeric value which will determine the alignment of the grob vertically within its designated area. A value of 0 means moving the grob all the way to the bottom, a value of 1 means moving the grob all the way to the top and a value of 0.5 means keeping the grob in the middle. Defaults to 0.5. The grob-column is moved around within its padding, so if there is no padding, then \code{vjust} will be rendered useless.
#' @param padding_p The proportion of the minimum of the height and width which will be used for the padding around the edge of the grob-column. Overridden by any numeric value provided in the \code{padding} parameter.
#' @param padding The numeric amount of padding around the edge of the grob-column in the \code{units} supplied by the grob-layout. Overrides the \code{padding_p} parameter.
#' @param width The numeric width of the grob-column in the \code{units} supplied by the grob-layout. Overrides the \code{p} parameter. 
#' @return An R6 class object which contains all the information needed to create the grob-column.
#' @details The individual grob-column is obtained with \code{grob_col()$grob}.
#' @export

grob_col = function(...,
                    p = 1,
                    aes_list = ga_list(),
                    border = F,
                    border_aes_list = ga_list(),
                    title = '',
                    title_aes_list = ga_list(),
                    title_p = 0.15,
                    caption = '',
                    caption_aes_list = ga_list(),
                    caption_p = 0.15,
                    padding_p = 0.05,
                    width = NA_real_,
                    padding = NA_real_,
                    hjust = 0.5,
                    vjust = 0.5){

  if(!is.numeric(p)) {
    
    if(p < 0) stop("p in grob_col() must be a positive numeric value.", call. = FALSE)
    
  }
  
  grob_col_class$new(
    contents = list(...),
    aes_list = aes_list,
    proportion = p,
    padding = padding,
    padding_proportion = padding_p,
    border = border,
    border_aes_list = border_aes_list,
    title = title,
    title_p = title_p,
    title_aes_list = title_aes_list,
    caption = caption,
    caption_p = caption_p,
    caption_aes_list = caption_aes_list,
    width = width,
    hjust = hjust,
    vjust = vjust
    )
}
