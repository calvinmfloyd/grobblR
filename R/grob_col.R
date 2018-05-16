#' The grob column function which helps gives the grob from the GrobblR::grob_layout()
#' function its shape. Works hand in hand with the GrobblR::grob_row() function.
#'
#' @param ... Either the object to be converted to a grob, or a combination of
#' grob_row's which need to be converted to sub-grob's.
#' @param width_prop The numeric proportion of the width given to the outer grob_row which
#' should be given to the sub-grob outputted by this function. Defaults to 1.
#' @param more_args A list of arguments corresponding to the desired sub-grob which 
#' affect its appearance. Default is an empty list.
#' @param border A TRUE/FALSE argument corresponding to whether or not a border
#' around the outputted sub-grob is desired. Defaults to FALSE.
#' @param border_args A gpar() list which contain desired aesthetics for the border
#' around the outputted the sub-grob. Ignored if the border param is set to FALSE. 
#' @return An R6 class which contains all the information needed to create the sub-grob.
#' The sub-grob is obtained with grob_col$grob.
#' @export

grob_col <- function(..., width_prop = 1, more_args = list(), border = F, border_args = gpar()){
  
  grob_col_class <- R6::R6Class(
    "grob_col",
    public = list(
      contents = list(),
      proportion = 1,
      height = 0,
      width = 0,
      padding = 0,
      more_args = list(),
      border = F,
      border_args = grid::gpar(),
      initialize = function(contents, more_args, proportion, border, border_args){
        stopifnot(is.list(contents), is.list(more_args), is.numeric(proportion))
        self$contents <- contents
        self$proportion <- proportion
        self$more_args <- more_args
        self$border <- border
        self$border_args <- border_args
      }
    ),
    active = list(
      grob = function(contents = self$contents,
                      m_a = self$more_args,
                      ht = self$height,
                      wth = self$width,
                      pad = self$padding,
                      bor = self$border,
                      bor_args = self$border_args){
        
        wth_w_padding <- wth - 2*pad
        hts_w_padding <- rep((ht - 2*pad)/length(contents), length(contents))
        hts <- rep(ht/length(contents), length(contents))
        raw_grobs <- grid::gList()
        
        for(i in 1:length(contents)){
          if(R6::is.R6(contents[[i]])){
            height_props <- sapply(1:length(contents), function(i) contents[[i]]$proportion)
            contents[[i]]$height <- ht*(height_props/sum(height_props))[i]
            contents[[i]]$width <- wth
            contents[[i]]$padding <- pad
            raw_grobs <- grid::gList(raw_grobs, contents[[i]]$grob)
          } else {
            g <- convert_to_grob(x = contents[[i]], height = hts_w_padding[i], width = wth_w_padding, more_args = m_a)
            if(bor){
              g <- grid::grobTree(g, rectGrob(height = unit(hts[i], 'mm'), width = unit(wth, 'mm'), gp = bor_args))
            }
            raw_grobs <- grid::gList(raw_grobs, g)
          }
        }
        
        gridExtra::arrangeGrob(
          grobs = raw_grobs,
          layout_matrix = matrix(1:length(raw_grobs), ncol = 1),
          height = unit(hts, 'mm'),
          width = unit(wth, 'mm'))
      }
    ))
  
  grob_col_contents <- list(...)
  grob_col_class$new(
    contents = grob_col_contents,
    more_args = more_args,
    proportion = prop,
    border = border,
    border_args = border_args)
}
