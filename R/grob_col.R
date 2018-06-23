#' The grob column function which helps gives the grob from the grobblR::grob_layout() function its shape. Works hand in hand with the grobblR::grob_row() function.
#'
#' @param ... Either the object to be converted to a grob, or a combination of grob_row's which need to be converted to sub-grob's.
#' @param p The numeric proportion of the width given to the outer grob_row which should be given to the sub-grob outputted by this function. Defaults to 1.
#' @param aes_list A list of arguments corresponding to the desired sub-grob which affect its appearance. Default is an empty list.
#' @param border A TRUE/FALSE argument corresponding to whether or not a border around the outputted sub-grob is desired. Defaults to FALSE.
#' @param hjust A numeric value between 0 and 1 which will determine the alignment of the grob horizontally within its designated area. A value of 0 means moving the grob all the way to the left, a value of 1 means moving the grob all the way to the right and a value of 0.5 means keeping the grob in the middle. Defaults to 0.5.
#' @param vjust A numeric value between 0 and 1 which will determine the alignment of the grob vertically within its designated area. A value of 0 means moving the grob all the way to the bottom, a value of 1 means moving the grob all the way to the top and a value of 0.5 means keeping the grob in the middle. Defaults to 0.5.
#' @param border_args A gpar() list which contain desired aesthetics for the border around the outputted the sub-grob. Ignored if the border param is set to FALSE.
#' @return An R6 class which contains all the information needed to create the sub-grob. The sub-grob is obtained with grob_col$grob.
#' @export

grob_col <- function(..., p = 1, aes_list = list(), border = F, hjust = 0.5, vjust = 0.5, border_args = grid::gpar()){

  grob_col_class <- R6::R6Class(
    "grob_col",
    public = list(
      contents = list(),
      proportion = 1,
      height = 0,
      width = 0,
      hjust = 0.5,
      vjust = 0.5,
      padding = 0,
      aes_list = list(),
      border = F,
      border_args = grid::gpar(),
      initialize = function(contents, aes_list, proportion, border, border_args, hjust, vjust){
        stopifnot(is.list(contents), is.list(aes_list), is.numeric(proportion))
        self$contents <- contents
        self$proportion <- proportion
        self$aes_list <- aes_list
        self$border <- border
        self$border_args <- border_args
        self$hjust <- hjust
        self$vjust <- vjust
      }
    ),
    active = list(
      grob = function(contents = self$contents,
                      m_a = self$aes_list,
                      ht = self$height,
                      wth = self$width,
                      pad = self$padding,
                      bor = self$border,
                      bor_args = self$border_args,
                      hjust = self$hjust,
                      vjust = self$vjust){

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
            ctg <- convert_to_grob(x = contents[[i]], height = hts_w_padding[i], width = wth_w_padding, aes_list = m_a)
            g <- gridExtra::arrangeGrob(
              grobs = grid::gList(grid::nullGrob(), grid::nullGrob(), grid::nullGrob(), grid::nullGrob(), ctg)
              ,layout_matrix = cbind(3,rbind(1, 5, 2), 4)
              ,heights = grid::unit(c(2*pad*(1-vjust), hts_w_padding[i], 2*pad*vjust), 'mm')
              ,widths = grid::unit(c(2*pad*hjust, wth_w_padding[i], 2*pad*(1-hjust)), 'mm'))

            if(bor) g <- grid::grobTree(g, rectGrob(height = grid::unit(hts[i], 'mm'), width = grid::unit(wth, 'mm'), gp = bor_args))
            raw_grobs <- grid::gList(raw_grobs, g)
          }
        }

        gridExtra::arrangeGrob(
          grobs = raw_grobs,
          layout_matrix = matrix(1:length(raw_grobs), ncol = 1),
          height = grid::unit(hts, 'mm'),
          width = grid::unit(wth, 'mm'))
      }
    ))

  stopifnot(hjust >= 0, hjust <= 1)
  stopifnot(vjust >= 0, vjust <= 1)

  grob_col_contents <- list(...)
  grob_col_class$new(
    contents = grob_col_contents,
    aes_list = aes_list,
    proportion = p,
    border = border,
    border_args = border_args,
    hjust = hjust,
    vjust = vjust)
}
