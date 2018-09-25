#' The grob row function which helps gives the grob from the GrobblR::grob_layout() function its shape. Works hand in hand with the GrobblR::grob_col() function.
#'
#' @param ... A series of grob_col's.
#' @param p The numeric proportion of the given height which should be given to sub-grob's outputted in this grob row. Defaults to 1.
#' @param border A TRUE/FALSE argument corresponding to whether or not a border around the outputted sub-grob (or sub-grob's) is desired. Defaults to FALSE.
#' @param border_aes_list A list which contains desired aesthetics for the border around the outputted the sub-grob. Ignored if the border param is set to FALSE. Elements used within grid::gpar().
#' @return An R6 class which contains all the information needed to carry on to the grob_col's to create their sub-grob's.
#' @export

grob_row <- function(..., p = 1, border = F, border_aes_list = list()){

  grob_row_class <- R6::R6Class(
    "grob_row",
    public = list(
      height = 0,
      width = 0,
      padding = 0,
      proportion = 1,
      grob_classes = list(),
      border = F,
      border_aes_list = list(),
      initialize = function(grob_classes, proportion, border, border_aes_list){

        stopifnot(
          is.list(grob_classes)
          ,is.numeric(proportion)
          ,is.logical(border)
          ,is.list(border_aes_list)
        )

        self$grob_classes <- grob_classes
        self$proportion <- proportion
        self$border <- border
        self$border_aes_list <- border_aes_list
      }),
    active = list(
      grob = function(gc = self$grob_classes,
                      ht = self$height,
                      wth = self$width,
                      pad = self$padding,
                      bor = self$border,
                      bor_aes_list = self$border_aes_list){

        props <- unlist(lapply(1:length(gc), function(i) gc[[i]]$proportion))
        widths <- (props/sum(props))*(wth - 2*pad)
        height <- ht - 2*pad
        raw_grobs <- grid::gList()

        for(i in 1:length(gc)){
          gc[[i]]$height <- height
          gc[[i]]$width <- widths[i]
          gc[[i]]$padding <- pad
          raw_grobs <- grid::gList(raw_grobs, gc[[i]]$grob)
        }

        g <- gridExtra::arrangeGrob(
          grobs = raw_grobs,
          layout_matrix = matrix(1:length(raw_grobs), nrow = 1),
          heights = grid::unit(height, 'mm'),
          widths = grid::unit(widths, 'mm'))

        if(bor){
          class(bor_aes_list) <- 'gpar'
          bor_aes_list$fill <- NA
          g <- grid::grobTree(
            g,
            grid::rectGrob(height = grid::unit(ht, 'mm'), width = grid::unit(wth, 'mm'), gp = bor_aes_list)
          )
        }

        g

      }
    ))

  grob_row_class$new(
    grob_classes = list(...),
    proportion = p,
    border = border,
    border_aes_list = border_aes_list)
}
