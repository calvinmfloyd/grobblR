#' The grob row function which helps gives the grob from the \code{grobblR::grob_layout()} function its shape. Works hand in hand with the \code{grobblR::grob_col()} function.
#'
#' @param ... A series of \code{grob_col}'s.
#' @param p The numeric proportion of the given height which should be given to sub-grob's outputted in this grob row. Defaults to 1.
#' @param border A TRUE/FALSE argument corresponding to whether or not a border around the outputted grob row is desired. Defaults to FALSE.
#' @param border_sides Controls the borders around the total grob row. The input is a string with the possible words "top", "bottom", "left", "right" separated by ", ". For example, "top, left, right" will put borders on the top, left and right side of the grid cell, but not the bottom. Default is "top, bottom, left, right", or all borders.
#' @param border_aes_list A list which contains desired aesthetics for the border around the outputted the grob row. Ignored if \code{border} is set to FALSE. Elements of list inputted directly into \code{grid::gpar()}.
#' @param title A character string which will be displayed as the title of the grob row.
#' @param title_aes_list A list which contains desired aesthetics for the title of the grob row. Elements of this list are treated the same way as \code{aes_list} - see \code{\link{grob_matrix}} for more details on its possible elements.
#' @param title_p The numeric proportion of height within the grob row and its allotted space which will be used by the title grob. A numeric value between 0 and 1. Defaults to 0.2.
#' @return An R6 class which contains all the information needed to carry on to its grob columns and create the grob row.
#' @export

grob_row <- function(...,
                     p = 1,
                     border = F,
                     border_sides = 'top, bottom, left, right',
                     border_aes_list = list(),
                     title = '',
                     title_aes_list = list(),
                     title_p = 0.1){

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
      border_sides = 'top, bottom, left, right',
      title = character(),
      title_proportion = 0.2,
      title_aes_list = list(),
      initialize = function(grob_classes,
                            proportion,
                            border,
                            border_sides,
                            border_aes_list,
                            title,
                            title_proportion,
                            title_aes_list){

        self$grob_classes <- grob_classes
        self$proportion <- proportion
        self$border <- border
        self$border_sides <- border_sides
        self$border_aes_list <- border_aes_list
        self$title <- title
        self$title_proportion <- title_proportion
        self$title_aes_list <- title_aes_list

      }),
    active = list(
      grob = function(gc = self$grob_classes,
                      ht = self$height,
                      wth = self$width,
                      pad = self$padding,
                      bor = self$border,
                      bor_sides = self$border_sides,
                      bor_aes_list = self$border_aes_list,
                      title = self$title,
                      title_proportion = self$title_proportion,
                      title_aes_list = self$title_aes_list){

        if(!all(unlist(lapply(gc, class)) %in% c('R6', 'grob_col'))) stop(
          "Did you remember to wrap all of your objects within grob_row() with grob_col()?",
          call. = F)

        title_present <- nchar(title) > 0

        props <- unlist(lapply(1:length(gc), function(i) gc[[i]]$proportion))
        widths <- (props/sum(props))*(wth - 2*pad)
        height <- ht - 2*pad
        height_edit <- height - height*title_proportion*title_present
        raw_grobs <- grid::gList()

        for(i in 1:length(gc)){
          gc[[i]]$height <- height_edit
          gc[[i]]$width <- widths[i]
          gc[[i]]$padding <- pad
          raw_grobs <- grid::gList(raw_grobs, gc[[i]]$grob)
        }

        g <- gridExtra::arrangeGrob(
          grobs = raw_grobs,
          layout_matrix = matrix(1:length(raw_grobs), nrow = 1),
          heights = grid::unit(height_edit, 'mm'),
          widths = grid::unit(widths, 'mm'))

        if(title_present){

          title_grob <- grob_matrix(
            df = matrix(title, ncol = 1, nrow = 1),
            height = height*title_proportion,
            width = sum(widths),
            aes_list = title_aes_list,
            m_type = 5)

          g <- gridExtra::arrangeGrob(
            grobs = grid::gList(title_grob, g)
            ,layout_matrix = matrix(c(1,2), ncol = 1)
            ,heights = grid::unit(c(
              height*title_proportion,
              height_edit), 'mm')
            ,widths = grid::unit(sum(widths), 'mm'))

        }

        if(bor){

          class(bor_aes_list) <- 'gpar'

          cell_border_gs <- grid::gList()
          borders_split <- unlist(strsplit(bor_sides, split = ', ', fixed = T))

          if(length(borders_split) > 0){
            for(side in 1:length(borders_split)){

              cell_border_gs <- grid::gList(
                cell_border_gs,
                grid::segmentsGrob(
                  x0 = grid::unit(ifelse(borders_split[side] %in% c("right"), 1, 0), "npc"),
                  y0 = grid::unit(ifelse(borders_split[side] %in% c("top"), 1, 0), "npc"),
                  x1 = grid::unit(ifelse(borders_split[side] %in% c("top", "bottom", "right"), 1, 0), "npc"),
                  y1 = grid::unit(ifelse(borders_split[side] %in% c("left", "right", "top"), 1, 0), "npc"),
                  gp = bor_aes_list))
            }
            g <- grid::grobTree(g, cell_border_gs)
          }
        }

        g

      }
    ))

  if(title_p < 0 | title_p > 0.5 | !is.numeric(title_p)) stop(
    "title_p in grob_row() must be a numeric value between 0 and 0.5.",
    call. = F)
  if(!is.character(title) | length(title) > 1) stop(
    'title in grob_row() must be a single character string.',
    call. = F)
  if(!is.numeric(p)) if(p < 0) stop(
    'p in grob_row() must be a positive numeric value.',
    call. = F)
  if(!is.logical(border)) stop(
    'border in grob_row() must be a TRUE/FALSE value.',
    call. = F)
  if(!is.list(border_aes_list)) stop(
    'border_aes_list in grob_row() must be a list.',
    call. = F)
  if(!is.list(title_aes_list)) stop(
    'title_aes_list in grob_row() must be a list.',
    call. = F)
  if(!is.character(border_sides)) stop(
    "border_sides in grob_row() must be a character string with 'top', 'bottom', 'left' or 'right' separated with ', '.",
    call. = F)

  grob_row_class$new(
    grob_classes = list(...),
    proportion = p,
    border = border,
    border_sides = border_sides,
    border_aes_list = border_aes_list,
    title = title,
    title_proportion = title_p,
    title_aes_list = title_aes_list)
}
