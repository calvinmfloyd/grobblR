#' The grob column function which helps gives the grob from the \code{grob_layout()} function its shape. Works hand in hand with the \code{grob_row()} function.
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
#' @param title_p The numeric proportion of height within the grob column and its allotted space which will be used by the title grob. A numeric value between 0 and 1. Defaults to 0.2.
#' @param vjust A numeric value between 0 and 1 which will determine the alignment of the grob vertically within its designated area. A value of 0 means moving the grob all the way to the bottom, a value of 1 means moving the grob all the way to the top and a value of 0.5 means keeping the grob in the middle. Defaults to 0.5.
#' @return An R6 class which contains all the information needed to create the grob column. The grob column is obtained with grob_col$grob.
#' @export

grob_col <- function(...,
                     p = 1,
                     aes_list = list(),
                     border = F,
                     border_sides = 'top, bottom, left, right',
                     border_aes_list = list(),
                     title = '',
                     title_aes_list = list(),
                     title_p = 0.2,
                     hjust = 0.5,
                     vjust = 0.5){

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
      border_sides = 'top, bottom, left, right',
      border_aes_list = list(),
      title = character(),
      title_proportion = 0.1,
      title_aes_list = list(),
      initialize = function(contents,
                            aes_list,
                            proportion,
                            border,
                            border_sides,
                            border_aes_list,
                            title,
                            title_proportion,
                            title_aes_list,
                            hjust,
                            vjust){

        self$contents <- contents
        self$proportion <- proportion
        self$aes_list <- aes_list
        self$border <- border
        self$border_aes_list <- border_aes_list
        self$border_sides <- border_sides
        self$title <- title
        self$title_proportion <- title_proportion
        self$title_aes_list <- title_aes_list
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
                      bor_sides = self$border_sides,
                      bor_aes_list = self$border_aes_list,
                      title = self$title,
                      title_proportion = self$title_proportion,
                      title_aes_list = self$title_aes_list,
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
            contents[[i]]$width <- wth_w_padding
            contents[[i]]$padding <- pad
            raw_grobs <- grid::gList(raw_grobs, contents[[i]]$grob)

          } else {

            title_present <- nchar(title) > 0

            ctg <- convert_to_grob(
              x = contents[[i]],
              height = hts_w_padding[i] - hts_w_padding[i]*title_proportion*title_present,
              width = wth_w_padding,
              aes_list = m_a)

            if(title_present){

              white_space_p <- 0.15

              title_grob <- grob_matrix(
                df = matrix(title, ncol = 1, nrow = 1),
                height = hts_w_padding[i]*title_proportion - (hts_w_padding[i]*title_proportion)*(white_space_p),
                width = wth_w_padding,
                aes_list = title_aes_list,
                m_type = 5)

              ctg <- gridExtra::arrangeGrob(
                grobs = grid::gList(title_grob, grid::nullGrob(), ctg)
                ,layout_matrix = matrix(c(1, 2, 3), ncol = 1)
                ,heights = grid::unit(c(
                  hts_w_padding[i]*title_proportion - (hts_w_padding[i]*title_proportion)*(white_space_p),
                  (hts_w_padding[i]*title_proportion)*(white_space_p),
                  hts_w_padding[i] - hts_w_padding[i]*title_proportion), 'mm')
                ,widths = grid::unit(wth_w_padding, 'mm'))
            }

            g <- gridExtra::arrangeGrob(
              grobs = grid::gList(grid::nullGrob(), grid::nullGrob(), grid::nullGrob(), grid::nullGrob(), ctg)
              ,layout_matrix = cbind(3,rbind(1, 5, 2), 4)
              ,heights = grid::unit(c(2*pad*(1-vjust), hts_w_padding[i], 2*pad*vjust), 'mm')
              ,widths = grid::unit(c(2*pad*hjust, wth_w_padding[i], 2*pad*(1-hjust)), 'mm'))

            if(bor){
              class(bor_aes_list) <- 'gpar'
              cell_border_gs <- grid::gList()
              borders_split <- unlist(strsplit(bor_sides, split = ', ', fixed = TRUE))
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

            raw_grobs <- grid::gList(raw_grobs, g)
          }
        }

        gridExtra::arrangeGrob(
          grobs = raw_grobs,
          layout_matrix = matrix(1:length(raw_grobs), ncol = 1),
          heights = grid::unit(hts, 'mm'),
          widths = grid::unit(wth, 'mm'))
      }
    ))

  if(hjust < 0 | hjust > 1) stop("hjust in grob_col() must be a numeric value between 0 and 1.", call. = F)
  if(vjust < 0 | vjust > 1) stop("vjust in grob_col() must be a numeric value between 0 and 1.", call. = F)
  if(title_p < 0 | title_p > 1) stop("title_p in grob_col() must be a numeric value between 0 and 1.", call. = F)
  if(!is.character(title)) stop('title in grob_col() must be a character string.', call. = F)
  if(!is.numeric(p)) stop('p in grob_col() must be a numeric value.', call. = F)
  if(!is.logical(border)) stop('border in grob_col() must be a TRUE/FALSE value.', call. = F)
  if(!is.list(aes_list)) stop('aes_list in grob_col() must be a list.', call. = F)
  if(!is.list(border_aes_list)) stop('border_aes_list in grob_col() must be a list.', call. = F)
  if(!is.character(border_sides)) stop(
    "border_sides in grob_col() must be a character string with 'top', 'bottom', 'left' or 'right' separated with ', '.",
    call. = F)

  grob_col_contents <- list(...)
  grob_col_class$new(
    contents = grob_col_contents,
    aes_list = aes_list,
    proportion = p,
    border = border,
    border_sides = tolower(border_sides),
    border_aes_list = border_aes_list,
    title = title,
    title_proportion = title_p,
    title_aes_list = title_aes_list,
    hjust = hjust,
    vjust = vjust)
}
