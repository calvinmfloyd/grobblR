#' The main grobblR function which contains and organizes the GrobblR::grob_col() and grobblR::grob_row() functions, giving the combined grob its shape.
#'
#' @param ... The combination of grob_row's and grob_col's which will help give the main grob outputted its shape and look.
#' @param height The numeric height of the grob in mm. Default is 280 mm - which is the height of an upright 8.5 x 11 in piece of copy paper.
#' @param width The numeric width of the grob in mm. Default is 216 mm - which is the width of an upright 8.5 x 11 in piece of copy paper.
#' @param padding The numeric amount of padding around the edge of the grob  in mm. Default is 5 mm.
#' @param grob_padding The numeric amount of padding for each individual grob within their designated grid of area in mm. Default is 2 mm.
#' @param row_heights If the user wants to designate specific row heights instead relying on the proportions within the grob_row functions, set this parameter equal to a vector of numeric values corresponding to the individual row heights in mm. Length must be equal to the number of grob_row function's on the upper most level of the grob.
#' @return The final grob with the designated layout and format.
#' @export

grob_layout <- function(...,
                        height = 280,
                        width = 216,
                        padding = 5,
                        grob_padding = 2,
                        row_heights = c()){

  # Initializing Variables ----
  ph <- height - 2*padding
  pw <- width - 2*padding
  g_info <- list(...)
  # ----

  # Creating the Layout Matrix, using LCM ----
  nr <- length(g_info)
  layout_matrix <- matrix(1:nr, ncol = 1)
  # ----

  # Calculating Row Heights ----
  rh_inputted <- length(row_heights) > 0
  rh_wrong_length <- length(row_heights) != nr
  if(rh_inputted & rh_wrong_length) warning(sprintf("row_heights param must have length of %d.", nr))
  if(rh_wrong_length){
    row_props <- sapply(1:nr, function(i) g_info[[i]]$proportion)
    row_heights <- ph*(row_props/sum(row_props))
  }
  # ----

  # Readjusting Grob Widths to fit in the given Page Height and Page Width ----
  raw_grobs <- grid::gList()
  for(i in 1:nr){
    g_info[[i]]$height <- row_heights[i]
    g_info[[i]]$width <- pw
    g_info[[i]]$padding <- grob_padding
    raw_grobs <- grid::gList(raw_grobs, g_info[[i]]$grob)
  }
  # ----

  grob <- gridExtra::arrangeGrob(
    grobs = raw_grobs,
    heights = grid::unit(c(padding, row_heights, padding), 'mm'),
    widths = grid::unit(c(padding, pw, padding), 'mm'),
    layout_matrix = cbind(NA, rbind(NA, layout_matrix, NA), NA))

  grob

}
