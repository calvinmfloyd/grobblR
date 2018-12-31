
add_title_grob <- function(grob, title, title_aes_list, title_p, title_height, total_height, width, padding){

  white_space_p <- 0.05
  grob_height <- title_height - padding

  title_grob <- grob_matrix(
    df = matrix(title, ncol = 1, nrow = 1),
    height = grob_height - grob_height*white_space_p,
    width = width,
    aes_list = title_aes_list,
    m_type = 5)

  gridExtra::arrangeGrob(
    grobs = grid::gList(grid::nullGrob(), title_grob, grid::nullGrob(), grob)
    ,layout_matrix = matrix(c(1, 2, 3, 4), ncol = 1)
    ,heights = grid::unit(c(
      padding,
      grob_height - grob_height*white_space_p,
      grob_height*white_space_p,
      total_height - total_height*title_p), 'mm')
    ,widths = grid::unit(width, 'mm'))

}
