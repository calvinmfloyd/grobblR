
add_title_grob <- function(grob, title, title_aes_list, title_p, title_height, total_height, width){

  white_space_p <- 0.15

  title_grob <- grob_matrix(
    df = matrix(title, ncol = 1, nrow = 1),
    height = title_height - title_height*white_space_p,
    width = width,
    aes_list = title_aes_list,
    m_type = 5)

  g <- gridExtra::arrangeGrob(
    grobs = grid::gList(title_grob, grid::nullGrob(), grob)
    ,layout_matrix = matrix(c(1, 2, 3), ncol = 1)
    ,heights = grid::unit(c(
      title_height - title_height*white_space_p,
      title_height*white_space_p,
      total_height - total_height*title_p), 'mm')
    ,widths = grid::unit(width, 'mm'))

}
