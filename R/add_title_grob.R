
add_title_grob = function(grob, title, title_aes_list, title_p, title_height, padding){

  white_space_p = 0.05
  title_grob_height = title_height
  grob_height = sum(as.numeric(grob$heights))
  width = sum(as.numeric(grob$widths))
  
  title_grob = grob_matrix(
    df = matrix(title, ncol = 1, nrow = 1),
    height = title_grob_height - title_grob_height*white_space_p,
    width = width,
    aes_list = title_aes_list,
    m_type = 5
    )

  gridExtra::arrangeGrob(
    grobs = grid::gList(title_grob, grid::nullGrob(), grob)
    ,layout_matrix = matrix(c(1, 2, 3), ncol = 1)
    ,heights = grid::unit(c(
      title_grob_height - title_grob_height*white_space_p,
      title_grob_height*white_space_p,
      grob_height), 'mm')
    ,widths = grid::unit(width, 'mm')
    )

}
