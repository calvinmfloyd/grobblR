
add_title_grob = function(grob,
                          title,
                          title_aes_list,
                          title_p,
                          title_height,
                          units) {

  white_space_p = 0.05
  grob_height = sum(as.numeric(grob$heights))
  width = sum(as.numeric(grob$widths))
  
  if(is.null(title_aes_list$text_cex)){
    n_lines = ifelse(!is.null(title_aes_list$n_lines), title_aes_list$n_lines, 1)
    sep = ifelse(!is.null(title_aes_list$sep), title_aes_list$sep, '\n')
    lines = cex_val_convergence(
      string = title,
      n_lines = n_lines,
      sep = sep,
      height = title_height,
      width = width,
      units = units
      )
    title_aes_list$text_cex = convert_to_matrix(lines$cex_val)
    title = lines$lines
    if(length(title) > 1) title_aes_list$round_rect_radius = convert_to_matrix(0)
  }
  
  title_grob = grob_matrix(
    df = matrix(title, ncol = 1),
    height = title_height - title_height*white_space_p,
    width = width,
    units = units,
    aes_list = title_aes_list,
    m_type = 5
    )

  gridExtra::arrangeGrob(
    grobs = grid::gList(title_grob, grid::nullGrob(), grob),
    layout_matrix = matrix(c(1, 2, 3), ncol = 1),
    heights = grid::unit(
      units = units,
      x = c(
        title_height - title_height*white_space_p,
        title_height*white_space_p,
        grob_height
        )
      ),
    widths = grid::unit(x = width, units = units)
    )

}
