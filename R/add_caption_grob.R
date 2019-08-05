
add_caption_grob = function(grob,
                            caption,
                            caption_aes_list,
                            caption_p,
                            caption_height,
                            units) {

  white_space_p = 0.05
  grob_height = sum(as.numeric(grob$heights))
  width = sum(as.numeric(grob$widths))
  
  if(is.null(caption_aes_list$text_cex)){
    n_lines = ifelse(!is.null(caption_aes_list$n_lines), caption_aes_list$n_lines, 10000)
    sep = ifelse(!is.null(caption_aes_list$sep), caption_aes_list$sep, '\n')
    lines = cex_val_convergence(
      string = caption,
      n_lines = n_lines,
      sep = sep,
      height = caption_height,
      width = width,
      units = units
      )
    caption_aes_list$text_cex = convert_to_matrix(lines$cex_val)
    caption = lines$lines
    if(length(caption) > 1) caption_aes_list$round_rect_radius = convert_to_matrix(0)
  }
  
  caption_grob = grob_matrix(
    df = matrix(caption, ncol = 1),
    height = caption_height - caption_height*white_space_p,
    width = width,
    units = units,
    aes_list = caption_aes_list,
    m_type = 4
    )

  gridExtra::arrangeGrob(
    grobs = grid::gList(grob, grid::nullGrob(), caption_grob),
    layout_matrix = matrix(c(1, 2, 3), ncol = 1),
    heights = grid::unit(
      units = units,
      x = c(
        grob_height,
        caption_height*white_space_p,
        caption_height - caption_height*white_space_p
        )
      ),
    widths = grid::unit(x = width, units = units)
    )

}
