
add_page_number = function(grob, page_number, padding){

  grob_height = sum(as.numeric(grob$heights))
  grob_width = sum(as.numeric(grob$widths))
  
  pn_pad_pct = (0.3)/2
  ga_l = ga_list(text_color = 'gray40', background_color = NA)
  if(page_number == '') ga_l$text_cex = convert_to_matrix(1)
  
  page_number_grob = convert_to_grob(
    x = page_number,
    height = padding*(1 - 2*pn_pad_pct),
    width = padding*(1 - 2*pn_pad_pct),
    aes_list = ga_l
    )

  layout_matrix_w_pn = matrix(NA, nrow = 5, ncol = 5)
  layout_matrix_w_pn[2,2] = 1
  layout_matrix_w_pn[4,4] = 2

  gridExtra::arrangeGrob(
    grobs = grid::gList(grob, page_number_grob),
    heights = grid::unit(
      c(padding, grob_height, padding*pn_pad_pct, (padding - 2*padding*pn_pad_pct), padding*pn_pad_pct),
      'mm'),
    widths = grid::unit(
      c(padding, grob_width, padding*pn_pad_pct, (padding - 2*padding*pn_pad_pct), padding*pn_pad_pct),
      'mm'),
    layout_matrix = layout_matrix_w_pn
    )

}
