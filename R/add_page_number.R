
add_page_number <- function(grob, page_number, padding, height, width){

  pn_pad_pct <- 0.9
  page_number_grob <- convert_to_grob(
    x = page_number,
    height = padding*pn_pad_pct,
    width = padding*pn_pad_pct,
    aes_list = ga_list(
      text_color = 'gray40',
      background_color = NA))

  layout_matrix_w_pn <- matrix(NA, nrow = 4, ncol = 4)
  layout_matrix_w_pn[2,2] <- 1
  layout_matrix_w_pn[3,3] <- 2

  grob <- gridExtra::arrangeGrob(
    grobs = grid::gList(grob, page_number_grob),
    heights = grid::unit(
      c(padding, height, padding*pn_pad_pct, (padding - padding*pn_pad_pct)),
      'mm'),
    widths = grid::unit(
      c(padding, width, padding*pn_pad_pct, (padding - padding*pn_pad_pct)),
      'mm'),
    layout_matrix = layout_matrix_w_pn)

  grob

}
