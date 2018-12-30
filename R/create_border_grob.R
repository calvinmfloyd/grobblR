
create_border_grob <- function(border_color, border_width, border_sides){

  def_vals <- list(
    border_color = 'gray40',
    border_width = 2)

  border_color <- ifelse(is.null(border_color), def_vals$border_color, border_color)
  border_width <- ifelse(is.null(border_width), def_vals$border_width, border_width)

  cell_border_gs <- grid::gList()
  borders_split <- unlist(strsplit(border_sides, split = ', ', fixed = T))

  if(length(borders_split) > 0){
    for(side in 1:length(borders_split)){
      cell_border_gs <- grid::gList(
        cell_border_gs,
        grid::segmentsGrob(
          x0 = grid::unit(ifelse(borders_split[side] %in% c("right"), 1, 0), "npc"),
          y0 = grid::unit(ifelse(borders_split[side] %in% c("top"), 1, 0), "npc"),
          x1 = grid::unit(ifelse(borders_split[side] %in% c("top", "bottom", "right"), 1, 0), "npc"),
          y1 = grid::unit(ifelse(borders_split[side] %in% c("left", "right", "top"), 1, 0), "npc"),
          gp = grid::gpar(col = border_color, lwd = border_width)))
    }
  }

  cell_border_gs

}
