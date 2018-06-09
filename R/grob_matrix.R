#' Converts a data.frame/matrix to a grob, with flexible aesthetics. Used within the grobblR::convert_to_grob() function.
#'
#' @param df The data.frame/matrix to be converted to a grob.
#' @param gm_list A list which contains aesthetic parameters for the matrix grob.
#' @param m_type A integer value which indicates what the default aesthetics of the table will be. The possible options:
#' \enumerate{
#' \item 1 - Plain theme.
#' \item 2 - Table theme.
#' \item 3 - Column name theme.
#' \item 4 - Row name theme.
#' }
#' @param tot_height A numeric value designating the total height of the matrix grob in mm.
#' @param tot_width A numeric value designating the total width of the matrix grob in mm.
#' @param txt_cex_adj A numeric value used to adjust the automatic text cex sizing.
#' @return A grob of df, with the corresponding aesthetics.
#' @export

grob_matrix <- function(df, gm_list, m_type = 1, tot_height = numeric(), tot_width = numeric(), txt_cex_adj = 0.25){

  in_to_mm <- function(x) x*25.4

  stopifnot(!is.null(nrow(df)), !is.null(ncol(df)))
  stopifnot(!length(tot_height) == 0, !length(tot_width) == 0)
  stopifnot(m_type %in% c(1, 2, 3, 4))

  nr <- nrow(df)
  nc <- ncol(df)

  # Adding in default values (non-matrices) if they are missing ----
  def_vals_non_matrices <- list(
    group_elements = FALSE,
    cell_sep = 1)

  for(val_name in names(def_vals_non_matrices)){
    if(length(gm_list[[val_name]]) == 0){
      gm_list[[val_name]] <- def_vals_non_matrices[[val_name]]
    }
  }
  # ----

  # Making adjustments to txt_cex, if need be ----
  adjust_cex <- length(gm_list$txt_cex) == 0 &
    (length(tot_height) == 1 & length(gm_list$row_heights) == 0) &
    (length(tot_width) == 1 & length(gm_list$col_widths) == 0)

  if(!adjust_cex){

    def_txt_cex <- 2

  } else {

    cex_vals <- seq(0.01, 20, 0.01)
    rh <- tot_height/nr - 2*gm_list$cell_sep
    cw <- tot_width/nc - 2*gm_list$cell_sep
    widest_element <- c(df)[which(graphics::strwidth(c(df), units = 'in') == max(graphics::strwidth(c(df), units = 'in')))[1]]
    tallest_element <- c(df)[which(graphics::strheight(c(df), units = 'in') == max(graphics::strheight(c(df), units = 'in')))[1]]

    poss_str_widths <- sapply(cex_vals, function(c) in_to_mm(graphics::strwidth(widest_element, cex = c, units = 'inches')))
    poss_str_heights <- sapply(cex_vals, function(c) in_to_mm(graphics::strheight(tallest_element, cex = c, units = 'inches')))
    def_txt_cex <- min(c(cex_vals[max(which(poss_str_widths <= cw))],cex_vals[max(which(poss_str_heights <= rh))]))
    def_txt_cex <- def_txt_cex - txt_cex_adj*def_txt_cex

  }
  # ----

  # Adding in default values (matrices) if they are missing ----
  def_vals_matrices <- list(
    fnt_face = c(1, 1, 4, 3),
    bg_color = c('white', 'white', 'white', 'white'),
    bg_alpha = c(1, 1, 1, 1),
    borders = c('', '', '', ''),
    txt_color = c('black', 'black', '#3A70A6', 'black'),
    txt_align = c(0.5, 0.5, 0.5, 0.5),
    txt_v_align = c(0.5, 0.5, 0.5, 0.5),
    txt_just = c(0.5, 0.5, 0.5, 0.5),
    txt_v_just = c(0.5, 0.5, 0.5, 0.5),
    txt_cex = rep(def_txt_cex, 4),
    txt_font = rep('sans', 4),
    border_color = c('gray40', 'gray40', '#3A70A6', 'gray40'),
    txt_angle = rep(0, 4),
    border_width = rep(4, 4))

  bg_color_not_inputted <- all(dim(gm_list$bg_color) == 0)

  for(val_name in names(def_vals_matrices)){
    def_vals_matrices[[val_name]][m_type] <- ifelse(
      all(is.null(gm_list[[val_name]])) | all(dim(gm_list[[val_name]]) == 1),
      gm_list[[val_name]][1,1],
      def_vals_matrices[[val_name]][m_type])

    if(all(dim(gm_list[[val_name]]) <= 1)){
      gm_list[[val_name]] <- matrix(def_vals_matrices[[val_name]][m_type], nrow = nr, ncol = nc)
    } else {
      stopifnot(nrow(gm_list[[val_name]]) == nr, ncol(gm_list[[val_name]]) == nc)
    }
  }

  if(bg_color_not_inputted & m_type %in% c(2,4)) gm_list[['bg_color']][rep(c(F,T), length = nr),] <- 'gray90'

  # ----

  raw_grobs <- grid::gList()
  for(j in 1:nc){
    for(i in 1:nr){

      rect_grob <- grid::rectGrob(
        gp = grid::gpar(
          fill = gm_list$bg_color[i,j],
          col = gm_list$bg_color[i,j],
          alpha = gm_list$bg_alpha[i,j]))

      text_grob <- grid::textGrob(
        df[i,j],
        x = grid::unit(gm_list$txt_align[i,j], "npc"),
        y = grid::unit(gm_list$txt_v_align[i,j], "npc"),
        hjust = gm_list$txt_just[i,j],
        vjust = gm_list$txt_v_just[i,j],
        rot = gm_list$txt_angle[i,j],
        gp = grid::gpar(
          fontface = gm_list$fnt_face[i,j],
          fontfamily = gm_list$txt_font[i,j],
          cex = gm_list$txt_cex[i,j],
          col = gm_list$txt_color[i,j]))

      cell_border_gs <- grid::gList()
      borders_split <- unlist(strsplit(gm_list$borders[i,j], split = ', ', fixed = TRUE))

      if(length(borders_split) > 0){
        for(side in 1:length(borders_split)){

          cell_border_gs <- grid::gList(
            cell_border_gs,
            grid::segmentsGrob(
              x0 = grid::unit(ifelse(borders_split[side] %in% c("right"), 1, 0), "npc"),
              y0 = grid::unit(ifelse(borders_split[side] %in% c("top"), 1, 0), "npc"),
              x1 = grid::unit(ifelse(borders_split[side] %in% c("top", "bottom", "right"), 1, 0), "npc"),
              y1 = grid::unit(ifelse(borders_split[side] %in% c("left", "right", "top"), 1, 0), "npc"),
              gp = grid::gpar(col = gm_list$border_color[i,j], lwd = gm_list$border_width[i,j])))
        }

        raw_grobs <- grid::gList(raw_grobs, grid::grobTree(rect_grob, cell_border_gs, text_grob))

      } else {

        raw_grobs <- grid::gList(raw_grobs, grid::grobTree(rect_grob, text_grob))

      }
    }}

  # ----
  if(gm_list$group_elements){
    ue <- unique(c(as.matrix(df)))
    level_df <- data.frame(element = ue, level = 1:length(ue), stringsAsFactors = F)
    matched_df <- matrix(level_df$level[match(df, level_df$element)], nrow = nr)
    m.v <- c(matched_df)
    l.v <- rep(0, nr*nc)
    i.v <- 1:(nr*nc)
    for(i in i.v){
      r <- (i-1)%/%nr
      like_indices <- which(m.v == m.v[i])
      left_index <- i.v[i.v == (i-nr)]
      top_bottom_indices <- i.v[(i.v-1)%/%nr == r & i.v %in% c(i-1, i+1)]
      adj_indices <- c(left_index, top_bottom_indices)

      if(length(like_indices) == 1 | i == min(like_indices) | all(!like_indices %in% adj_indices)){

        l.v[i] <- ifelse(i == 1, 1, max(cumsum(l.v)[1:(i-1)]) - cumsum(l.v)[i-1] + 1)

      } else if (i != min(like_indices) & (i+1) %in% like_indices & !(i-nr) %in% like_indices){

        l.v[i] <- cumsum(l.v)[min(like_indices)] - cumsum(l.v)[i-1]

      } else if ((i-nr) %in% like_indices){

        l.v[i] <- cumsum(l.v)[i-nr] - cumsum(l.v)[i-1]

      }
    }
    layout_matrix <- matrix(cumsum(l.v), nrow = nr)

  } else {

    layout_matrix <- matrix(1:(nr*nc), nrow = nr, ncol = nc)

  }
  # ----

  if(length(gm_list$row_heights) == 0 & length(tot_height) == 0){

    tmp_row_heights <- c()
    for(i in 1:nr){
      ind_row_heights <- mapply(
        function(txt, cex, face, fam){
          in_to_mm(graphics::strheight(txt, units = 'inches', cex = cex, family = fam, font = face))
        },
        df[i,],
        gm_list$txt_cex[i,],
        gm_list$fnt_face[i,],
        gm_list$txt_font[i,])
      tmp_row_heights <- c(tmp_row_heights, max(ind_row_heights) + 2*gm_list$cell_sep)
    }
    gm_list$row_heights <- rep(max(tmp_row_heights), nr)

  } else if(length(tot_height) == 1 & length(gm_list$row_heights) != nr){

    gm_list$row_heights <- rep(tot_height/nr, nr)

  }

  if(length(gm_list$col_widths) == 0 & length(tot_width) == 0){

    tmp_col_widths <- c()
    for(i in 1:nc){
      ind_col_widths <- mapply(
        function(txt, cex, face, fam){
          in_to_mm(graphics::strwidth(txt, units = 'inches', cex = cex, family = fam, font = face))
        },
        df[,i],
        gm_list$txt_cex[,i],
        gm_list$fnt_face[,i],
        gm_list$txt_font[,i])
      tmp_col_widths <- c(tmp_col_widths, max(ind_col_widths) + 2*gm_list$cell_sep)
    }
    gm_list$col_widths <- tmp_col_widths

  } else if(length(tot_width) == 1 & length(gm_list$col_widths) != nc){

    gm_list$col_widths <- rep(tot_width/nc, nc)

  }
  # ----
  first_element_indices <- unlist(lapply(unique(c(layout_matrix)), function(x) min(which(c(layout_matrix) == x))))

  gridExtra::arrangeGrob(
    grobs = raw_grobs[first_element_indices],
    heights = grid::unit(gm_list$row_heights, 'mm'),
    widths = grid::unit(gm_list$col_widths, 'mm'),
    layout_matrix = layout_matrix)

}
