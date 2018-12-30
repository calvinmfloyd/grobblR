#' Converts a data.frame/matrix to a grob, with flexible aesthetics.
#'
#' @param df The data.frame/matrix to be converted to a grob.
#' @param aes_list A list which contains aesthetic parameters for the matrix grob. Possible aesthetic elements for matrices are:
#' \itemize{
#' \item \code{background_alpha} - Controls the background alpha/opacity of the elements of the matrix. Values are used in grid::gpar(). Default is 1.0.
#' \item \code{background_color} - Controls the background color of the elements of the matrix. If the matrix has no rownames or colnames, the default is white. If the matrix has rownames or colnames, the default is white-gray90 on every odd-even row.
#' \item \code{borders} - Controls the borders of the elements of the matrix. The input is a string with the possible words "top", "bottom", "left", "right" separated by commas. For example, "top, left, right" will put borders on the top, left and right side of the grid cell, but not the bottom. Default is "", or no borders.
#' \item \code{border_color} - Controls the color of the selected borders. Default is gray40.
#' \item \code{border_width} - Controls the line width density/thickness of the selected borders. Values are used in grid::gpar(). Default is 4.
#' \item \code{cell_sep} - Controls the amount of padding around each cell in mm. Default is 1 mm.
#' \item \code{color_binary_cut_off} - A cut-off value which the binary color gradient will be applied to. Default is 0.
#' \item \code{color_binary_high} - The color of the binary color gradient if the numeric element is greater than the \code{color_binary_cut_off}. Default is green.
#' \item \code{color_binary_low} - The color of the binary color gradient if the numeric element is less than the \code{color_binary_cut_off}. Default is red.
#' \item \code{color_binary_equal} - The color of the binary color gradient if the numeric element is equal to the \code{color_binary_cut_off}. Default is gray.
#' \item \code{color_gradient_binary} - A TRUE/FALSE value which signifies if a binary color gradient should be applied to the \code{color_gradient_columns}.
#' \item \code{color_gradient_columns} - Controls the columns which a color gradient scale will be applied to. Integer values denoting the column numbers. Can only be applied to columns with all numeric values.
#' \item \code{color_gradient_max} - The high color for the gradual color gradient. Default is green.
#' \item \code{color_gradient_mid} - The middle color for the gradual color gradient. Default is yellow.
#' \item \code{color_gradient_min} - The low color for the gradual color gradient. Default is red.
#' \item \code{column_widths} - If automatic column widths are not desired, the user can provide a vector of widths for each column in the matrix in mm.
#' \item \code{font_face} - Controls the font face of the elements of the matrix (i.e. bold, italic, etc.). Values are used in grid::gpar(). Default for table elements is normal, or 1. Default for column name elements is bold and italic, or 4.
#' \item \code{group_elements} - Controls whether same, adjacent elements within the matrix should be grouped together into one single grid. A TRUE/FALSE value, with the default being FALSE.
#' \item \code{round_rect_radius} - Controls the radius of the corners of the rectangles matrix text is laid on top of.
#' \item \code{row_heights} - If equal row heights are not desired, the user can provide a vector of heights for each row in the matrix in mm.
#' \item \code{text_align} - Controls where the text in each grid cell will be centered around, horizontally. A numeric value between 0 and 1, with 0 being all the way to the left of the grid cell, and 1 being all the way to the right of the grid cell. Default is 0.5. Can also input 'left', 'right' or 'center', which will also make edits to \code{text_just} to make the text completely left-justified, right-justified or centered, respectively.
#' \item \code{text_angle} - Controls the text angle of the text within the matrix. A numeric value in degrees, with the default being 0.
#' \item \code{text_cex} - Controls the size of the text within the matrix. Default is automatic text sizing based on the length of the elements within the matrix, the row heights and the column widths.
#' \item \code{text_color} - Controls the text color of the elements of the matrix. Default for table elements and row names is black, and a gray-blue color for column names.
#' \item \code{text_font} - Controls the font family of the text within the matrix. Default is sans.
#' \item \code{text_just} - Controls the horizontal justification of the text in the matrix. A numeric value between 0 and 1, with 0 being left justification and 1 being right justification. Default is 0.5, or center justification. Can also input 'left', 'right' or 'center', which will also make edits to \code{text_align} to make the text completely left-justified, right-justified or centered, respectively.
#' \item \code{text_v_align} - Controls where the text in each grid cell will be centered around, vertically. A numeric value between 0 and 1, with 0 being all the way to the bottom of the grid cell, and 1 being all the way to the top of the grid cell. Default is 0.5. Can also input 'top', 'bottom' or 'center', which will also make edits to \code{text_v_just} to make the text completely top-justified, bottom-justified or centered, respectively.
#' \item \code{text_v_just} - Controls the vertical justification of the text in the matrix. A numeric value between 0 and 1, with 0 being bottom justification and 1 being top justification. Default is 0.5, or center justification. Can also input 'top', 'bottom' or 'center', which will also make edits to \code{text_v_align} to make the text completely top-justified, bottom-justified or centered, respectively.
#' }
#' @param m_type A integer value which indicates what the default aesthetics of the table will be. Default is 1. The possible options:
#' \enumerate{
#' \item Plain theme.
#' \item Table theme.
#' \item Column name theme.
#' \item Row name theme.
#' \item Title theme.
#' }
#' @param height A numeric value designating the total height of the matrix grob in mm.
#' @param width A numeric value designating the total width of the matrix grob in mm.
#' @param text_cex_adj A numeric value used to adjust the automatic text cex sizing.
#' @return A grob of df, with the corresponding aesthetics.
#' @export

grob_matrix <- function(df, aes_list = ga_list(), m_type = 1, height = numeric(), width = numeric(), text_cex_adj = 0.2){

  in_to_mm <- function(x) x*25.4

  decimal_places <- function(x) {
    if((x %% 1) != 0){
      return(nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]]))
    } else {
      return(0)
    }
  }

  stopifnot(!is.null(nrow(df)), !is.null(ncol(df)))
  stopifnot(!length(height) == 0, !length(width) == 0)
  stopifnot(m_type %in% c(1, 2, 3, 4, 5))

  m_type_desc <- data.frame(
    m_type = c(1, 2, 3, 4, 5)
    ,desc = c('matrix', 'table', 'column names', 'row names', 'title'))

  df_fit <- df
  if(m_type == 3) df <- matrix(colnames(df), nrow = 1)

  nr <- nrow(df)
  nc <- ncol(df)

  # Adding in default values (non-matrices) if they are missing ----
  def_vals_non_matrices <- list(
    group_elements = FALSE,
    cell_sep = 1,
    color_gradient_columns = numeric(),
    color_gradient_binary = FALSE,
    color_binary_cut_off = 0,
    color_binary_high = '#63BE7B',
    color_binary_low = '#F8696B',
    color_binary_equal = 'gray90',
    color_gradient_max = '#63BE7B',
    color_gradient_mid = '#FFEB84',
    color_gradient_min = '#F8696B')

  for(val_name in names(def_vals_non_matrices)){

    if(length(aes_list[[val_name]]) > 1 & val_name != 'color_gradient_columns') stop(sprintf(
        "The %s in aes_list has a length of %d, but must be a single value.",
        val_name,
        length(aes_list[[val_name]])),
      call. = FALSE)

    if(length(aes_list[[val_name]]) == 0){
      aes_list[[val_name]] <- def_vals_non_matrices[[val_name]]
    }
  }
  # ----

  def_vals_matrices <- list(
    font_face = c(1, 1, 2, 3, 2),
    background_color = c('white', 'white', 'white', 'white', 'gray50'),
    background_alpha = c(1, 1, 1, 1, 1),
    borders = c('', '', 'bottom', '', ''),
    text_color = c('black', 'black', 'gray40', 'black', 'white'),
    text_align = c(0.5, 0.5, 0.5, 0.5, 0.5),
    text_v_align = c(0.5, 0.5, 0.5, 0.5, 0.5),
    text_just = c(0.5, 0.5, 0.5, 0.5, 0.5),
    text_v_just = c(0.5, 0.5, 0.5, 0.5, 0.5),
    text_font = c('sans', 'sans', 'sans', 'sans', 'sans', 'sans'),
    border_color = c('gray40', 'gray40', 'gray40', 'gray40', 'gray40'),
    # text_angle = c(0, 0, 0, 0, 0),
    border_width = c(4, 4, 1, 4, 4),
    round_rect_radius = c(0.0, 0.0, 0.0, 0.0, 0.2))

  def_orig_vals_list <- c(def_vals_matrices, def_vals_non_matrices)

  # Making adjustments to text_cex, if need be ----
  adjust_cex <- length(aes_list$text_cex) == 0 &
    (length(height) == 1 & length(aes_list$row_heights) == 0) &
    (length(width) == 1 & length(aes_list$column_widths) == 0)

  if(!adjust_cex){

    def_text_cex <- 2

  } else {

    cex_vals <- seq(0.5, 15, 0.1)
    rh <- height/nr - 2*aes_list$cell_sep

    column_props <- apply(
      rbind(colnames(df_fit), df_fit), 2,
      function(x) max(graphics::strwidth(c(x), units = 'in')))
    column_props <- column_props/sum(column_props)

    cw <- width*column_props - 2*aes_list$cell_sep

    widest_elements <- apply(
      rbind(colnames(df_fit), df_fit), 2,
      function(x) x[graphics::strwidth(x, units = 'in') == max(graphics::strwidth(c(x), units = 'in'))]
    )

    tallest_element <- c(df_fit)[
      which(graphics::strheight(c(df_fit), units = 'in') == max(graphics::strheight(c(df_fit), units = 'in')))[1]]

    poss_width_cex_vals <- sapply(
      1:nc,
      function(i){
        x <- sapply(cex_vals, function(c) in_to_mm(graphics::strwidth(widest_elements[i], cex = c, units = 'in')))
        if(sum(x <= cw[i]) == 0){
          warning(sprintf(
            "Text in %s is too wide for allotted width. Using minimum text cex value.",
            m_type_desc$desc[m_type_desc$m_type == m_type]),
            call. = F)
          return(min(cex_vals))
        } else {
          cex_vals[max(which(x <= cw[i]))]
        }
        })

    poss_str_heights <- sapply(
      cex_vals,
      function(c) in_to_mm(graphics::strheight(tallest_element, cex = c, units = 'in')))

    if(sum(poss_str_heights <= rh) == 0){
      warning(sprintf(
        "Text in %s is too tall for allotted height. Using minimum text cex value.",
        m_type_desc$desc[m_type_desc$m_type == m_type]),
        call. = F)
      max_str_height <- min(cex_vals)
    } else {
      max_str_height <- cex_vals[max(which(poss_str_heights <= rh))]
    }

    def_text_cex <- min(c(max(poss_width_cex_vals), max_str_height))
    def_text_cex <- def_text_cex - text_cex_adj*def_text_cex

  }
  # ----

  # Adding in default values (matrices) if they are missing ----

  def_vals_matrices$text_cex <- rep(def_text_cex, length(def_vals_matrices[[1]]))
  background_color_not_inputted <- all(dim(aes_list$background_color) == 0)

  for(val_name in names(def_vals_matrices)){

    def_vals_matrices[[val_name]][m_type] <- ifelse(
      !is.null(aes_list[[val_name]]),
        ifelse(
          all(dim(aes_list[[val_name]]) %in% 1),
          aes_list[[val_name]][1,1],
          def_vals_matrices[[val_name]][m_type]),
      def_vals_matrices[[val_name]][m_type])

    if(all(dim(aes_list[[val_name]]) <= 1)){
      aes_list[[val_name]] <- matrix(def_vals_matrices[[val_name]][m_type], nrow = nr, ncol = nc)
    } else {
      if(nrow(aes_list[[val_name]]) != nr | ncol(aes_list[[val_name]]) != nc) stop(sprintf(
        "The dimensions of %s in aes_list do not match the dimension of the matrix its linked to.", val_name),
        .call = FALSE)
    }
  }

  if(background_color_not_inputted & m_type %in% c(2,4)) aes_list[['background_color']][rep(c(F,T), length = nr),] <- 'gray95'

  if(length(aes_list[['color_gradient_columns']]) > 0 &
     aes_list[['color_gradient_binary']] %in% FALSE &
     m_type %in% c(1,2)){

    crp.f <- grDevices::colorRampPalette(
      c(aes_list[['color_gradient_min']],
        aes_list[['color_gradient_mid']],
        aes_list[['color_gradient_max']]))

    for(cc in aes_list[['color_gradient_columns']]){

      num_vals <- as.numeric(df[,cc])
      range <- (max(num_vals) - min(num_vals))
      dp <- decimal_places(range)
      range <- range*10^dp
      crp <- crp.f(range + 1)
      val_vec <- (num_vals - min(num_vals))*10^dp + 1
      aes_list[['background_color']][,cc] <- crp[val_vec]
      }

    } else if (aes_list[['color_gradient_binary']] %in% TRUE & m_type %in% c(1,2)){

      for(cc in aes_list[['color_gradient_columns']]){

        num_vals <- as.numeric(df[,cc])
        aes_list[['background_color']][,cc][num_vals < aes_list[['color_binary_cut_off']]] <- aes_list[['color_binary_low']]
        aes_list[['background_color']][,cc][num_vals > aes_list[['color_binary_cut_off']]] <- aes_list[['color_binary_high']]
        aes_list[['background_color']][,cc][num_vals == aes_list[['color_binary_cut_off']]] <- aes_list[['color_binary_equal']]
        }
    }

  # ----

  # Adjustments to text_just, text_align, text_v_just, text_v_align ----

  for(val_name in c('text_just', 'text_align')){
    if(!any(is.numeric(aes_list[[val_name]]), all(aes_list[[val_name]] %in% c('left', 'right', 'center')))){
      stop(paste0(
        sprintf("The %s in aes_list must either be a numeric value (usually between 0 and 1), ", val_name),
        "or a character value in ('left', 'right', 'center'). Also, do not mix character values and ",
        "numeric values."),
        call. = FALSE)
    }
  }

  al_text_just <- aes_list[['text_just']]
  al_text_align <- aes_list[['text_align']]

  aes_list[['text_just']][al_text_just %in% 'left' | al_text_align %in% 'left'] <- 0
  aes_list[['text_align']][al_text_just %in% 'left' | al_text_align %in% 'left'] <- 0
  aes_list[['text_just']][al_text_just %in% 'right' | al_text_align %in% 'right'] <- 1
  aes_list[['text_align']][al_text_just %in% 'right' | al_text_align %in% 'right'] <- 1
  aes_list[['text_just']][al_text_just %in% 'center' | al_text_align %in% 'center'] <- 0.5
  aes_list[['text_align']][al_text_just %in% 'center' | al_text_align %in% 'center'] <- 0.5

  aes_list[['text_just']] <- matrix(as.numeric(aes_list[['text_just']]), nrow = nr)
  aes_list[['text_align']] <- matrix(as.numeric(aes_list[['text_align']]), nrow = nr)

  ####

  al_text_v_just <- aes_list[['text_v_just']]
  al_text_v_align <- aes_list[['text_v_align']]

  for(val_name in c('text_v_just', 'text_v_align')){
    if(!any(is.numeric(aes_list[[val_name]]), all(aes_list[[val_name]] %in% c('top', 'bottom', 'center')))){
      stop(paste0(
        sprintf("The %s in aes_list must either be a numeric value (usually between 0 and 1), ", val_name),
        "or a character value in ('top', 'bottom', 'center'). Also, do not mix character values and ",
        "numeric values."),
        call. = FALSE)
    }
  }

  aes_list[['text_v_just']][al_text_v_just %in% 'bottom' | al_text_v_align %in% 'bottom'] <- 0
  aes_list[['text_v_align']][al_text_v_just %in% 'bottom' | al_text_v_align %in% 'bottom'] <- 0
  aes_list[['text_v_just']][al_text_v_just %in% 'top' | al_text_v_align %in% 'top'] <- 1
  aes_list[['text_v_align']][al_text_v_just %in% 'top' | al_text_v_align %in% 'top'] <- 1
  aes_list[['text_v_just']][al_text_v_just %in% 'center' | al_text_v_align %in% 'center'] <- 0.5
  aes_list[['text_v_align']][al_text_v_just %in% 'center' | al_text_v_align %in% 'center'] <- 0.5

  aes_list[['text_v_just']] <- matrix(as.numeric(aes_list[['text_v_just']]), nrow = nr)
  aes_list[['text_v_align']] <- matrix(as.numeric(aes_list[['text_v_align']]), nrow = nr)

  # ----

  # Aesthetic value type checks before we start creating the grobs themselves ----

  for(val_name in names(def_orig_vals_list)){
    if(!class(c(aes_list[[val_name]])) %in% class(def_orig_vals_list[[val_name]])){
      stop(sprintf(
        "The class of %s in aes_list must be %s.", val_name, class(def_orig_vals_list[[val_name]])),
        call. = FALSE)
    }
  }

  # ----

  # Creating each of our mini grobs that will compose the grob_matrix ----
  raw_grobs <- grid::gList()
  for(j in 1:nc){
    for(i in 1:nr){

      rect_grob <- grid::roundrectGrob(
        r = grid::unit(aes_list$round_rect_radius, 'snpc')
        ,gp = grid::gpar(
          fill = aes_list$background_color[i,j],
          col = aes_list$background_color[i,j],
          alpha = aes_list$background_alpha[i,j]))

      text_grob <- grid::textGrob(
        df[i,j],
        x = grid::unit(aes_list$text_align[i,j], "npc"),
        y = grid::unit(aes_list$text_v_align[i,j], "npc"),
        hjust = aes_list$text_just[i,j],
        vjust = aes_list$text_v_just[i,j],
        # rot = aes_list$text_angle[i,j],
        gp = grid::gpar(
          fontface = aes_list$font_face[i,j],
          fontfamily = aes_list$text_font[i,j],
          cex = aes_list$text_cex[i,j],
          col = aes_list$text_color[i,j]))

      borders_split <- unlist(strsplit(aes_list$borders[i,j], split = ', ', fixed = T))
      if(length(borders_split) > 0){

        cell_border_gs <- create_border_grob(
          border_color = aes_list$border_color[i,j],
          border_width = aes_list$border_width[i,j],
          border_sides = aes_list$borders[i,j])

        raw_grobs <- grid::gList(raw_grobs, grid::grobTree(rect_grob, cell_border_gs, text_grob))

      } else {

        raw_grobs <- grid::gList(raw_grobs, grid::grobTree(rect_grob, text_grob))

      }
    }}

  # ----

  # Creating the layout matrix, dependent on if we're grouping elements ----
  if(aes_list$group_elements){
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

  if(length(aes_list$row_heights) == 0 & length(height) == 0){

    tmp_row_heights <- c()
    for(i in 1:nr){
      ind_row_heights <- mapply(
        function(text, cex, face, fam){
          in_to_mm(graphics::strheight(text, units = 'in', cex = cex, family = fam, font = face))
        },
        df[i,],
        aes_list$text_cex[i,],
        aes_list$font_face[i,],
        aes_list$text_font[i,])
      tmp_row_heights <- c(tmp_row_heights, max(ind_row_heights) + 2*aes_list$cell_sep)
    }
    aes_list$row_heights <- rep(max(tmp_row_heights), nr)

  } else if(length(height) == 1 & length(aes_list$row_heights) != nr){

    aes_list$row_heights <- rep(height/nr, nr)

  }

  if(length(aes_list$column_widths) == 0 & length(width) == 0){

    tmp_column_widths <- c()
    for(i in 1:nc){
      ind_column_widths <- mapply(
        function(text, cex, face, fam){
          in_to_mm(graphics::strwidth(text, units = 'in', cex = cex, family = fam, font = face))},
        df[,i],
        aes_list$text_cex[,i],
        aes_list$font_face[,i],
        aes_list$text_font[,i])
      tmp_column_widths <- c(tmp_column_widths, max(ind_column_widths) + 2*aes_list$cell_sep)
    }
    aes_list$column_widths <- tmp_column_widths

  } else if(adjust_cex){

    aes_list$column_widths <- cw

  } else if(length(width) == 1 & length(aes_list$column_widths) != nc){

    aes_list$column_widths <- rep(width/nc, nc)

  }
  # ----

  first_element_indices <- unlist(lapply(unique(c(layout_matrix)), function(x) min(which(c(layout_matrix) == x))))

  gridExtra::arrangeGrob(
    grobs = raw_grobs[first_element_indices],
    heights = grid::unit(aes_list$row_heights, 'mm'),
    widths = grid::unit(aes_list$column_widths, 'mm'),
    layout_matrix = layout_matrix)

}
