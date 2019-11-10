#' Converts a data.frame/matrix to a grob, with flexible aesthetics.
#'
#' @param df The data.frame/matrix to be converted to a grob.
#' @param aes_list The list outputted by \code{\link{ga_list}} which gives the data.frame/matrix grob its aesthetics.
#' @param m_type A integer value which indicates what the default aesthetics of the table will be. Default is 1. The possible options:
#' \enumerate{
#' \item Plain theme.
#' \item Table theme.
#' \item Column name theme.
#' \item Caption theme.
#' \item Title theme.
#' }
#' @param height A numeric value designating the total height of the matrix grob in mm.
#' @param width A numeric value designating the total width of the matrix grob in mm.
#' @param text_cex_adj A numeric value used to adjust the automatic text cex sizing.
#' @return A grob of df, with the corresponding aesthetics.
#' @export

grob_matrix = function(df,
                       aes_list = ga_list(),
                       m_type = 1,
                       height = numeric(),
                       width = numeric(),
                       padding = numeric(),
                       units = c('mm', 'cm', 'inches'),
                       text_cex_adj = 0.2){

  stopifnot(!is.null(nrow(df)), !is.null(ncol(df)))
  stopifnot(!length(height) == 0, !length(width) == 0)
  stopifnot(m_type %in% 1:5)
  units = match.arg(units)

  m_type_desc = data.frame(
    m_type = c(1, 2, 3, 4, 5),
    desc = c('matrix', 'table', 'column names', 'caption', 'title')
    )

  df_fit = df
  if(m_type == 3) df = matrix(colnames(df), nrow = 1)

  nr = nrow(df)
  nc = ncol(df)

  # Adding in default values (non-matrices) if they are missing ----
  def_vals_non_matrices = list(
    padding_p = 0.0025,
    color_gradient_columns = numeric(),
    color_gradient_binary = FALSE,
    color_binary_cut_off = 0,
    color_binary_high = '#63BE7B',
    color_binary_low = '#F8696B',
    color_binary_equal = 'gray90',
    color_gradient_max = '#63BE7B',
    color_gradient_mid = '#FFEB84',
    color_gradient_min = '#F8696B'
    )

  for(val_name in names(def_vals_non_matrices)){

    if (length(aes_list[[val_name]]) > 1 & val_name != 'color_gradient_columns') {
      
      stop(
        sprintf("The %s in aes_list has a length of %d, but must be a single value.", val_name, length(aes_list[[val_name]])),
        call. = FALSE
        )
    }

    if(length(aes_list[[val_name]]) == 0){
      
      aes_list[[val_name]] = def_vals_non_matrices[[val_name]]
      
    }
  }

  padding = (aes_list$padding_p[1])*width
  
  def_vals_matrices = list(
    font_face = c(1, 1, 2, 3, 4),
    background_color = c('white', 'white', 'white', 'white', 'white'),
    background_alpha = c(1, 1, 1, 1, 1),
    border_sides = c('', '', 'bottom', '', ''),
    text_color = c('black', 'black', 'gray40', 'gray40', 'gray40'),
    text_align = c(0.5, 0.5, 0.5, 0.0, 0.0),
    text_v_align = c(0.5, 0.5, 0.5, 0.5, 0.5),
    text_just = c(0.5, 0.5, 0.5, 0.0, 0.0),
    text_v_just = c(0.5, 0.5, 0.5, 0.5, 0.5),
    text_font = c('sans', 'sans', 'sans', 'sans', 'sans', 'sans'),
    text_rot = c(0.0, 0.0, 0.0, 0.0, 0.0),
    border_color = c('gray40', 'gray40', 'gray40', 'gray40', 'gray40'),
    border_width = c(1, 1, 1, 1, 1),
    round_rect_radius = c(0.0, 0.0, 0.0, 0.0, 0.2),
    group_elements = c(FALSE, FALSE, FALSE, FALSE, FALSE)
    )

  def_orig_vals_list = c(def_vals_matrices, def_vals_non_matrices)

  # Figuring out the column proportions ----
 
  if (length(width) == 1 & length(aes_list[['column_widths']]) == nc) {
    
    column_props = aes_list[['column_widths']]
    
  } else if (length(width) == 1 & length(aes_list[['column_widths_p']]) == nc) {
  
    column_props = aes_list[['column_widths_p']]/sum(aes_list[['column_widths_p']])
    
  } else {
  
    column_props = apply(
      X = rbind(colnames(df_fit), df_fit),
      MARGIN = 2,
      FUN = function(x) max(graphics::strwidth(c(x), units = 'in'))
      )
    
    if (sum(column_props) == 0) {
      
      column_props = rep(1, length(column_props))
      
    } else {
      
      column_props = column_props/sum(column_props)
      
    }
    
  }
 
  column_widths = width*column_props - 2*padding
  
  # Making adjustments to text_cex, if need be ----
  adjust_cex = length(aes_list$text_cex) == 0 &
    (length(height) == 1 & length(aes_list$row_heights) == 0)

  if(!adjust_cex){

    def_text_cex = 2

  } else {

    optimal_cvs = sapply(
      X = 1:nc, 
      FUN = function(x){
        column_edited = ifelse(df[,x] %in% '', " ", df[,x])
        cex_val_convergence(
          string = paste(column_edited, collapse = '\n'),
          n_lines = nr,
          height = height,
          units = units,
          width = column_widths[x],
          convergence_limit = 0.025,
          sep = '\n'
        )$cex_val
        }
      )
    
    def_text_cex = ifelse(is.null(unlist(optimal_cvs)), 1, min(optimal_cvs))
    def_text_cex = def_text_cex - text_cex_adj*def_text_cex

  }
  
  # Adding in default values (matrices) if they are missing ----

  def_vals_matrices$text_cex = rep(def_text_cex, length(def_vals_matrices[[1]]))
  background_color_not_inputted = all(dim(aes_list$background_color) == 0)

  for(val_name in names(def_vals_matrices)){

    def_vals_matrices[[val_name]][m_type] = ifelse(
      !is.null(aes_list[[val_name]]),
        ifelse(
          all(dim(aes_list[[val_name]]) %in% 1),
          aes_list[[val_name]][1,1],
          def_vals_matrices[[val_name]][m_type]
          ),
      def_vals_matrices[[val_name]][m_type]
      )

    if(all(dim(aes_list[[val_name]]) <= 1)){
      aes_list[[val_name]] = matrix(def_vals_matrices[[val_name]][m_type], nrow = nr, ncol = nc)
    } else {
      if(nrow(aes_list[[val_name]]) != nr | ncol(aes_list[[val_name]]) != nc) stop(sprintf(
        "The dimensions of %s in aes_list do not match the dimension of the matrix its linked to.", val_name),
        .call = FALSE)
    }
  }

  if (background_color_not_inputted & m_type %in% c(2,4)) {
    
    aes_list[['background_color']][rep(c(F,T), length = nr),] = 'gray95'
    
  }

  if(length(aes_list[['color_gradient_columns']]) > 0 &
     aes_list[['color_gradient_binary']] %in% FALSE &
     m_type %in% c(1,2)){
    
      crp.f = grDevices::colorRampPalette(
        c(aes_list[['color_gradient_min']],
          aes_list[['color_gradient_mid']],
          aes_list[['color_gradient_max']])
        )
  
      for(cc in aes_list[['color_gradient_columns']]){
  
        num_vals = as.numeric(df[,cc])
        range = (max(num_vals) - min(num_vals))
        dp = decimal_places(range)
        range = range*10^dp
        crp = crp.f(range + 1)
        val_vec = (num_vals - min(num_vals))*10^dp + 1
        aes_list[['background_color']][,cc] = crp[val_vec]
        }

    } else if (aes_list[['color_gradient_binary']] %in% TRUE & m_type %in% c(1,2)){

      for(cc in aes_list[['color_gradient_columns']]){

        num_vals = as.numeric(df[,cc])
        aes_list[['background_color']][,cc][num_vals < aes_list[['color_binary_cut_off']][1]] = aes_list[['color_binary_low']]
        aes_list[['background_color']][,cc][num_vals > aes_list[['color_binary_cut_off']][1]] = aes_list[['color_binary_high']]
        aes_list[['background_color']][,cc][num_vals == aes_list[['color_binary_cut_off']][1]] = aes_list[['color_binary_equal']]
        }
    }

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

  al_text_just = aes_list[['text_just']]
  al_text_align = aes_list[['text_align']]

  aes_list[['text_just']][al_text_just %in% 'left' | al_text_align %in% 'left'] = 0
  aes_list[['text_align']][al_text_just %in% 'left' | al_text_align %in% 'left'] = 0
  aes_list[['text_just']][al_text_just %in% 'right' | al_text_align %in% 'right'] = 1
  aes_list[['text_align']][al_text_just %in% 'right' | al_text_align %in% 'right'] = 1
  aes_list[['text_just']][al_text_just %in% 'center' | al_text_align %in% 'center'] = 0.5
  aes_list[['text_align']][al_text_just %in% 'center' | al_text_align %in% 'center'] = 0.5

  aes_list[['text_just']] = matrix(as.numeric(aes_list[['text_just']]), nrow = nr)
  aes_list[['text_align']] = matrix(as.numeric(aes_list[['text_align']]), nrow = nr)

  ####

  al_text_v_just = aes_list[['text_v_just']]
  al_text_v_align = aes_list[['text_v_align']]

  for(val_name in c('text_v_just', 'text_v_align')){
    if(!any(is.numeric(aes_list[[val_name]]), all(aes_list[[val_name]] %in% c('top', 'bottom', 'center')))){
      stop(paste0(
        sprintf("The %s in aes_list must either be a numeric value (usually between 0 and 1), ", val_name),
        "or a character value in ('top', 'bottom', 'center'). Also, do not mix character values and ",
        "numeric values."),
        call. = FALSE)
    }
  }

  aes_list[['text_v_just']][al_text_v_just %in% 'bottom' | al_text_v_align %in% 'bottom'] = 0
  aes_list[['text_v_align']][al_text_v_just %in% 'bottom' | al_text_v_align %in% 'bottom'] = 0
  aes_list[['text_v_just']][al_text_v_just %in% 'top' | al_text_v_align %in% 'top'] = 1
  aes_list[['text_v_align']][al_text_v_just %in% 'top' | al_text_v_align %in% 'top'] = 1
  aes_list[['text_v_just']][al_text_v_just %in% 'center' | al_text_v_align %in% 'center'] = 0.5
  aes_list[['text_v_align']][al_text_v_just %in% 'center' | al_text_v_align %in% 'center'] = 0.5

  aes_list[['text_v_just']] = matrix(as.numeric(aes_list[['text_v_just']]), nrow = nr)
  aes_list[['text_v_align']] = matrix(as.numeric(aes_list[['text_v_align']]), nrow = nr)

  # Aesthetic value type checks before we start creating the grobs themselves ----

  for(val_name in names(def_orig_vals_list)){
    if(!class(c(aes_list[[val_name]])) %in% class(def_orig_vals_list[[val_name]])){
      stop(sprintf(
        "The class of %s in aes_list must be %s.", val_name, class(def_orig_vals_list[[val_name]])),
        call. = FALSE
        )
    }
  }

  # Creating each of our mini grobs that will compose the grob_matrix ----
  raw_grobs = grid::gList()
  for(j in 1:nc){
    for(i in 1:nr){

      rect_grob = grid::roundrectGrob(
        r = grid::unit(aes_list$round_rect_radius, 'snpc'),
        gp = grid::gpar(
          fill = aes_list$background_color[i,j],
          col = aes_list$background_color[i,j],
          alpha = aes_list$background_alpha[i,j]
          )
        )

      text_grob = grid::textGrob(
        df[i,j],
        x = grid::unit(aes_list$text_align[i,j], "npc"),
        y = grid::unit(aes_list$text_v_align[i,j], "npc"),
        hjust = aes_list$text_just[i,j],
        vjust = aes_list$text_v_just[i,j],
        rot = aes_list$text_rot[i,j],
        gp = grid::gpar(
          fontface = aes_list$font_face[i,j],
          fontfamily = aes_list$text_font[i,j],
          cex = aes_list$text_cex[i,j],
          col = aes_list$text_color[i,j]
          )
        )

      borders_split = unlist(strsplit(aes_list$border_sides[i,j], split = ', ', fixed = T))
      if(length(borders_split) > 0){

        cell_border_gs = create_border_grob(
          border_color = aes_list$border_color[i,j],
          border_width = aes_list$border_width[i,j],
          border_sides = aes_list$border_sides[i,j]
          )

        raw_grobs = grid::gList(raw_grobs, grid::grobTree(rect_grob, cell_border_gs, text_grob))

      } else {

        raw_grobs = grid::gList(raw_grobs, grid::grobTree(rect_grob, text_grob))

      }
    }}

  # Layout Matrix ----
  layout_matrix = get_layout_matrix(df, aes_list$group_elements)

  if(length(aes_list$row_heights) == 0 & length(height) == 0){

    tmp_row_heights = c()
    for(i in 1:nr){
      ind_row_heights = mapply(
        function(text, cex, face, fam){
          units_convert(
            x = graphics::strheight(text, units = 'in', cex = cex, family = fam, font = face),
            from_units = 'inches',
            to_units = units
            )
        },
        df[i,],
        aes_list$text_cex[i,],
        aes_list$font_face[i,],
        aes_list$text_font[i,]
        )
      tmp_row_heights = c(tmp_row_heights, max(ind_row_heights) + 2*padding)
    }
    aes_list$row_heights = rep(max(tmp_row_heights), nr)

  } else if(length(height) == 1 & length(aes_list$row_heights) != nr){

    aes_list$row_heights = rep(height/nr, nr)

  }

  if (length(aes_list[['column_widths']]) == 0 & length(width) == 0) {

    tmp_column_widths = c()
    for(i in 1:nc){
      ind_column_widths = mapply(
        function(text, cex, face, fam){
          units_convert(
            x = graphics::strwidth(text, units = 'in', cex = cex, family = fam, font = face),
            from_units = 'inches',
            to_units = units
            )
          },
        df[,i],
        aes_list$text_cex[,i],
        aes_list$font_face[,i],
        aes_list$text_font[,i]
        )
      tmp_column_widths = c(tmp_column_widths, max(ind_column_widths) + 2*padding)
    }
    aes_list[['column_widths']] = tmp_column_widths

  } else if (length(aes_list[['column_widths']]) ==  0) {

    aes_list[['column_widths']] = column_widths

  } else if (length(width) == 1 & length(aes_list[['column_widths']]) != nc) {

    aes_list[['column_widths']] = rep(width/nc, nc)

  }
  
  first_element_indices = unlist(lapply(unique(c(layout_matrix)), function(x) min(which(c(layout_matrix) == x))))

  gridExtra::arrangeGrob(
    grobs = raw_grobs[first_element_indices],
    heights = grid::unit(aes_list[['row_heights']], units),
    widths = grid::unit(aes_list[['column_widths']], units),
    layout_matrix = layout_matrix
    )

}
