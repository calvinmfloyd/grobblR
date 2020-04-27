#' Takes in an object, and converts it to a grob based on inputted aesthetics arguments.
#'
#' @param x The object which needs to be converted to a grob. Must be either: 
#' A data.frame/matrix, the file name of a .png image, a character string, a 
#' vector, a ggplot object, or \code{NA} (for an empty grob). 
#' 
#' @param height The numeric height in mm of the desired grob.
#' 
#' @param width The numeric width in mm of the desired grob.
#' 
#' @param aes_list The list outputted by \code{ga_list} which contains elements 
#' to adjust aesthetics to the grob of \code{x}. Different type of grobs have
#' different types of elements of this list which will affect its aesthetics.
#' 
#' Possible elements for character strings, matrices and images can be found in \code{\link{ga_list}}.
#' 
#' @return A grob of x with aesthetics based on the aes_list parameter.

convert_to_grob = function(x,
                           height,
                           width,
                           aes_list = ga_list()) {

  units = 'mm'
  
  if (methods::is(x, "grob_matrix_object")) {
    
    x$height = height
    x$width = width
    x$units = units
    aes_list = x$finish_ga_list
    x = x$current %>% purrr::set_names(NULL)
      
  }
  
  if (methods::is(x, "grob_image_object")) {
    
    aes_list = x$finish_ga_list
    x = x$initial
      
  }
  
  if (all(is.numeric(x), length(x) > 0, is.null(dim(x)))) {
    
    x = convert_to_matrix(x)
    
  }
  
  # Matrix / Data.Frame
  if(is.data.frame(x) | is.matrix(x)){

    x = as.matrix(x)
    matrix_aes_elements = get_matrix_aes_elements()
    colname_present = !is.null(colnames(x))
    height_adj = ifelse(colname_present, 1, 0)

    if (colname_present) {

      colname_ga_list = list()

      for (name in matrix_aes_elements) {

        if (!is.null(aes_list[[name]])) {
          
          colname_ga_list[[name]] = aes_list[[name]]
          
        } else {
          
          colname_ga_list[[name]] = aes_list[[paste0('colname_', name)]]
          
        }
          
      }

      colname_grob = convert_to_matrix_grob(
        df = x,
        m_type = 3,
        aes_list = colname_ga_list,
        height = height/(nrow(x) + 1),
        width = width,
        units = units
        )
    }

    cell_ga_list = list()
    
    for (name in matrix_aes_elements) {
      
        if(!is.null(aes_list[[name]])){
          
          cell_ga_list[[name]] = aes_list[[name]]
          
        } else {
          
          cell_ga_list[[name]] = aes_list[[paste0('cell_', name)]]
          
        }
    }

    cell_grob = convert_to_matrix_grob(
      df = x,
      m_type = ifelse(colname_present, 2, 1),
      aes_list = cell_ga_list,
      height = height - height*height_adj/(nrow(x) + 1),
      width = width,
      units = units
      )

    if (colname_present) {

      grob = gridExtra::arrangeGrob(
        grobs = grid::gList(cell_grob, colname_grob),
        layout_matrix = rbind(c(2), c(1)),
        widths = grid::unit(width, units),
        heights = grid::unit(c(height/(nrow(x) + 1), height - height/(nrow(x) + 1)), units)
        )

    } else {

      grob = cell_grob

    }

  }
  
  # Image 
  else if (ifelse(is.character(x), tools::file_ext(x) %in% 'png', FALSE)) {
    
    if (!file.exists(x)) {
      
      error_msg = glue::glue("The file '{x}' does not exist.")
      stop(error_msg, call. = FALSE)

    }

    grob = convert_to_image_grob(
      img_path = x,
      aes_list = aes_list,
      height = height,
      width = width,
      units = units
      )

  }
  
  # Text 
  else if (ifelse(is.character(x), !grepl('.png', x), FALSE)) {

    if (is.null(aes_list[['text_cex']])) {
      
      n_lines = ifelse(!is.null(aes_list[['n_lines']]), aes_list[['n_lines']], 10000)
      
      lines = cex_val_convergence(
        string = x,
        n_lines = n_lines,
        sep = '\n',
        height = height,
        width = width,
        units = units
        )
      aes_list[['text_cex']] = convert_to_matrix(lines$cex_val)
      
    } else {
      
      lines = line_creator(
        cex_val = aes_list[['text_cex']],
        string = x,
        height = height,
        width = width,
        units = units
        )
      
    }
    
    text_matrix = matrix(lines$lines, ncol = 1)
    grob = convert_to_matrix_grob(
      df = text_matrix,
      aes_list = aes_list,
      height = height,
      width = width,
      units = units
      )

  }
  
  # ggplot object
  else if (ggplot2::is.ggplot(x)) {
    
    png_name = file.path(
      tempdir(),
      sprintf("ggplot_grob_%s.png", format(Sys.time(), '%m_%d_%Y_%H_%M_%S'))
      )
    
    aspect_ratio_multiplier = ifelse(
      length(aes_list$aspect_ratio_multiplier) == 0,
      1,
      aes_list$aspect_ratio_multiplier
      )
    
    ggplot2::ggsave(
      filename = png_name,
      plot = x,
      height = height*aspect_ratio_multiplier,
      width = width*aspect_ratio_multiplier,
      unit = ifelse(units %in% 'inches', 'in', units)
      )
    
    grob = convert_to_image_grob(
      img_path = png_name,
      aes_list = aes_list,
      height = height,
      width = width,
      units = units
      )
    
    remove_file = file.remove(png_name)

  }
  
  # NA (empty grob)
  else if (is.na(x)) {

    grob = grid::rectGrob(
      gp = grid::gpar(col = NA, fill = NA),
      height = grid::unit(height, units),
      width = grid::unit(width, units)
      )

  } else {

    stop(paste0("Object of class ", class(x)," not accepted."), call. = F)

  }

  return(grob)

}

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
#' @param padding A numeric value designating the amount of padding around the matrix cells.
#' @param text_cex_adj A numeric value used to adjust the automatic text cex sizing.
#' @param units millimeters
#' @return A grob of df, with the corresponding aesthetics.

convert_to_matrix_grob = function(df,
                                  aes_list = ga_list(),
                                  m_type = 1,
                                  height = numeric(),
                                  width = numeric(),
                                  padding = numeric(),
                                  units = c('mm'),
                                  text_cex_adj = 0.2){

  stopifnot(!is.null(nrow(df)), !is.null(ncol(df)))
  stopifnot(!length(height) == 0, !length(width) == 0)
  stopifnot(m_type %in% 1:5)

  m_type_desc = data.frame(
    m_type = c(1, 2, 3, 4, 5),
    desc = c('matrix', 'table', 'column names', 'caption', 'title')
    )

  df_fit = df
  if(m_type == 3) df = matrix(colnames(df), nrow = 1)

  nr = nrow(df)
  nc = ncol(df)

  # Adding in default values (non-matrices) if they are missing
  def_vals_non_matrices = list(
    padding_p = 0.0025
    )

  for(val_name in names(def_vals_non_matrices)){

    if (length(aes_list[[val_name]]) == 0){
      
      aes_list[[val_name]] = def_vals_non_matrices[[val_name]]
      
    }
  }

  padding = (aes_list[['padding_p']][1])*width
  
  def_vals_matrices = list(
    font_face = c(1, 1, 2, 3, 2),
    background_color = c('white', 'white', 'white', 'white', 'white'),
    background_alpha = c(1, 1, 1, 1, 1),
    border_sides = c('', '', 'bottom', '', ''),
    text_color = c('black', 'black', 'gray40', 'gray40', 'gray40'),
    text_align = c(0.5, 0.5, 0.5, 0.0, 0.5),
    text_v_align = c(0.5, 0.5, 0.5, 0.5, 0.5),
    text_just = c(0.5, 0.5, 0.5, 0.0, 0.5),
    text_v_just = c(0.5, 0.5, 0.5, 0.5, 0.5),
    text_font = c('sans', 'sans', 'sans', 'sans', 'sans', 'sans'),
    text_rot = c(0.0, 0.0, 0.0, 0.0, 0.0),
    border_color = c('gray40', 'gray40', 'gray40', 'gray40', 'gray40'),
    border_width = c(1, 1, 1, 1, 1),
    round_rect_radius = c(0.0, 0.0, 0.0, 0.0, 0.2),
    replace_na = c("", "", "", "", ""),
    group_elements = c(FALSE, FALSE, FALSE, FALSE, FALSE)
    )

  def_orig_vals_list = c(def_vals_matrices, def_vals_non_matrices)

  # - Setting row-heights, which at this time will all be equal heights
  aes_list[['row_heights']] = rep(height/nr, nr)
  
  # - Figuring out the column proportions and widths
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

  if (length(aes_list[['column_widths_p']]) == nc & length(aes_list[['padding_p']]) == nc) {
    
    aes_list[['column_widths']] = width*column_props
    
  } else {
  
    aes_list[['column_widths']] = width*column_props - 2*padding
    
  }
  
  # Making adjustments to text_cex, if need be 
  adjust_cex = length(aes_list[['text_cex']]) == 0 & length(height) == 1

  if(!adjust_cex){

    def_text_cex = 2

  } else {

    optimal_cvs = sapply(
      X = 1:nc, 
      function(x){
        column_edited = ifelse(df[,x] %in% '', " ", df[,x])
        cex_val_convergence(
          string = paste(column_edited, collapse = '\n'),
          n_lines = nr,
          height = height,
          units = units,
          width = aes_list[['column_widths']][x],
          convergence_limit = 0.025,
          sep = '\n'
        )$cex_val
        }
      )
    
    def_text_cex = ifelse(is.null(unlist(optimal_cvs)), 1, min(optimal_cvs))
    def_text_cex = def_text_cex - text_cex_adj*def_text_cex

  }
  
  # Adding in default values (matrices) if they are missing 

  def_vals_matrices[['text_cex']] = rep(def_text_cex, length(def_vals_matrices[[1]]))
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

    if (all(dim(aes_list[[val_name]]) <= 1)) {
      
      aes_list[[val_name]] = matrix(def_vals_matrices[[val_name]][m_type], nrow = nr, ncol = nc)
      
    } else {
      
      if (nrow(aes_list[[val_name]]) != nr | ncol(aes_list[[val_name]]) != nc) {
        
        error_msg = glue::glue("
          The dimensions of {val_name} in aes_list ({nrow(aes_list[[val_name]])}x{ncol(aes_list[[val_name]])}) \\
          do not match the dimension of the matrix it's linked to ({nr}x{nc}).
          ")
        
        stop(error_msg, .call = FALSE)
        
      }
    }
  }

  if (background_color_not_inputted & m_type %in% c(2)) {
    
    aes_list[['background_color']][rep(c(F,T), length = nr),] = 'gray95'
    
  }

  # Adjustments to text_just, text_align, text_v_just, text_v_align 

  # - Horizontal adjustments
  
  for(val_name in c('text_just', 'text_align')){
    if(!any(is.numeric(aes_list[[val_name]]), all(aes_list[[val_name]] %in% c('left', 'right', 'center')))){
      
      error_msg = glue::glue("
        The {val_name} in aes_list must either be a numeric value (usually between 0 and 1), \\
        or a character value in ('left', 'right', 'center'). Also, do not mix character values and \\
        numeric values.
        ")
      
      stop(error_msg, call. = FALSE)
      
    }
  }

  al_text_just = aes_list[['text_just']]
  al_text_align = aes_list[['text_align']]

  aes_list[['text_just']][al_text_just %in% 'center' | al_text_align %in% 'center'] = 0.5
  aes_list[['text_align']][al_text_just %in% 'center' | al_text_align %in% 'center'] = 0.5
  aes_list[['text_just']][al_text_just %in% 'left' | al_text_align %in% 'left'] = 0
  aes_list[['text_align']][al_text_just %in% 'left' | al_text_align %in% 'left'] = 0
  aes_list[['text_just']][al_text_just %in% 'right' | al_text_align %in% 'right'] = 1
  aes_list[['text_align']][al_text_just %in% 'right' | al_text_align %in% 'right'] = 1

  aes_list[['text_just']] = matrix(as.numeric(aes_list[['text_just']]), nrow = nr)
  aes_list[['text_align']] = matrix(as.numeric(aes_list[['text_align']]), nrow = nr)

  # - Vertical adjustments

  al_text_v_just = aes_list[['text_v_just']]
  al_text_v_align = aes_list[['text_v_align']]

  for(val_name in c('text_v_just', 'text_v_align')){
    if(!any(is.numeric(aes_list[[val_name]]), all(aes_list[[val_name]] %in% c('top', 'bottom', 'center')))){
      
      error_msg = glue::glue("
        The {val_name} in aes_list must either be a numeric value (usually between 0 and 1), \\
        or a character value in ('top', 'bottom', 'center'). Also, do not mix character values and \\
        numeric values.
        ")
      
      stop(error_msg, call. = FALSE)
      
    }
  }

  aes_list[['text_v_just']][al_text_v_just %in% 'center' | al_text_v_align %in% 'center'] = 0.5
  aes_list[['text_v_align']][al_text_v_just %in% 'center' | al_text_v_align %in% 'center'] = 0.5
  aes_list[['text_v_just']][al_text_v_just %in% 'bottom' | al_text_v_align %in% 'bottom'] = 0
  aes_list[['text_v_align']][al_text_v_just %in% 'bottom' | al_text_v_align %in% 'bottom'] = 0
  aes_list[['text_v_just']][al_text_v_just %in% 'top' | al_text_v_align %in% 'top'] = 1
  aes_list[['text_v_align']][al_text_v_just %in% 'top' | al_text_v_align %in% 'top'] = 1

  aes_list[['text_v_just']] = matrix(as.numeric(aes_list[['text_v_just']]), nrow = nr)
  aes_list[['text_v_align']] = matrix(as.numeric(aes_list[['text_v_align']]), nrow = nr)

  # Aesthetic value type checks before we start creating the grobs themselves
  for(val_name in names(def_orig_vals_list)){
    if(!class(c(aes_list[[val_name]])) %in% class(def_orig_vals_list[[val_name]])){
      
      error_msg = glue::glue("
        The class of {val_name} in aes_list must be {class(def_orig_vals_list[[val_name]])}.
        ")
      
      stop(error_msg, call. = FALSE)
      
    }
  }

  # Creating each of our mini grobs that will compose the matrix grob
  raw_grobs = grid::gList()
  
  for(j in 1:nc){
    
    for(i in 1:nr){

      rect_grob = grid::roundrectGrob(
        r = grid::unit(aes_list[['round_rect_radius']], 'snpc'),
        gp = grid::gpar(
          fill = aes_list[['background_color']][i,j],
          col = aes_list[['background_color']][i,j],
          alpha = aes_list[['background_alpha']][i,j]
          )
        )
      
      text_grob = grid::textGrob(
        label = ifelse(is.na(df[i,j]), aes_list[['replace_na']][i,j], df[i,j]),
        x = grid::unit(aes_list[['text_align']][i,j], "npc"),
        y = grid::unit(aes_list[['text_v_align']][i,j], "npc"),
        hjust = aes_list[['text_just']][i,j],
        vjust = aes_list[['text_v_just']][i,j],
        rot = aes_list[['text_rot']][i,j],
        gp = grid::gpar(
          fontface = aes_list[['font_face']][i,j],
          fontfamily = aes_list[['text_font']][i,j],
          cex = aes_list[['text_cex']][i,j],
          col = aes_list[['text_color']][i,j]
          )
        )

      borders_split = unlist(strsplit(aes_list[['border_sides']][i,j], split = ', ', fixed = T))
      if(length(borders_split) > 0){

        cell_border_gs = create_border_grob(
          border_color = aes_list[['border_color']][i,j],
          border_width = aes_list[['border_width']][i,j],
          border_sides = aes_list[['border_sides']][i,j]
          )

        raw_grobs = grid::gList(raw_grobs, grid::grobTree(rect_grob, cell_border_gs, text_grob))

      } else {

        raw_grobs = grid::gList(raw_grobs, grid::grobTree(rect_grob, text_grob))

      }
    }}

  # Layout Matrix
  layout_matrix = get_layout_matrix(df, aes_list[['group_elements']])
  first_element_indices = unlist(lapply(unique(c(layout_matrix)), function(x) min(which(c(layout_matrix) == x))))

  gridExtra::arrangeGrob(
    grobs = raw_grobs[first_element_indices],
    heights = grid::unit(aes_list[['row_heights']], units),
    widths = grid::unit(aes_list[['column_widths']], units),
    layout_matrix = layout_matrix
    )

}

#' Converts a raw .png file to a grob, with flexible aesthetics.
#'
#' @param img_path The local path to the raw .png file.
#' @param aes_list The list outputted by \code{\link{ga_list}} which gives the image grob its aesthetics.
#' @param height A numeric value designating the total height of the matrix grob in mm.
#' @param width A numeric value designating the total width of the matrix grob in mm.
#' @param units The units of the given height and width for the grob. Options are 'mm', 'cm' or 'inches', with the default of 'mm'.
#' @return A grob of the raw .png file.

convert_to_image_grob = function(img_path,
                                 aes_list,
                                 height = numeric(),
                                 width = numeric(),
                                 units = c('mm', 'cm', 'inches')) {

  stopifnot(!length(height) == 0, !length(width) == 0)
  units = match.arg(units)
  
  raw_png = png::readPNG(normalizePath(file.path(img_path)))
  edit_dims = ifelse(
    length(aes_list$maintain_aspect_ratio) == 0,
    TRUE,
    aes_list$maintain_aspect_ratio
    )
  aspect_ratio_multiplier = ifelse(
    length(aes_list$aspect_ratio_multiplier) == 0,
    1,
    aes_list$aspect_ratio_multiplier
    )

  if (edit_dims) {

    img_height_width_ratio = dim(raw_png)[1]/dim(raw_png)[2]
    img_width_height_ratio = dim(raw_png)[2]/dim(raw_png)[1]
    
    if (height >= width) {
      
      width_adj = width
      height_adj = width*img_height_width_ratio
      
    } else if (height < width) {
      
      height_adj = height
      width_adj = height*img_width_height_ratio
      
    }

  } else {

    height_adj = height
    width_adj = width
    
  }

  grid::rasterGrob(
    raw_png,
    height = grid::unit(height_adj*aspect_ratio_multiplier, units = units),
    width = grid::unit(width_adj*aspect_ratio_multiplier, units = units)
    )

}



