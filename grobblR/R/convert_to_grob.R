#' The function used within grob_col which takes in the object the 
#' user wants to grob, and converts it to a grob.
#'
#' @param x The object which needs to be converted to a grob. Must be either:
#' A data.frame/martrix, the name of a .png image, a character string, a 
#' ggplot, NA (for an empty grob), or already a grob.
#' @param height The numeric height in mm of the desired grob.
#' @param width The numeric width in mm of the desired grob.
#' @param more_args A list with any extra aesthetic preferences for the desired grobs.
#' @return A grob of x with aesthetics based on the more_args param.
#' @export

convert_to_grob <- function(x, height, width, more_args = list()){
  
  # Initilization of variables needed for matrix grobbing ----  
  convert_to_matrix <- function(x){
    if(length(x) > 0 & is.null(nrow(x))) x <- t(as.matrix(x))
    if(is.data.frame(x)) x <- as.matrix(x)
    x
  }
  
  cex_vals <- seq(0.05, 50, 0.05)
  logical_slots <- c('group_elements', 'rownames_present', 'colnames_present')
  numeric_slots <- c('cell_sep', 'row_heights', 'col_widths', 'rowname_colname_fnt_face', 'rowname_colname_border_width')
  character_slots <- c('rowname_colname_bg_color', 'rowname_colname_txt_color', 'rowname_colname_border_color', 'rowname_borders', 'colname_borders')
  non_matrix_slots <- c(logical_slots, numeric_slots, character_slots)
  
  grob_matrix_slots <- c(
    fnt_face = "matrix",
    bg_color = "matrix",
    bg_alpha = "matrix",
    borders = "matrix",
    border_color = "matrix",
    border_width = "matrix",
    txt_color = "matrix",
    txt_align = "matrix",
    txt_v_align = "matrix",
    txt_just = "matrix",
    txt_v_just = "matrix",
    txt_cex = "matrix",
    txt_font = "matrix",
    txt_angle = "matrix",
    group_elements = "logical",
    rownames_present = "logical",
    colnames_present = "logical",
    row_heights = "numeric",
    col_widths = "numeric",
    cell_sep = "numeric",
    rowname_colname_fnt_face = "numeric",
    rowname_colname_border_width = "numeric",
    rowname_colname_bg_color = "character",
    rowname_colname_txt_color = "character",
    rowname_colname_border_color = "character",
    rowname_borders = "character",
    colname_borders = "character")
  # ----
  
  if(is.data.frame(x) | is.matrix(x)){
    # converting to matrix grob if x is a dataframe or matrix 
    for(arg_name in names(more_args)[!names(more_args) %in% non_matrix_slots]){
      more_args[[arg_name]] <- convert_to_matrix(more_args[[arg_name]])
    }
    
    if(!is.null(rownames(x)) & !is.null(colnames(x))){
      x <- rbind(c('', colnames(x)), cbind(rownames(x), x))
      more_args[['colnames_present']] <- T
      more_args[['rownames_present']] <- T
    } else if(!is.null(rownames(x))){
      x <- cbind(rownames(x), x)
      more_args[['rownames_present']] <- T
    } else if(!is.null(colnames(x))){
      x <- rbind(colnames(x), x)
      more_args[['colnames_present']] <- T
    }
    
    setClass(
      "grob_matrix",
      slots = grob_matrix_slots,
      prototype = more_args)
    
    gm_obj <- new('grob_matrix')
    g <- grob_matrix(x, gm_obj, tot_height = height, tot_width = width)
    
  }
  else if(ifelse(is.character(x), grepl('.png', x), F)){
    # converting to image grob if x is a string with '.png' in it
    stopifnot(file.exists(x))
    
    setClass(
      "grob_image",
      slots = c(
        hjust = "numeric",
        vjust = "numeric",
        maintain_aspect_ratio = "logical"),
      prototype = more_args
    )
    
    gi_obj <- new('grob_image')
    g <- grob_image(x, gi_obj, tot_height = height, tot_width = width)
    
  }
  else if(ifelse(is.character(x), !grepl('.png', x), F)){
    
    one_line_tf <- ifelse('one_line' %in% names(more_args), more_args$one_line, F)
    if(!'txt_cex' %in% names(more_args)){
      for(cv in cex_vals){
        validity <- line_creator(cv, x, height, width, one_line = one_line_tf)$valid
        if(validity) optimal_cv <- cv else break
      }
      lines <- line_creator(optimal_cv, x, height, width, one_line = one_line_tf)
      more_args$txt_cex <- optimal_cv
    } else {
      lines <- line_creator(more_args$txt_cex, x, height, width, one_line = one_line_tf)
    }
    
    txt_matrix <- matrix(lines$lines, ncol = 1)
    
    for(arg_name in names(more_args)[!names(more_args) %in% non_matrix_slots]){
      more_args[[arg_name]] <- convert_to_matrix(more_args[[arg_name]])
    }
    
    setClass(
      "grob_matrix",
      slots = grob_matrix_slots,
      prototype = more_args)
    
    gm_obj <- new('grob_matrix')
    g <- grob_matrix(txt_matrix, gm_obj, tot_height = height, tot_width = width)
    
  }
  else if(ggplot2::is.ggplot(x)){
    
    png_name <- sprintf("ggplot_grob_%s_%s.png", format(Sys.time(), '%m_%d_%Y'), format(Sys.time(), "%H_%M_%S"))
    ggplot2::ggsave(png_name, x, height = height, width = width, unit = 'mm')
    
    setClass(
      "grob_image",
      slots = c(
        hjust = "numeric",
        vjust = "numeric",
        maintain_aspect_ratio = "logical"),
      prototype = more_args
    )
    
    gi_obj <- new('grob_image')
    g <- grob_image(png_name, gi_obj, tot_height = height, tot_width = width)
    file.remove(png_name)
    
  } 
  else if(is.na(x)){
    
    g <- grid::nullGrob()
    
  }
  else if(grid::is.grob(x)){
    
    g <- x
    
  }
  
  return(g)
  
}
