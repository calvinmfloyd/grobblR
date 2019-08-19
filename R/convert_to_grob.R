#' Takes in an object, and converts it to a grob based on inputted aesthetics arguments.
#'
#' @param x The object which needs to be converted to a grob. Must be either: A data.frame/matrix, the file name of a .png image, a character string, a vector, a ggplot object, NA (for an empty grob), or already a grob. 
#' @param height The numeric height in mm of the desired grob.
#' @param width The numeric width in mm of the desired grob.
#' @param aes_list The list outputted by \code{ga_list} which contains elements to adjust aesthetics to the grob of \code{x}. Different type of grobs have different types of elements of this list which will affect its aesthetics.\\
#' For character strings or matrices of dimensions n x p, the aesthetic elements can either be a single value which will be applied to the entire matrix, or a matrix of dimension n x p, which specifies how each element of the matrix will be adjusted. Note that column names and actual matrix elements are treated differently.\\
#' Possible elements for character strings, matrices and images can be found in \code{\link{ga_list}}.
#' \itemize{
#' \item \code{n_lines} The maximum number of lines is desired for the character string to be broken up into.
#' \item \code{sep} The separator within the character string which designates where a new line should start.
#' }
#' @return A grob of x with aesthetics based on the aes_list parameter.
#' @export

convert_to_grob = function(x,
                           height,
                           width,
                           units = c('mm', 'cm', 'inches'),
                           aes_list = ga_list()) {

  units = match.arg(units)

  if (all(is.numeric(x), length(x) > 0, is.null(dim(x)))) {
    x = convert_to_matrix(x)
  }
  # Matrix
  if(is.data.frame(x) | is.matrix(x)){

    x = as.matrix(x)
    matrix_aes_elements = get_matrix_aes_elements()
    colname_present = !is.null(colnames(x))
    height_adj = ifelse(colname_present, 1, 0)

    if (length(aes_list$cell_text_cex) > 0 & length(aes_list$colname_column_widths) == 0) {
      aes_list$colname_column_widths = rep(width/ncol(x), ncol(x))
    }
    
    if (length(aes_list$colname_text_cex) > 0 & length(aes_list$cell_column_widths) == 0) {
      aes_list$cell_column_widths = rep(width/ncol(x), ncol(x))
    }
    
    if (colname_present) {

      colname_ga_list = list()

      for (name in matrix_aes_elements) {

        if (!is.null(aes_list[[name]])) {
          
          colname_ga_list[[name]] = aes_list[[name]]
          
        } else {
          
          colname_ga_list[[name]] = aes_list[[paste0('colname_', name)]]
          
        }
          
      }

      colname_grob = grob_matrix(
        df = x,
        m_type = 3,
        aes_list = colname_ga_list,
        height = height/(nrow(x) + 1),
        width = width,
        units = units
        )
    }

    cell_ga_list = list()
    for(name in matrix_aes_elements){
      
        if(!is.null(aes_list[[name]])){
          
          cell_ga_list[[name]] = aes_list[[name]]
          
        } else {
          
          cell_ga_list[[name]] = aes_list[[paste0('cell_', name)]]
          
        }
    }
 
    cell_grob = grob_matrix(
      df = x,
      m_type = ifelse(colname_present, 2, 1),
      aes_list = cell_ga_list,
      height = height - height*height_adj/(nrow(x) + 1),
      width = width,
      units = units
      )

    if(colname_present){

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
  else if (ifelse(is.character(x), grepl('.png', x), FALSE)) {
    if(!file.exists(x)) stop(sprintf("The file '%s' does not exist.", x), call. = FALSE)

    grob = grob_image(
      img_path = x,
      aes_list = aes_list,
      height = height,
      width = width,
      units = units
      )

  }
  # Text
  else if (ifelse(is.character(x), !grepl('.png', x), FALSE)) {

    if (is.null(aes_list$text_cex)) {
      
      n_lines = ifelse(!is.null(aes_list$n_lines), aes_list$n_lines, 10000)
      sep = ifelse(!is.null(aes_list$str_sep), aes_list$str_sep, '\n')
      
      lines = cex_val_convergence(
        string = x,
        n_lines = n_lines,
        sep = sep,
        height = height,
        width = width,
        units = units
        )
      aes_list$text_cex = convert_to_matrix(lines$cex_val)
      
    } else {
      
      lines = line_creator(
        cex_val = aes_list$text_cex,
        string = x,
        height = height,
        width = width,
        units = units
        )
      
    }
    
    text_matrix = matrix(lines$lines, ncol = 1)
    grob = grob_matrix(
      df = text_matrix,
      aes_list = aes_list,
      height = height,
      width = width,
      units = units
      )

  }
  else if (ggplot2::is.ggplot(x)) {

    png_name = sprintf("ggplot_grob_%s.png", format(Sys.time(), '%m_%d_%Y_%H_%M_%S'))
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
    
    grob = grob_image(
      img_path = png_name,
      aes_list = aes_list,
      height = height,
      width = width,
      units = units
      )
    
    file.remove(png_name)

  }
  else if (grid::is.grob(x)) {

    grob = x

  }
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
