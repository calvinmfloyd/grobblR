#' Takes in an object, and converts it to a grob based on inputted aesthetics arguments.
#'
#' @param x The object which needs to be converted to a grob. Must be either: A data.frame/martrix, the file name of a .png image, a character string, a vector, a ggplot, NA (for an empty grob), or already a grob.
#' @param height The numeric height in mm of the desired grob.
#' @param width The numeric width in mm of the desired grob.
#' @param aes_list The list which contains elements to adjust aesthetics to the grob of x. Different type of grobs have different types of elements of this list which will affect its aesthetics.\\
#' For character strings or matrices of dimensions n x p, the aesthetic elements can either be a single value which will be applied to the entire matrix, or a matrix of dimension n x p, which specifies how each element of the matrix will be adjusted. Note that column names and acual matrix elements are treated differently. The below listed elements by themselves will correspond to the matrix elements. The listed elements with "colname_" in front of them will correspond to column name aesthetics. For example, "colname_bg_color" will refer to the background color of the column names. Possible aesthetic elements for character strings or matrices can be found in \code{\link{grob_matrix}}. Possible aesthetic elements for .png image files, or ggplot plots can be found in \code{\link{grob_image}}. Possible aesthetic elements for character strings are:
#' \itemize{
#' \item \code{n_lines} The maximum number of lines is desired for the character string to be broken up into.
#' \item \code{sep} The separator within the character string which designates where a new line should start.
#' }
#' @return A grob of x with aesthetics based on the aes_list param.
#' @export

convert_to_grob <- function(x, height, width, aes_list = ga_list()){

  if(is.numeric(x) & length(x) > 0 & is.null(dim(x))){
    x <- convert_to_matrix(x)
  }

  # converting to matrix grob if x is a dataframe or matrix
  if(is.data.frame(x) | is.matrix(x)){

    x <- as.matrix(x)

    cn_pres <- !is.null(colnames(x))

    width_adj <- 0
    height_adj <- ifelse(cn_pres, 1, 0)

    if(cn_pres){

      cn_gm_list <- list()
      for(name in names(aes_list)[grepl('colname', names(aes_list))]){
        cn_gm_list[[gsub('colname_', '', name)]] <- aes_list[[name]]
      }

      cn_grob <- grob_matrix(
        x
        ,m_type = 3
        ,height = height/(nrow(x) + 1)
        ,width = width - width_adj*width/(ncol(x) + 1)
        ,aes_list = cn_gm_list)
    }

    data_grob <- grob_matrix(
      x
      ,m_type = ifelse(cn_pres, 2, 1)
      ,aes_list = aes_list
      ,height = height - height*height_adj/(nrow(x) + 1)
      ,width = width - width*width_adj/(ncol(x) + 1))

    if(cn_pres){

      grob <- gridExtra::arrangeGrob(
        grobs = grid::gList(data_grob, cn_grob)
        ,layout_matrix = rbind(c(2), c(1))
        ,widths = grid::unit(width, 'mm')
        ,heights = grid::unit(c(height/(nrow(x) + 1), height - height/(nrow(x) + 1)), 'mm'))

    } else {

      grob <- data_grob

    }

  }
  # converting to image grob if x is a string with '.png' in it
  else if(ifelse(is.character(x), grepl('.png', x), F)){
    stopifnot(file.exists(x))
    grob <- grob_image(x, aes_list = aes_list, height = height, width = width)

  }
  else if(ifelse(is.character(x), !grepl('.png', x), F)){

    cex_vals <- seq(0.01, 20, 0.01)
    if(is.null(aes_list$text_cex)){
      n_lines <- ifelse(!is.null(aes_list$n_lines), aes_list$n_lines, 10000)
      sep <- ifelse(!is.null(aes_list$sep), aes_list$sep, '\n')
      optimal_cv <- min(cex_vals)
      for(cv in cex_vals){
        lc <- line_creator(cex_val = cv, string = x, height = height, width = width, sep = sep)
        validity <- lc$valid
        lc_n_lines <- length(lc$lines)
        if(validity & lc_n_lines <= n_lines) optimal_cv <- cv else break
      }
      lines <- line_creator(cex_val = optimal_cv, string = x, height = height, width = width, sep = sep)
      aes_list$txt_cex <- optimal_cv
    } else {
      lines <- line_creator(aes_list$txt_cex, x, height = height, width = width)
    }

    txt_matrix <- matrix(lines$lines, ncol = 1)

    grob <- grob_matrix(txt_matrix, aes_list = aes_list, height = height, width = width)

  }
  else if(ggplot2::is.ggplot(x)){

    png_name <- sprintf("ggplot_grob_%s.png", format(Sys.time(), '%m_%d_%Y_%H_%M_%S'))
    ggplot2::ggsave(png_name, x, height = height, width = width, unit = 'mm')
    grob<- grob_image(png_name, aes_list = aes_list, height = height, width = width)
    file.remove(png_name)

  }
  else if(grid::is.grob(x)){

    grob <- x

  }
  else if(is.na(x)){

    grob <- grid::rectGrob(
      gp = grid::gpar(col = NA, fill = NA),
      height = grid::unit(height, 'mm'),
      width = grid::unit(width, 'mm'))

  } else {

    stop(sprintf("Object of class %s not accepted.", class(x)), call. = F)

  }

  return(grob)

}
