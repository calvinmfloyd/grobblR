#' Takes in an object, and converts it to a grob based on inputted aesthetics arguments.
#'
#' @param x The object which needs to be converted to a grob. Must be either: A data.frame/martrix, the file name of a .png image, a character string, a ggplot, NA (for an empty grob), or already a grob.
#' @param height The numeric height in mm of the desired grob.
#' @param width The numeric width in mm of the desired grob.
#' @param aes_list The list which contains elements to adjust aesthetics to the grob of x. Different type of grobs have different types of elements of this list which will affect its aesthetics.\\
#' For character strings or matrices of dimensions n x p, the aesthetic elements can either be a single value which will be applied to the entire matrix, or a matrix of dimension n x p, which specifies how each element of the matrix will be adjusted. Note that column names and acual matrix elements are treated differently. The below listed elements by themselves will correspond to the matrix elements. The listed elements with "colname_" in front of them will correspond to column name aesthetics. For example, "colname_bg_color" will refer to the background color of the column names. Possible aesthetic elements for character strings or matrices can be found in \code{\link{grob_matrix}}. Possible aesthetic elements for .png image files, or ggplot plots can be found in \code{\link{grob_image}}. Possible aesthetic elements for character strings are:
#' \itemize{
#' \item \code{n_lines} The maximum number of lines is desired for the character string to be broken up into.
#' \item \code{sep} The separator within the character string which designates where a new line should start. Default is "\n".
#' }
#' @return A grob of x with aesthetics based on the aes_list param.
#' @export

convert_to_grob <- function(x, height, width, aes_list = list()){

  # Initilization of variables needed for matrix grobbing ----
  convert_to_matrix <- function(x){
    if(length(x) > 0 & is.null(nrow(x))) x <- t(as.matrix(x))
    if(is.data.frame(x)) x <- as.matrix(x)
    x
  }

  sub_list_elements <- function(base_list, new_list){
    for(element_name in names(new_list)[names(new_list) %in% names(base_list)]){
      base_list[[element_name]] <- new_list[[element_name]]
    }
    base_list
  }

  if(is.numeric(x) & length(x) > 0 & is.null(dim(x))){
    x <- convert_to_matrix(x)
  }

  cex_vals <- seq(0.01, 20, 0.01)

  gm_list <- list(
    fnt_face = matrix(ncol = 0, nrow = 0),
    bg_color = matrix(ncol = 0, nrow = 0),
    bg_alpha = matrix(ncol = 0, nrow = 0),
    borders = matrix(ncol = 0, nrow = 0),
    border_color = matrix(ncol = 0, nrow = 0),
    border_width = matrix(ncol = 0, nrow = 0),
    txt_color = matrix(ncol = 0, nrow = 0),
    txt_align = matrix(ncol = 0, nrow = 0),
    txt_v_align = matrix(ncol = 0, nrow = 0),
    txt_just = matrix(ncol = 0, nrow = 0),
    txt_v_just = matrix(ncol = 0, nrow = 0),
    txt_cex = matrix(ncol = 0, nrow = 0),
    txt_font = matrix(ncol = 0, nrow = 0),
    txt_angle = matrix(ncol = 0, nrow = 0),
    round_rect_radius = matrix(ncol = 0, nrow = 0),
    group_elements = logical(),
    row_heights = numeric(),
    col_widths = numeric(),
    cell_sep = numeric(),
    color_gradient_cols = numeric(),
    color_gradient_binary = logical(),
    color_binary_cut_off = numeric(),
    color_binary_high = character(),
    color_binary_low = character(),
    color_binary_equal = character(),
    color_gradient_max = character(),
    color_gradient_mid = character(),
    color_gradient_min = character())

  cn_gm_list <- gm_list

  gi_list <- list(
    maintain_aspect_ratio = logical(),
    hjust = numeric(),
    vjust = numeric())

  logical_slots <- c('group_elements')
  numeric_slots <- c('cell_sep', 'row_heights', 'col_widths')
  non_matrix_slots <- c(
    logical_slots,
    numeric_slots,
    names(gm_list)[grepl('color_g|color_b', names(gm_list))])

  # Checking that all the elements names in aes_list are valid/accepted ----
  invalid_names <- names(aes_list)[
    !names(aes_list) %in% c(names(gm_list), names(gi_list), paste0('colname_', names(gm_list)), 'n_lines', 'sep')]
  if(length(invalid_names) > 0) warning(sprintf(
      "The following elements in aes_list are not accepted and are not affecting any aesthetics: %s",
      paste(invalid_names, collapse = ', ')),
    call. = FALSE)

  ctm_names <- names(aes_list)[(names(aes_list) %in% names(gm_list)) & (!names(aes_list) %in% non_matrix_slots)]
  for(arg_name in ctm_names){
    aes_list[[arg_name]] <- convert_to_matrix(aes_list[[arg_name]])
  }

  # converting to matrix grob if x is a dataframe or matrix
  if(is.data.frame(x) | is.matrix(x)){

    x <- as.matrix(x)

    gm_list <- sub_list_elements(gm_list, aes_list)
    cn_pres <- !is.null(colnames(x))

    width_adj <- 0
    height_adj <- ifelse(cn_pres, 1, 0)

    if(cn_pres){

      if(any(grepl('colname_', names(aes_list)))){
        for(name in names(aes_list)[grepl('colname', names(aes_list))]){
          cn_gm_list[[gsub('colname_', '', name)]] <- aes_list[[name]]
        }
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
      ,aes_list = gm_list
      ,height = height - height*height_adj/(nrow(x) + 1)
      ,width = width - width*width_adj/(ncol(x) + 1))

    if(cn_pres){

      g <- gridExtra::arrangeGrob(
        grobs = grid::gList(data_grob, cn_grob)
        ,layout_matrix = rbind(c(2), c(1))
        ,widths = grid::unit(width, 'mm')
        ,heights = grid::unit(c(height/(nrow(x) + 1), height - height/(nrow(x) + 1)), 'mm'))

    } else {

      g <- data_grob

    }

  }
  # converting to image grob if x is a string with '.png' in it
  else if(ifelse(is.character(x), grepl('.png', x), F)){
    stopifnot(file.exists(x))
    gi_list <- sub_list_elements(gi_list, aes_list)
    g <- grob_image(x, aes_list = gi_list, height = height, width = width)

  }
  else if(ifelse(is.character(x), !grepl('.png', x), F)){

    if(!'txt_cex' %in% names(aes_list)){
      n_lines <- ifelse('n_lines' %in% names(aes_list), aes_list$n_lines, 10000)
      sep <- ifelse('sep' %in% names(aes_list), aes_list$sep, '/')
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

    gm_list <- sub_list_elements(gm_list, aes_list)
    g <- grob_matrix(txt_matrix, gm_list, height = height, width = width)

  }
  else if(ggplot2::is.ggplot(x)){

    png_name <- sprintf("ggplot_grob_%s.png", format(Sys.time(), '%m_%d_%Y_%H_%M_%S'))
    ggplot2::ggsave(png_name, x, height = height, width = width, unit = 'mm')
    gi_list <- sub_list_elements(gi_list, aes_list)
    g <- grob_image(png_name, aes_list = gi_list, height = height, width = width)
    file.remove(png_name)

  }
  else if(grid::is.grob(x)){

    g <- x

  }
  else if(is.na(x)){

    g <- grid::rectGrob(
      gp = grid::gpar(col = NA, fill = NA),
      height = grid::unit(height, 'mm'),
      width = grid::unit(width, 'mm'))

  } else {

    stop(sprintf("Object of class %s not accepted.", class(x)), call. = F)

  }

  return(g)

}
