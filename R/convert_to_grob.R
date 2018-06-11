#' The function used within grob_col which takes in the object the user wants to grob, and converts it to a grob.
#'
#' @param x The object which needs to be converted to a grob. Must be either: A data.frame/martrix, the name of a .png image, a character string, a ggplot, NA (for an empty grob), or already a grob.
#' @param height The numeric height in mm of the desired grob.
#' @param width The numeric width in mm of the desired grob.
#' @param aes_list The list which contains elements to adjust aesthetics to the grob of x.
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

  cex_vals <- seq(0.05, 50, 0.05)

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

  rn_gm_list <- gm_list
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

  # ----

  if(is.data.frame(x) | is.matrix(x)){
    # converting to matrix grob if x is a dataframe or matrix
    for(arg_name in names(aes_list)[!names(aes_list) %in% non_matrix_slots]){
      aes_list[[arg_name]] <- convert_to_matrix(aes_list[[arg_name]])
    }

    gm_list <- sub_list_elements(gm_list, aes_list)
    cn_pres <- !is.null(colnames(x))
    rn_pres <- !is.null(rownames(x))
    width_adj <- ifelse(rn_pres, 1, 0)
    height_adj <- ifelse(cn_pres, 1, 0)

    if(rn_pres){

      if(any(grepl('rowname_', names(aes_list)))){
        for(name in names(aes_list)[grepl('rowname', names(aes_list))]){
          rn_gm_list[[gsub('rowname_', '', name)]] <- aes_list[[name]]
        }
      }

      rn_df <- matrix(rownames(x), ncol = 1)
      rn_grob <- grob_matrix(
        rn_df
        ,m_type = 4
        ,height = height - height_adj*height/(nrow(x) + 1)
        ,width = width/(ncol(x) + 1)
        ,aes_list = rn_gm_list)
    }

    if(cn_pres){

      if(any(grepl('colname_', names(aes_list)))){
        for(name in names(aes_list)[grepl('colname', names(aes_list))]){
          cn_gm_list[[gsub('colname_', '', name)]] <- aes_list[[name]]
        }
      }

      cn_df <- matrix(colnames(x), nrow = 1)
      cn_grob <- grob_matrix(
        cn_df
        ,m_type = 3
        ,height = height/(nrow(x) + 1)
        ,width = width - width_adj*width/(ncol(x) + 1)
        ,aes_list = cn_gm_list)
    }

    data_grob <- grob_matrix(
      x
      ,m_type = ifelse(cn_pres | rn_pres, 2, 1)
      ,aes_list = gm_list
      ,height = height - height*height_adj/(nrow(x) + 1)
      ,width = width - width*width_adj/(ncol(x) + 1))

    if(cn_pres & !rn_pres){
      g <- gridExtra::arrangeGrob(
        grobs = grid::gList(cn_grob, data_grob)
        ,layout_matrix = rbind(c(2), c(1))
        ,widths = grid::unit(width, 'mm')
        ,heights = grid::unit(c(height/(nrow(x) + 1), height - height/(nrow(x) + 1)), 'mm'))
    } else if(!cn_pres & rn_pres){
      g <- gridExtra::arrangeGrob(
        grobs = grid::gList(data_grob, rn_grob)
        ,layout_matrix = cbind(c(2), c(1))
        ,widths = grid::unit(c(width/(ncol(x) + 1), width - width/(ncol(x) + 1)), 'mm')
        ,heights = grid::unit(height, 'mm'))
    } else if(cn_pres & rn_pres){
      g <- gridExtra::arrangeGrob(
        grobs = grid::gList(data_grob, cn_grob, rn_grob)
        ,layout_matrix = rbind(c(NA, 2), c(3, 1))
        ,widths = grid::unit(c(width/(ncol(x) + 1), width - width/(ncol(x) + 1)), 'mm')
        ,heights = grid::unit(c(height/(nrow(x) + 1), height - height/(nrow(x) + 1)), 'mm'))
    } else {

      g <- data_grob

    }

  }
  else if(ifelse(is.character(x), grepl('.png', x), F)){
    # converting to image grob if x is a string with '.png' in it
    stopifnot(file.exists(x))
    gi_list <- sub_list_elements(gi_list, aes_list)
    g <- grob_image(x, aes_list = gi_list, height = height, width = width)

  }
  else if(ifelse(is.character(x), !grepl('.png', x), F)){

    one_line_tf <- ifelse('one_line' %in% names(aes_list), aes_list$one_line, F)
    if(!'txt_cex' %in% names(aes_list)){
      for(cv in cex_vals){
        validity <- line_creator(cv, x, height, width, one_line = one_line_tf)$valid
        if(validity) optimal_cv <- cv else break
      }
      lines <- line_creator(optimal_cv, x, height = height, width = width, one_line = one_line_tf)
      aes_list$txt_cex <- optimal_cv
    } else {
      lines <- line_creator(aes_list$txt_cex, x, height = height, width = width, one_line = one_line_tf)
    }

    txt_matrix <- matrix(lines$lines, ncol = 1)

    for(arg_name in names(aes_list)[!names(aes_list) %in% non_matrix_slots]){
      aes_list[[arg_name]] <- convert_to_matrix(aes_list[[arg_name]])
    }

    gm_list <- sub_list_elements(gm_list, aes_list)
    g <- grob_matrix(txt_matrix, gm_list, height = height, width = width)

  }
  else if(ggplot2::is.ggplot(x)){

    png_name <- sprintf("ggplot_grob_%s_%s.png", format(Sys.time(), '%m_%d_%Y'), format(Sys.time(), "%H_%M_%S"))
    ggplot2::ggsave(png_name, x, height = height, width = width, unit = 'mm')
    gi_list <- sub_list_elements(gi_list, aes_list)
    g <- grob_image(png_name, aes_list = gi_list, height = height, width = width)
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
