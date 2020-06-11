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
  
  # - If a single number is provided, convert it right away to a matrix so the
  # methodology below knows to convert it to a grob matrix
  if (all(is.numeric(x), length(x) > 0, is.null(dim(x)))) {

    x = convert_to_matrix(x)

  }
  
  # - Converting to a grob matrix if x is a matrix or data frame
  if (is.data.frame(x) | is.matrix(x)) {
    
    x = distribute_aes_list_to(x = x, aes_list = aes_list, to = "grob_matrix")  
    
  }
  
  # - Converting to a grob text or a grob image depending on whether the character
  # string is actually an exisiting png file path or an existing png URL
  if (is.character(x)) {
    
    is_existing_png_file_path = file.exists(x) & (tools::file_ext(x) %in% "png")
    is_existing_png_url = RCurl::url.exists(url = x) & grepl(".png", x)
    
    if (is_existing_png_file_path | is_existing_png_url) {
      
      x = distribute_aes_list_to(x = x, aes_list = aes_list, to = "grob_image")
      
    } else {
      
      x = distribute_aes_list_to(x = x, aes_list = aes_list, to = "grob_text")
      
    }
    
  }
  
  # - Converting to a grob image if it is a ggplot object
  if (ggplot2::is.ggplot(x)) {
  
    x = distribute_aes_list_to(x = x, aes_list = aes_list, to = "grob_image")  
    
  }
  
  if (methods::is(x, "grob_matrix_object")) {
    
    x$height = height
    x$width = width
    aes_list = x$finish_ga_list
    x = x$current
    
  }
  
  if (methods::is(x, "grob_image_object")) {
    
    aes_list = x$finish_ga_list
    x = x$initial
      
  }
  
  # - Matrix / Data.Frame 
  # --> Text will be converted to matrices beforehand
  if (is.data.frame(x) | is.matrix(x)) {

    grob = convert_to_matrix_grob(
      .df = x,
      aes_list = aes_list,
      height = height,
      width = width
      )

  }
  
  # - Image Paths
  else if (is.character(x)) {
    
    grob = convert_to_image_grob(
      .image = x,
      aes_list = aes_list,
      height = height,
      width = width
      )

  }
  # - ggplot objects
  else if (ggplot2::is.ggplot(x)) {
    
    tmp_file_path = file.path(
      tempdir(),
      sprintf("ggplot_grob_%s.png", format(Sys.time(), '%m_%d_%Y_%H_%M_%S'))
      )
    
    aspect_ratio_multiplier = aes_list[["aspect_ratio_multiplier"]]
    
    ggplot2::ggsave(
      filename = tmp_file_path,
      plot = x,
      height = height*aspect_ratio_multiplier,
      width = width*aspect_ratio_multiplier,
      unit = "mm"
      )
    
    grob = convert_to_image_grob(
      .image = tmp_file_path,
      aes_list = aes_list,
      height = height,
      width = width
      )
    
    remove_file = file.remove(tmp_file_path)

  }
  # Pre-Made grob  
  else if (grid::is.grob(x)) {
    
    grob = x

  }  
  # NA (empty grob)
  else if (is.na(x)) {

    grob = grid::rectGrob(
      gp = grid::gpar(col = NA, fill = NA),
      height = grid::unit(height, units),
      width = grid::unit(width, units)
      )

  } else {

    stop(paste0("Object of class ", class(x)," not accepted."), call. = FALSE)

  }

  return(grob)

}

#' Converts a data.frame/matrix to a grob, with flexible aesthetics.
#'
#' @param .df The data.frame/matrix to be converted to a grob.
#' @param aes_list The list outputted by \code{\link{ga_list}} which gives the data.frame/matrix grob its aesthetics.
#' @param height A numeric value designating the total height of the matrix grob in mm.
#' @param width A numeric value designating the total width of the matrix grob in mm.
#' @return A grob of \code{.df}, with the corresponding aesthetics.

convert_to_matrix_grob = function(.df,
                                  aes_list = list(),
                                  height = numeric(),
                                  width = numeric()) {

  units = "mm"
  nr = nrow(.df)
  nc = ncol(.df)
  
  # - Setting row-heights, which at this time will all be equal heights
  row_heights = rep(height/nr, nr)

  column_props = aes_list[['column_widths_p']]/sum(aes_list[['column_widths_p']])
  column_widths = width*column_props

  # - Creating each of our mini grobs that will compose the matrix grob
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
        label = ifelse(is.na(.df[i,j]), aes_list[['replace_na']][i,j], .df[i,j]),
        x = grid::unit(aes_list[['text_align']][i,j], "npc"),
        y = grid::unit(aes_list[['text_v_align']][i,j], "npc"),
        hjust = aes_list[['text_just']][i,j],
        vjust = aes_list[['text_v_just']][i,j],
        rot = aes_list[['text_rot']][i,j],
        gp = grid::gpar(
          # - Checks to see if the user provided a numeric for the font face (1, 2, 3, ...)
          # or a character ("bold", "italic", etc.)
          fontface = ifelse(
            test = !is.na(as_numeric_without_warnings(aes_list[['font_face']][i,j])),
            yes = as.numeric(aes_list[['font_face']][i,j]),
            no = aes_list[['font_face']][i,j]
            ),
          fontfamily = aes_list[['text_font']][i,j],
          cex = aes_list[['text_cex']][i,j],
          col = aes_list[['text_color']][i,j]
          )
        )

      borders_split = unlist(strsplit(aes_list[['border_sides']][i,j], split = ', ', fixed = TRUE))
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

  # - Layout Matrix
  layout_matrix = get_layout_matrix(.df, aes_list[['group_elements']])
  first_element_indices = unlist(lapply(unique(c(layout_matrix)), function(x) min(which(c(layout_matrix) == x))))

  gridExtra::arrangeGrob(
    grobs = raw_grobs[first_element_indices],
    heights = grid::unit(row_heights, units),
    widths = grid::unit(column_widths, units),
    layout_matrix = layout_matrix
    )

}

#' Converts a raw .png file to a grob, with flexible aesthetics.
#'
#' @param .image The local path to the raw .png file.
#' @param aes_list The list outputted by \code{\link{ga_list}} which gives the image grob its aesthetics.
#' @param height A numeric value designating the total height of the matrix grob in mm.
#' @param width A numeric value designating the total width of the matrix grob in mm.
#' @return A grob of the raw .png file.

convert_to_image_grob = function(.image,
                                 aes_list,
                                 height = numeric(),
                                 width = numeric()) {


  units = "mm"
  
  # - If the .image string is an existing URL we will assume
  # the user is providing a link to an image.
  if (RCurl::url.exists(url = .image)) {

    raw_png = png::readPNG(source = RCurl::getURLContent(url = .image))

  # - Otherwise we will assume it's a local file path to an image
  } else {

    raw_png = png::readPNG(source = normalizePath(path = file.path(.image)))

  }
  
  edit_dims = ifelse(
    test = length(aes_list$maintain_aspect_ratio) == 0,
    yes  = TRUE,
    no   = aes_list$maintain_aspect_ratio
    )
  aspect_ratio_multiplier = ifelse(
    test = length(aes_list$aspect_ratio_multiplier) == 0,
    yes  = 1,
    no   = aes_list$aspect_ratio_multiplier
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




