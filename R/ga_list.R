
#' Grob Aesthetic / Structure List
#' 
#' Grob aesthetic list used to control aesthetics and structures within \code{\link{grob_col}},
#' \code{\link{grob_row}} and \code{\link{grob_layout}}.
#'
#' @param aspect_ratio_multiplier A numeric value which controls how much to 
#' increase/decrease the aspect ratio of images.
#' 
#' @param background_alpha Controls the background alpha/opacity of the elements 
#' of the text / matrix. Default is 1.0.
#' 
#' @param background_color Controls the background color of the elements of the text / matrix.
#' 
#' @param border_color Controls the color of the selected borders.
#' 
#' @param border_sides Controls the borders of the elements of the matrix. The
#' input is a string with the possible words "top", "bottom", "left", "right" 
#' separated by commas. For example, "top, left, right" will put borders on the 
#' top, left and right side of the grid cell, but not the bottom. 
#' Default is "", or no borders.
#' 
#' @param border_width Controls the line width density/thickness of the selected borders.
#' 
#' @param padding_p Controls the amount of proportional padding around each matrix cell.
#' 
#' @param column_widths_p If automatic column widths are not desired, the user 
#' can provide a vector of width proportions corresponding to each column of the matrix.
#' 
#' @param font_face Controls the font face of the elements of the matrix 
#' (i.e. bold, italic, etc.).
#' 
#' @param group_elements A boolean argument on whether like, adjacent matrix 
#' elements should be grouped together into a single element.
#' 
#' @param round_rect_radius Controls the radius of the corners of the rectangles 
#' matrix text is laid on top of.
#' 
#' @param maintain_aspect_ratio A boolean argument which indicates whether the 
#' aspect ratio of the image should be maintained. Default is FALSE - meaning 
#' the image will be stretched to fit the designated grid area.
#' 
#' @param text_align Controls where the text in each grid cell will be centered 
#' around, horizontally. A numeric value between 0 and 1, with 0 being all the 
#' way to the left of the grid cell, and 1 being all the way to the right of the 
#' grid cell. Default is 0.5. Can also input 'left', 'right' or 'center', which 
#' will also make edits to \code{text_just} to make the text completely left-justified, 
#' right-justified or centered, respectively.
#' 
#' @param text_cex Controls the size of the text within the matrix. 
#' Default is automatic text sizing based on the length of the elements within 
#' the matrix, the row heights and the column widths.
#' 
#' @param text_color Controls the text color of the elements of the text / matrix. 
#' 
#' @param text_font Controls the font family within the text / matrix. Default is 'sans'.
#' 
#' @param text_rot Controls the rotation in degrees of the text within the matrix. Default is 0 degrees
#' 
#' \strong{Please be aware that the automatic text sizing will not react properly 
#' if the text is angled at anything other than 0 degrees.}
#' 
#' @param text_just Controls the horizontal justification of the text in the matrix. 
#' A numeric value between 0 and 1, with 0 being left justification and 1 being right justification.
#' Default is 0.5, or center justification. Can also input 'left', 'right' or 'center', 
#' which will also make edits to \code{text_align} to make the text completely 
#' left-justified, right-justified or centered, respectively.
#' 
#' @param text_v_align Controls where the text in each grid cell will be centered 
#' around, vertically. A numeric value between 0 and 1, with 0 being all the way 
#' to the bottom of the grid cell, and 1 being all the way to the top of the 
#' grid cell. Default is 0.5.
#' 
#' Can also input 'top', 'bottom' or 'center', which will also make edits 
#' to \code{text_v_just} to make the text completely top-justified, bottom-justified 
#' or centered, respectively.
#' 
#' @param text_v_just Controls the vertical justification of the text in the matrix. 
#' A numeric value between 0 and 1, with 0 being bottom justification and 1 being 
#' top justification. Default is 0.5, or center justification. Can also input 
#' 'top', 'bottom' or 'center', which will also make edits to \code{text_v_align} 
#' to make the text completely top-justified, bottom-justified or centered, respectively.
#' 
#' @param n_lines The maximum number of lines is desired for the character string to be broken up into.
#'
#' @return A list with all possible aesthetic / structure elements.
#' 
#' @details 
#' 
#' Arguments beginning with \code{cell_} correspond to that aesthetic / structure 
#' for cells of a matrix. Arguments beginning with \code{colname_} correspond to that aesthetic / structure 
#' for column names of a matrix. Both are overridden by the argument without \code{cell_} or \code{colname_}
#' in front of them.
#' 
#' Most of the matrix aesthetics / structures are inputted into \code{\link[grid]{gpar}}.
#' More in-depth details on input possibilities can be found in its documentation.2
#' 
#' To see which of the arguments are aesthetics and what types of grobs they apply to,
#' view the documentation of \code{\link{add_aesthetic}}.
#' 
#' To see which of the arguments are structures and what types of grobs they apply to,
#' view the documentation of \code{\link{add_structure}}.
#' 
#' @export

ga_list = function(aspect_ratio_multiplier = NULL,
                   background_color = NULL,
                   background_alpha = NULL,
                   border_color = NULL,
                   border_sides = NULL,
                   border_width = NULL,
                   font_face = NULL,
                   group_elements = NULL,
                   text_color = NULL,
                   text_align = NULL,
                   text_v_align = NULL,
                   text_just = NULL,
                   text_v_just = NULL,
                   text_cex = NULL,
                   text_font = NULL,
                   text_rot = NULL,
                   round_rect_radius = NULL,
                   column_widths = NULL,
                   column_widths_p = NULL,
                   padding_p = NULL,
                   cell_font_face = NULL,
                   cell_group_elements = NULL,
                   cell_background_color = NULL,
                   cell_background_alpha = NULL,
                   cell_border_color = NULL,
                   cell_border_sides = NULL,
                   cell_border_width = NULL,
                   cell_text_color = NULL,
                   cell_text_align = NULL,
                   cell_text_v_align = NULL,
                   cell_text_just = NULL,
                   cell_text_v_just = NULL,
                   cell_text_cex = NULL,
                   cell_text_font = NULL,
                   cell_text_rot = NULL,
                   cell_round_rect_radius = NULL,
                   cell_column_widths = NULL,
                   cell_column_widths_p = NULL,
                   cell_padding_p = NULL,
                   colname_font_face = NULL,
                   colname_group_elements = NULL,
                   colname_background_color = NULL,
                   colname_background_alpha = NULL,
                   colname_border_color = NULL,
                   colname_border_sides = NULL,
                   colname_border_width = NULL,
                   colname_text_color = NULL,
                   colname_text_align = NULL,
                   colname_text_v_align = NULL,
                   colname_text_just = NULL,
                   colname_text_v_just = NULL,
                   colname_text_cex = NULL,
                   colname_text_font = NULL,
                   colname_text_rot = NULL,
                   colname_round_rect_radius = NULL,
                   colname_column_widths = NULL,
                   colname_column_widths_p = NULL,
                   colname_padding_p = NULL,
                   maintain_aspect_ratio = NULL,
                   n_lines = NULL) {

  grob_aes_list = list(
    font_face = font_face,
    group_elements = group_elements,
    background_color = background_color,
    background_alpha = background_alpha,
    border_color = border_color,
    border_width = border_width,
    border_sides = border_sides,
    text_color = text_color,
    text_align = text_align,
    text_v_align = text_v_align,
    text_just = text_just,
    text_v_just = text_v_just,
    text_cex = text_cex,
    text_font = text_font,
    round_rect_radius = round_rect_radius,
    column_widths = column_widths,
    column_widths_p = column_widths_p,
    padding_p = padding_p,
    cell_font_face = cell_font_face,
    cell_group_elements = cell_group_elements,
    cell_background_color = cell_background_color,
    cell_background_alpha = cell_background_alpha,
    cell_border_color = cell_border_color,
    cell_border_width = cell_border_width,
    cell_border_sides = cell_border_sides,
    cell_text_color = cell_text_color,
    cell_text_align = cell_text_align,
    cell_text_v_align = cell_text_v_align,
    cell_text_just = cell_text_just,
    cell_text_v_just = cell_text_v_just,
    cell_text_cex = cell_text_cex,
    cell_text_font = cell_text_font,
    cell_text_rot = cell_text_rot,
    cell_round_rect_radius = cell_round_rect_radius,
    cell_column_widths = cell_column_widths,
    cell_column_widths_p = cell_column_widths_p,
    cell_padding_p = cell_padding_p,
    colname_font_face = colname_font_face,
    colname_group_elements = colname_group_elements,
    colname_background_color = colname_background_color,
    colname_background_alpha = colname_background_alpha,
    colname_border_color = colname_border_color,
    colname_border_width = colname_border_width,
    colname_border_sides = colname_border_sides,
    colname_text_color = colname_text_color,
    colname_text_align = colname_text_align,
    colname_text_v_align = colname_text_v_align,
    colname_text_just = colname_text_just,
    colname_text_v_just = colname_text_v_just,
    colname_text_cex = colname_text_cex,
    colname_text_font = colname_text_font,
    colname_text_rot = colname_text_rot,
    colname_round_rect_radius = colname_round_rect_radius,
    colname_column_widths = colname_column_widths,
    colname_column_widths_p = colname_column_widths_p,
    colname_padding_p = colname_padding_p,
    maintain_aspect_ratio = maintain_aspect_ratio,
    aspect_ratio_multiplier = aspect_ratio_multiplier,
    n_lines = n_lines
    )
  
  grob_aes_list = lapply(
    X = grob_aes_list,
    FUN = convert_to_matrix
    )
  
  class(grob_aes_list) = 'grob_aes_list'
  return(grob_aes_list)

}

convert_to_matrix = function(x) {
  
  if (length(x) > 0 & is.null(nrow(x))) {
   
    x = t(as.matrix(x))
     
  }
  
  if (is.data.frame(x)) {
    
    x = as.matrix(x)
    
  }
  
  return(x)
  
}