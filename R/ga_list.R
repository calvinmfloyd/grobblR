#' Grob aesthetic list used to control aesthetics within `grobblR`.
#'
#' @param aspect_ratio_multiplier A numeric value which controls how much to increase/decrease the aspect ratio of images or ggplots.
#' @param background_alpha Controls the background alpha/opacity of the elements of the matrix. Values are used in \code{grid::gpar()}. Default is 1.0. Used with matrices.
#' @param background_color Controls the background color of the elements of the matrix. If the matrix has no rownames or colnames, the default is white. If the matrix has column names, the default is white-gray90 on every odd-even row. Used with matrices.
#' @param border_color Controls the color of the selected borders. Default is gray40. Used with matrices.
#' @param border_sides Controls the borders of the elements of the matrix. The input is a string with the possible words "top", "bottom", "left", "right" separated by commas. For example, "top, left, right" will put borders on the top, left and right side of the grid cell, but not the bottom. Default is "", or no borders. Used with matrices.
#' @param border_width Controls the line width density/thickness of the selected borders. Values are used in \code{grid::gpar()}. Default is 4. Used with matrices.
#' @param padding_p Controls the amount of proportional padding around each matrix cell. Used with matrices.
#' @param column_widths If automatic column widths are not desired, the user can provide a vector of widths for each column in the matrix in whatever units are specified in the grob-layout. Used with matrices.
#' @param font_face Controls the font face of the elements of the matrix (i.e. bold, italic, etc.). Values are used in \code{grid::gpar()}. Default for table elements is normal, or 1. Default for column name elements is bold and italic, or 4. Used with matrices or character strings.
#' @param round_rect_radius Controls the radius of the corners of the rectangles matrix text is laid on top of. Used with matrices.
#' @param maintain_aspect_ratio A TRUE/FALSE value which indicates whether the aspect ratio of the image should be maintained. Default is FALSE - meaning the image will be stretched to fit the designated grid area. Used with images.
#' @param text_align Controls where the text in each grid cell will be centered around, horizontally. A numeric value between 0 and 1, with 0 being all the way to the left of the grid cell, and 1 being all the way to the right of the grid cell. Default is 0.5. Can also input 'left', 'right' or 'center', which will also make edits to \code{text_just} to make the text completely left-justified, right-justified or centered, respectively. Used with matrices or character strings.
#' @param text_cex Controls the size of the text within the matrix. Default is automatic text sizing based on the length of the elements within the matrix, the row heights and the column widths. Used with matrices or character strings.
#' @param text_color Controls the text color of the elements of the matrix. Default for table elements and row names is black, and a gray-blue color for column names. Used with matrices or character strings.
#' @param text_font Controls the font family of the text within the matrix. Default is sans. Used with matrices or character strings.
#' @param text_just Controls the horizontal justification of the text in the matrix. A numeric value between 0 and 1, with 0 being left justification and 1 being right justification. Default is 0.5, or center justification. Can also input 'left', 'right' or 'center', which will also make edits to \code{text_align} to make the text completely left-justified, right-justified or centered, respectively. Used with matrices or character strings.
#' @param text_v_align Controls where the text in each grid cell will be centered around, vertically. A numeric value between 0 and 1, with 0 being all the way to the bottom of the grid cell, and 1 being all the way to the top of the grid cell. Default is 0.5. Can also input 'top', 'bottom' or 'center', which will also make edits to \code{text_v_just} to make the text completely top-justified, bottom-justified or centered, respectively. Used with matrices or character strings.
#' @param text_v_just Controls the vertical justification of the text in the matrix. A numeric value between 0 and 1, with 0 being bottom justification and 1 being top justification. Default is 0.5, or center justification. Can also input 'top', 'bottom' or 'center', which will also make edits to \code{text_v_align} to make the text completely top-justified, bottom-justified or centered, respectively. Used with matrices or character strings.
#' @param n_lines The maximum number of lines is desired for the character string to be broken up into. Used with character strings.
#' @param str_sep The separator within the character string which designates where a new line should start. Used with character strings.
#' @param cell_font_face Controls the font face of matrix cells. Overridden by the \code{font_face} parameter.
#' @param cell_background_color Controls the \code{background_color} of matrix cells. Overridden by the \code{background_color} parameter.
#' @param cell_background_alpha Controls the \code{background_alpha} of matrix cells. Overridden by the \code{background_alpha} parameter.
#' @param cell_border_color Controls the \code{border_color} of matrix cells. Overridden by the \code{border_color} parameter.
#' @param cell_border_sides Controls the \code{border_sides} of matrix cells. Overridden by the \code{border_sides} parameter.
#' @param cell_border_width Controls the \code{border_width} of matrix cells. Overridden by the \code{border_width} parameter.
#' @param cell_text_color Controls the \code{text_color} of matrix cells. Overridden by the \code{text_color} parameter.
#' @param cell_text_align Controls the \code{text_align} of matrix cells. Overridden by the \code{text_align} parameter.
#' @param cell_text_v_align Controls the \code{text_v_align} of matrix cells. Overridden by the \code{text_v_align} parameter.
#' @param cell_text_just Controls the \code{text_just} of matrix cells. Overridden by the \code{text_just} parameter.
#' @param cell_text_v_just Controls the \code{text_v_just} of matrix cells. Overridden by the \code{text_v_just} parameter.
#' @param cell_text_cex Controls the \code{text_cex} of matrix cells. Overridden by the \code{text_cex} parameter.
#' @param cell_text_font Controls the \code{text_font} of matrix cells. Overridden by the \code{text_font} parameter.
#' @param cell_round_rect_radius Controls the \code{round_rect_radius} of matrix cells. Overridden by the \code{round_rect_radius} parameter.
#' @param cell_column_widths Controls the \code{column_widths} of matrix cells. Overridden by the \code{column_widths} parameter.
#' @param cell_padding_p Controls the \code{padding_p} of matrix cells. Overridden by the \code{padding_p} parameter.
#' @param cell_color_binary_cut_off A cut-off value which the binary color gradient will be applied to. Default is 0. Used with matrices.
#' @param cell_color_binary_high The color of the binary color gradient if the numeric element is greater than the \code{color_binary_cut_off}. Default is green. Used with matrices.
#' @param cell_color_binary_low The color of the binary color gradient if the numeric element is less than the \code{color_binary_cut_off}. Default is red. Used with matrices.
#' @param cell_color_binary_equal The color of the binary color gradient if the numeric element is equal to the \code{color_binary_cut_off}. Default is gray. Used with matrices.
#' @param cell_color_gradient_binary A TRUE/FALSE value which signifies if a binary color gradient should be applied to the \code{color_gradient_columns}. Used with matrices.
#' @param cell_color_gradient_columns Controls the columns which a color gradient scale will be applied to. Integer values denoting the column numbers. Can only be applied to columns with all numeric values. Used with matrices.
#' @param cell_color_gradient_max The high color for the gradual color gradient. Default is green. Used with matrices.
#' @param cell_color_gradient_mid The middle color for the gradual color gradient. Default is yellow. Used with matrices.
#' @param cell_color_gradient_min The low color for the gradual color gradient. Default is red. Used with matrices.
#' @param colname_font_face Controls the font face of column names. Overridden by the \code{font_face} parameter.
#' @param colname_background_color Controls the \code{background_color} of column names. Overridden by the \code{background_color} parameter.
#' @param colname_background_alpha Controls the \code{background_alpha} of column names. Overridden by the \code{background_alpha} parameter.
#' @param colname_border_color Controls the \code{border_color} of column names. Overridden by the \code{border_color} parameter.
#' @param colname_border_sides Controls the \code{border_sides} of column names. Overridden by the \code{border_sides} parameter.
#' @param colname_border_width Controls the \code{border_width} of column names. Overridden by the \code{border_width} parameter.
#' @param colname_text_color Controls the \code{text_color} of column names. Overridden by the \code{text_color} parameter.
#' @param colname_text_align Controls the \code{text_align} of column names. Overridden by the \code{text_align} parameter.
#' @param colname_text_v_align Controls the \code{text_v_align} of column names. Overridden by the \code{text_v_align} parameter.
#' @param colname_text_just Controls the \code{text_just} of column names. Overridden by the \code{text_just} parameter.
#' @param colname_text_v_just Controls the \code{text_v_just} of column names. Overridden by the \code{text_v_just} parameter.
#' @param colname_text_cex Controls the \code{text_cex} of column names. Overridden by the \code{text_cex} parameter.
#' @param colname_text_font Controls the \code{text_font} of column names. Overridden by the \code{text_font} parameter.
#' @param colname_round_rect_radius Controls the \code{round_rect_radius} of column names. Overridden by the \code{round_rect_radius} parameter.
#' @param colname_column_widths Controls the \code{column_widths} of column names. Overridden by the \code{column_widths} parameter.
#' @param colname_padding_p Controls the \code{padding_p} of column names. Overridden by the \code{padding_p} parameter.

#' @return A list with all possible aesthetic elements that can be adjusted, with the class of "grob_aes_list".
#' @export

ga_list = function(aspect_ratio_multiplier = NULL,
                   background_color = NULL,
                   background_alpha = NULL,
                   border_color = NULL,
                   border_sides = NULL,
                   border_width = NULL,
                   font_face = NULL,
                   text_color = NULL,
                   text_align = NULL,
                   text_v_align = NULL,
                   text_just = NULL,
                   text_v_just = NULL,
                   text_cex = NULL,
                   text_font = NULL,
                   round_rect_radius = NULL,
                   column_widths = NULL,
                   padding_p = NULL,
                   cell_font_face = NULL,
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
                   cell_round_rect_radius = NULL,
                   cell_group_elements = NULL,
                   cell_column_widths = NULL,
                   cell_padding_p = NULL,
                   cell_color_gradient_columns = NULL,
                   cell_color_gradient_binary = NULL,
                   cell_color_binary_cut_off = NULL,
                   cell_color_binary_high = NULL,
                   cell_color_binary_low = NULL,
                   cell_color_binary_equal = NULL,
                   cell_color_gradient_max = NULL,
                   cell_color_gradient_mid = NULL,
                   cell_color_gradient_min = NULL,
                   colname_font_face = NULL,
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
                   colname_round_rect_radius = NULL,
                   colname_group_elements = NULL,
                   colname_column_widths = NULL,
                   colname_padding_p = NULL,
                   maintain_aspect_ratio = NULL,
                   n_lines = NULL,
                   str_sep = NULL) {

  grob_aes_list = list(
    font_face = font_face,
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
    padding_p = padding_p,
    cell_font_face = cell_font_face,
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
    cell_round_rect_radius = cell_round_rect_radius,
    cell_group_elements = cell_group_elements,
    cell_column_widths = cell_column_widths,
    cell_padding_p = cell_padding_p,
    cell_color_gradient_columns = cell_color_gradient_columns,
    cell_color_gradient_binary = cell_color_gradient_binary,
    cell_color_binary_cut_off = cell_color_binary_cut_off,
    cell_color_binary_high = cell_color_binary_high,
    cell_color_binary_low = cell_color_binary_low,
    cell_color_binary_equal = cell_color_binary_equal,
    cell_color_gradient_max = cell_color_gradient_max,
    cell_color_gradient_mid = cell_color_gradient_mid,
    cell_color_gradient_min = cell_color_gradient_min,
    colname_font_face = colname_font_face,
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
    colname_round_rect_radius = colname_round_rect_radius,
    colname_group_elements = colname_group_elements,
    colname_column_widths = colname_column_widths,
    colname_padding_p = colname_padding_p,
    maintain_aspect_ratio = maintain_aspect_ratio,
    aspect_ratio_multiplier = aspect_ratio_multiplier,
    n_lines = n_lines,
    str_sep = str_sep
    )

  grob_aes_list = lapply(grob_aes_list, convert_to_matrix)
  class(grob_aes_list) = 'grob_aes_list'
  grob_aes_list

}


