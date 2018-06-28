#' Based on the given text cex value and given width breaks down the given string into a series of lines. Checks to see if the lines would fit within the given height, as well. Utilized when creating character string grobs.
#'
#' @param cex_val The text cex multiplier applied to the string.
#' @param string The character string needed to be broken down into several lines.
#' @param height A numeric value designating the total height of the matrix grob in mm.
#' @param width A numeric value designating the total width of the matrix grob in mm.
#' @param one_line A TRUE/FALSE value indicating whether or not only one line is desired, instead of a series of lines. Utilized with headers, for example.
#' @return A list containing a vector with each index equal to a line of the broken-down string, a TRUE/FALSE value indicating whether the lines will fit within equal sized rows and the widths in mm of each of the lines.
#' @export

line_creator <- function(cex_val, string, height = numeric(), width = numeric(), one_line = FALSE){

  in_to_mm <- function(x) x*25.4

  if(one_line){

    string_width <- in_to_mm(graphics::strwidth(string, cex = cex_val, units = 'inches'))
    potential_rh <- height - height*0.5
    actual_rh <- in_to_mm(graphics::strheight(string, cex = cex_val, units = 'inches'))

    list(
      'valid' = actual_rh < potential_rh & string_width < width,
      'lines' = string,
      'width_of_lines' = in_to_mm(graphics::strwidth(string, cex = cex_val, units = 'inches')))

  }
  else {

    words <- unlist(strsplit(string, ' '))
    n_words <- length(words)
    width_of_words <- in_to_mm(graphics::strwidth(paste(words, ' '), cex = cex_val, units = 'inches'))

    out_of_words <- FALSE
    line_index_start <- 1
    words_to_line <- rep(0, n_words)
    while(!out_of_words){
      tmp_wtl <- cumsum(width_of_words[line_index_start:n_words]) %/% width
      line_indices <- which(tmp_wtl == min(tmp_wtl)) + (line_index_start - 1)
      words_to_line[line_indices] <- max(words_to_line) + 1
      line_index_start <- max(line_indices) + 1
      if(line_index_start > n_words) out_of_words <- TRUE
    }
    n_lines <- max(words_to_line)
    lines <- sapply(1:n_lines, function(x) paste(words[words_to_line == x], collapse = ' '))

    width_of_lines <- in_to_mm(graphics::strwidth(lines, cex = cex_val, units = 'inches'))

    potential_rh <- (height/n_lines) - (height/n_lines)*0.5
    actual_rh <- max(in_to_mm(graphics::strheight(lines, cex = cex_val, units = 'inches')))
    list(
      'valid' = actual_rh < potential_rh,
      'lines' = lines,
      'width_of_lines' = in_to_mm(graphics::strwidth(lines, cex = cex_val, units = 'inches')))
  }
}
