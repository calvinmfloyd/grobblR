#' Breaks down character strings into one or several lines, and determines if it would fit into a specific height and width.
#'
#' @param cex_val The text cex multiplier applied to the string.
#' @param string The character string needed to be broken down into several lines.
#' @param sep The separator within the character string which designates where a new line should start.
#' @param height A numeric value designating the total height of the matrix grob in mm.
#' @param width A numeric value designating the total width of the matrix grob in mm.
#' @return A list containing a vector with each index equal to a line of the broken-down string, a TRUE/FALSE value indicating whether the lines will fit within equal sized rows and the widths in mm of each of the lines.

line_creator = function(cex_val, string, height = numeric(), width = numeric(), sep = '\n'){

  in_to_mm = function(x) x*25.4

  if(sep == '|') stop("Cannot use '|' as a separator.", call. = F)

  paragraphs = unlist(strsplit(string, split = sep))

  if(length(paragraphs) == 0){

    list(
      'valid' = TRUE,
      'lines' = string,
      'width_of_lines' = 0.0
      )

  } else {

    lines = c()
    for(p in paragraphs){

      words = unlist(strsplit(p, ' '))
      n_words = length(words)
      width_of_words = in_to_mm(graphics::strwidth(paste(words, ' '), cex = cex_val, units = 'inches'))

      out_of_words = FALSE
      line_index_start = 1
      words_to_line = rep(0, n_words)
      while(!out_of_words){
        tmp_wtl = cumsum(width_of_words[line_index_start:n_words]) %/% width
        line_indices = which(tmp_wtl == min(tmp_wtl)) + (line_index_start - 1)
        words_to_line[line_indices] = max(words_to_line) + 1
        line_index_start = max(line_indices) + 1
        if(line_index_start > n_words) out_of_words = TRUE
      }
      paragraph_n_lines = max(words_to_line)
      lines = c(lines, sapply(1:paragraph_n_lines, function(x) paste(words[words_to_line == x], collapse = ' ')))

    }

      n_lines = length(lines)
      width_of_lines = in_to_mm(graphics::strwidth(lines, cex = cex_val, units = 'inches'))
      potential_rh = (height/n_lines) - (height/n_lines)*0.5
      actual_rh = max(in_to_mm(graphics::strheight(lines, cex = cex_val, units = 'inches')))

    list(
      'valid' = actual_rh < potential_rh & max(width_of_lines) < width,
      'lines' = lines,
      'width_of_lines' = width_of_lines,
      'cex_val' = cex_val
      )
  }
}
