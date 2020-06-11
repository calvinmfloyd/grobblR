#' Breaks down character strings into one or several lines, and determines if it would fit into a specific height and width.
#'
#' @param cex_val The text cex multiplier applied to the string.
#' @param string The character string needed to be broken down into several lines.
#' @param sep The separator within the character string which designates where a new line should start.
#' @param height A numeric value designating the total height of the matrix grob in mm.
#' @param width A numeric value designating the total width of the matrix grob in mm.
#' @param units millimeters
#' @return A list containing a vector with each index equal to a line of the broken-down string, a TRUE/FALSE value indicating whether the lines will fit within equal sized rows and the widths in mm of each of the lines.

line_creator = function(cex_val,
                        string,
                        height = numeric(),
                        width = numeric(),
                        units = c('mm'),
                        sep = '\n') {

  if (sep == '|') {
    
    stop("Cannot use '|' as a separator.", call. = FALSE)
    
  }
  
  units = match.arg(units)
  
  paragraphs = unlist(strsplit(string, split = sep))

  if (length(paragraphs) == 0) {

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
      width_of_words = units_convert(
        graphics::strwidth(paste(words, ' '), cex = cex_val, units = 'inches'),
        from_units = 'inches',
        to_units = units
        )
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
    width_of_lines = units_convert(
      graphics::strwidth(lines, cex = cex_val, units = 'inches', font = 2),
      from_units = 'inches',
      to_units = units
      )
    potential_rh = (height/n_lines) - (height/n_lines)*0.5
    actual_rh = max(units_convert(
      graphics::strheight(lines, cex = cex_val, units = 'inches', font = 2),
      from_units = 'inches',
      to_units = units
      ))

    list(
      'valid' = actual_rh < potential_rh & max(width_of_lines) < width,
      'lines' = lines,
      'width_of_lines' = width_of_lines,
      'cex_val' = cex_val
      )
  }
}

cex_val_convergence = function(string,
                               n_lines,
                               sep,
                               height,
                               width,
                               units = c('mm', 'cm', 'inches'),
                               cex_val_min = 0,
                               cex_val_max = 20,
                               convergence_limit = 0.05){

  if (sep == '|') stop("Cannot use '|' as a separator.", call. = FALSE)

  cv_min = cex_val_min
  cv_max = cex_val_max
  cv_mid = (cv_min + cv_max)/2
  cv_mid_prev = cv_max*2
  
  if(length(unlist(strsplit(string, split = sep))) == 0){
    return({
      line_creator(
        cex_val = cv_mid,
        string = string,
        height = height,
        width = width,
        sep = sep
        )
      })
    }

  evaluate = fit_valid = convergence = n_lines_valid = FALSE
  while(!all(evaluate, fit_valid, n_lines_valid, convergence)){
    evaluate = fit_valid = convergence = n_lines_valid = FALSE
    lc = line_creator(
      cex_val = cv_mid,
      string = string,
      height = height,
      width = width,
      units = units,
      sep = sep
    )
    fit_valid = lc$valid
    n_lines_valid = length(lc$lines) <= n_lines
    convergence = abs(cv_mid_prev - cv_mid) <= convergence_limit
    evaluate = TRUE
    
    cv_max = ifelse(fit_valid & n_lines_valid, cv_max, cv_mid)
    cv_min = ifelse(fit_valid & n_lines_valid, cv_mid, cv_min)
    cv_mid_prev = cv_mid
    cv_mid = (cv_min + cv_max)/2
  }
  
  return(lc)
  
}
