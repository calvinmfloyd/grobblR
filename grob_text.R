
line_creator <- function(cv, string, height, width, one_line = FALSE){
  
  in_to_mm <- function(x) x*25.4
  
  if(one_line){
    
    string_width <- in_to_mm(strwidth(string, cex = cv, units = 'inches'))
    potential_rh <- height - height*0.5
    actual_rh <- in_to_mm(strheight(string, cex = cv, units = 'inches'))
    
    list(
      'valid' = actual_rh < potential_rh & string_width < width,
      'lines' = string,
      'width_of_lines' = in_to_mm(strwidth(string, cex = cv, units = 'inches')))
    
  }
  else {
    
    words <- unlist(strsplit(string, ' '))
    n_words <- length(words)
    width_of_words <- in_to_mm(strwidth(paste(words, ' '), cex = cv, units = 'inches'))
    
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
    
    width_of_lines <- in_to_mm(strwidth(lines, cex = cv, units = 'inches'))

    potential_rh <- (height/n_lines) - (height/n_lines)*0.5
    actual_rh <- max(in_to_mm(strheight(lines, cex = cv, units = 'inches')))
    list(
      'valid' = actual_rh < potential_rh,
      'lines' = lines,
      'width_of_lines' = in_to_mm(strwidth(lines, cex = cv, units = 'inches')))
  }
}
