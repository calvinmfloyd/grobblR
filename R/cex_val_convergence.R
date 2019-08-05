
cex_val_convergence = function(string,
                               n_lines,
                               sep,
                               height,
                               width,
                               units = c('mm', 'cm', 'inches'),
                               cex_val_min = 0,
                               cex_val_max = 50,
                               convergence_limit = 0.25){

  sep = ifelse(is.null(sep), '\n', sep)
  n_lines = ifelse(is.null(n_lines), 10000, n_lines)
  if(sep == '|') stop("Cannot use '|' as a separator.", call. = FALSE)

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
