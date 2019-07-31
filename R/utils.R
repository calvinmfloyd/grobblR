
units_convert = function(x, from_units, to_units) {
  
  if (from_units == to_units)
    x
  else if (from_units %in% 'inches' & to_units %in% 'mm')
    x*25.4
  else if (from_units %in% 'inches' & to_units %in% 'cm')
    x*2.54
  else if (from_units %in% 'mm' & to_units %in% 'inches')
    x/25.4
  else if (from_units %in% 'cm' & to_units %in% 'inches')
    x/2.54
  else if (from_units %in% 'mm' & to_units %in% 'cm')
    x/10
  else if (from_units %in% 'cm' & to_units %in% 'mm')
    x*10

}

decimal_places = function(x) {
  if((x %% 1) != 0){
    return(nchar(strsplit(sub('0+$', '', as.character(round(x,5))), ".", fixed = T)[[1]][[2]]))
  } else {
    return(0)
  }
}

get_matrix_aes_elements = function() {
  
  aes_list = ga_list()
  stringr::str_replace(
    string = names(aes_list)[grepl('cell_', names(aes_list))],
    pattern = 'cell_',
    replacement = ''
    )
  
}