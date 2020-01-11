
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


allot_sizes = function(space_size,
                       inputted_proportions,
                       inputted_sizes,
                       grob_layout_location = '',
                       affected_grobs = '',
                       measurement = '',
                       units = 'mm') {
  
  have_inputted_sizes = !is.na(inputted_sizes)
  all_inputted_sizes = all(have_inputted_sizes)
  all_inputted_proportions = all(!have_inputted_sizes)
  
  total_inputted_sizes = round(sum(inputted_sizes, na.rm = TRUE), 1)
  space_size = round(space_size, 1)
  
  # - Checking to make sure the inputted sizes don't actually go above the alloted
  # space size.
  if (total_inputted_sizes > space_size) {
  
    stop(
      paste0(
        "The total inputted ", measurement, " of ", total_inputted_sizes, units, " is greater ",
        "than the allotted ", measurement, " of ", space_size, units, " ",
        "for the ", affected_grobs, " in the ", grob_layout_location, "."
        ),
      call. = FALSE
      )
    
  }

  # - If all the specifc sizes are given, making sure they add up to the total 
  # allotted space size.
  if (all_inputted_sizes && total_inputted_sizes != space_size) {
  
    stop(
      paste0(
        "All sizes are provided, but the total inputted ", measurement,
        " of ", total_inputted_sizes, units, " does not equal ",
        "the allotted ", measurement, " of ", space_size, units, " ",
        "for the ", affected_grobs, " in the ", grob_layout_location, "."
        ),
      call. = FALSE
      )
    
  }
  
  # - If some but not all specifc sizes are given, making sure they leave room 
  # for the rows that rely on proportions for their sizes.
  if (!all_inputted_sizes && total_inputted_sizes == space_size) {
  
    stop(
      paste0(
        "Some sizes are provided, but the total inputted ", measurement,
        " of ", total_inputted_sizes, units, " equals ",
        "the allotted ", measurement, " of ", space_size, units, ", ",
        "leaving no room for the ", affected_grobs, " that rely on proportions",
        " in the ", grob_layout_location, "."
        ),
      call. = FALSE
      )
    
  }
    
  # - 3 Situations we must handle when alloting sizes to grobs:
  # (1) The user gives all specific sizes for the grobs
  # (2) The user gives all size proportions for the grobs
  # (3) The user gives a mix of size proportions and specific sizes
  
  # - Case (1)
  if (all_inputted_sizes) {
  # - NOTE: If the user inputs all specific sizes and they do not add to
  # total space size, then the sizes will be spread out 
  # appropriately over the total height of the grob-layout.

    sizes = inputted_sizes

  # - Case (2)
  } else if (all_inputted_proportions) {
    
    sizes = space_size*(inputted_proportions/sum(inputted_proportions))
    
  # - Case (3)
  } else {
  # - Approach: 
  # (Step 1) We will figure out which grob rows have been given specific sizes.
  # (Step 2) Those sizes will remained untouched, but we have to figure out how
  # much size proportion is remaining to distribute to rest of the grobs
  # which have been given size proportions instead of specific sizes.
  # (Step 3) Distribute the remaining size proportion to those grobs relying
  # on size proportion for its height allotment.
 
    proportions = rep(NA_real_, length(inputted_proportions))
    sizes = rep(NA_real_, length(inputted_sizes))
    
    # (Step 1) 
    proportions[have_inputted_sizes] = inputted_sizes[have_inputted_sizes]/space_size
    
    # (Step 2)
    proportions_of_inputted_sizes = proportions[have_inputted_sizes]
    proportions_of_inputted_proportions = inputted_proportions[!have_inputted_sizes]
    
    # (Step 3)
    proportions[!have_inputted_sizes] = {
      
      inputted_proportions[!have_inputted_sizes]/sum(proportions_of_inputted_proportions)*(1 - sum(proportions_of_inputted_sizes))
      
    }
 
    sizes = space_size*proportions
    
  }
  
  return(sizes)

}

