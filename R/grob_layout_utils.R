
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


add_caption_grob = function(grob,
                            caption,
                            caption_aes_list,
                            caption_p,
                            caption_height,
                            units) {

  white_space_p = 0.05
  grob_height = sum(as.numeric(grob$heights))
  width = sum(as.numeric(grob$widths))
  
  if(is.null(caption_aes_list$text_cex)){
    n_lines = ifelse(!is.null(caption_aes_list$n_lines), caption_aes_list$n_lines, 10000)
    sep = ifelse(!is.null(caption_aes_list$sep), caption_aes_list$sep, '\n')
    lines = cex_val_convergence(
      string = caption,
      n_lines = n_lines,
      sep = sep,
      height = caption_height,
      width = width,
      units = units
      )
    caption_aes_list$text_cex = convert_to_matrix(lines$cex_val)
    caption = lines$lines
    if(length(caption) > 1) caption_aes_list$round_rect_radius = convert_to_matrix(0)
  }
  
  caption_grob = convert_to_matrix_grob(
    df = matrix(caption, ncol = 1),
    height = caption_height - caption_height*white_space_p,
    width = width,
    units = units,
    aes_list = caption_aes_list,
    m_type = 4
    )

  gridExtra::arrangeGrob(
    grobs = grid::gList(grob, grid::nullGrob(), caption_grob),
    layout_matrix = matrix(c(1, 2, 3), ncol = 1),
    heights = grid::unit(
      units = units,
      x = c(
        grob_height,
        caption_height*white_space_p,
        caption_height - caption_height*white_space_p
        )
      ),
    widths = grid::unit(x = width, units = units)
    )

}

add_page_number = function(grob, page_number, padding){

  grob_height = sum(as.numeric(grob$heights))
  grob_width = sum(as.numeric(grob$widths))
  
  pn_pad_pct = (0.3)/2
  ga_l = ga_list(text_color = 'gray40', background_color = NA)
  if(page_number == '') ga_l$text_cex = convert_to_matrix(1)
  
  page_number_grob = convert_to_grob(
    x = page_number,
    height = padding*(1 - 2*pn_pad_pct),
    width = padding*(1 - 2*pn_pad_pct),
    aes_list = ga_l
    )

  layout_matrix_w_pn = matrix(NA, nrow = 5, ncol = 5)
  layout_matrix_w_pn[2,2] = 1
  layout_matrix_w_pn[4,4] = 2

  gridExtra::arrangeGrob(
    grobs = grid::gList(grob, page_number_grob),
    heights = grid::unit(
      x = c(padding, grob_height, padding*pn_pad_pct, (padding - 2*padding*pn_pad_pct), padding*pn_pad_pct),
      units = 'mm'
      ),
    widths = grid::unit(
      x = c(padding, grob_width, padding*pn_pad_pct, (padding - 2*padding*pn_pad_pct), padding*pn_pad_pct),
      units = 'mm'
      ),
    layout_matrix = layout_matrix_w_pn
    )

}

add_title_grob = function(grob,
                          title,
                          title_aes_list,
                          title_p,
                          title_height,
                          units) {

  white_space_p = 0.05
  grob_height = sum(as.numeric(grob$heights))
  width = sum(as.numeric(grob$widths))
  
  if(is.null(title_aes_list$text_cex)){
    n_lines = ifelse(!is.null(title_aes_list$n_lines), title_aes_list$n_lines, 1)
    sep = ifelse(!is.null(title_aes_list$sep), title_aes_list$sep, '\n')
    lines = cex_val_convergence(
      string = title,
      n_lines = n_lines,
      sep = sep,
      height = title_height,
      width = width,
      units = units
      )
    title_aes_list$text_cex = convert_to_matrix(lines$cex_val)
    title = lines$lines
    if(length(title) > 1) title_aes_list$round_rect_radius = convert_to_matrix(0)
  }
  
  title_grob = convert_to_matrix_grob(
    df = matrix(title, ncol = 1),
    height = title_height - title_height*white_space_p,
    width = width,
    units = units,
    aes_list = title_aes_list,
    m_type = 5
    )

  gridExtra::arrangeGrob(
    grobs = grid::gList(title_grob, grid::nullGrob(), grob),
    layout_matrix = matrix(c(1, 2, 3), ncol = 1),
    heights = grid::unit(
      units = units,
      x = c(
        title_height - title_height*white_space_p,
        title_height*white_space_p,
        grob_height
        )
      ),
    widths = grid::unit(x = width, units = units)
    )

}

create_border_grob = function(border_color, border_width, border_sides){

  def_vals = list(
    border_color = 'gray40',
    border_width = 2,
    border_sides = 'top, bottom, left, right'
    )

  border_color = ifelse(is.null(border_color), def_vals$border_color, border_color)
  border_width = ifelse(is.null(border_width), def_vals$border_width, border_width)
  border_sides = ifelse(is.null(border_sides), def_vals$border_sides, border_sides)
  
  cell_border_gs = grid::gList()
  borders_split = unlist(strsplit(border_sides, split = ', ', fixed = T))

  if (length(borders_split) > 0) {
    
    for (side in 1:length(borders_split)) {
      
      cell_border_gs = grid::gList(
        cell_border_gs,
        grid::segmentsGrob(
          x0 = grid::unit(ifelse(borders_split[side] %in% c("right"), 1, 0), "npc"),
          y0 = grid::unit(ifelse(borders_split[side] %in% c("top"), 1, 0), "npc"),
          x1 = grid::unit(ifelse(borders_split[side] %in% c("top", "bottom", "right"), 1, 0), "npc"),
          y1 = grid::unit(ifelse(borders_split[side] %in% c("left", "right", "top"), 1, 0), "npc"),
          gp = grid::gpar(col = border_color, lwd = border_width)
          )
        )
      
    }
  }

  return(cell_border_gs)

}
