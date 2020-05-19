
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

distribute_aes_list_to = function(x,
                                  aes_list,
                                  to = c("grob_matrix", "grob_text", "grob_image")) {
  
  
  to = match.arg(to)
  
  supplied_aesthetics = aes_list %>%
    # - Figuring out which elements of the aes_list are not null
    purrr::map_lgl(~ !is.null(.)) %>% 
    # - Taking only the TRUE elements
    .[.] %>%
    # - Extracting the not null names
    names()
  
  if (to %in% "grob_matrix") {
    
    x = grob_matrix(x)
    
  } else if (to %in% "grob_text") {
    
    x = grob_text(x)
    
  } else if (to %in% "grob_image") {
    
    x = grob_image(x)
    
  }
  
  if (to %in% c("grob_matrix", "grob_text")) {
    
    matrix_groups = unique(x$test[["grobblR_group"]])
    
    # - Supplied overall aesthetics  
    supplied_overall = supplied_aesthetics %>%
      .[!grepl("^cell_|^colname", ., perl = TRUE)]
    
    supplied_overall_aesthetics = supplied_overall[supplied_overall %in% matrix_aesthetics]
    supplied_overall_structures = supplied_overall[supplied_overall %in% matrix_structures]
    
    # - Supplied cell aesthetics  
    supplied_cell_aesthetics = supplied_aesthetics %>%
      .[grepl("^cell_", ., perl = TRUE)] %>% 
      gsub(pattern = "cell_", replacement = "", x = .) %>%
      .[. %in% matrix_aesthetics]

    # - Supplied column name aesthetics  
    supplied_colname_aesthetics = supplied_aesthetics %>%
      .[grepl("^colname_", ., perl = TRUE)] %>% 
      gsub(pattern = "colname_", replacement = "", x = .) %>%
      .[. %in% matrix_aesthetics]

    # - Looping through the overall aesthetics and apply them to the cells
    for (overall_aes in supplied_overall_aesthetics) {

      for (group in matrix_groups) {
        
        x = x %>%
          add_aesthetic(
            aesthetic = overall_aes,
            value = aes_list[[overall_aes]],
            group = group
            )
        
      }
      
    }
    
    # - Looping through the overall structures and applying them to the grob matrix
    for (overall_str in supplied_overall_structures) {
      
      x = x %>%
        add_structure(structure = overall_str, value = aes_list[[overall_str]])
      
    }
    
    # - Looping through the cell aesthetics and applying them to the grob matrix
    for (cell_aes in supplied_cell_aesthetics) {
    
      x = x %>%
        add_aesthetic(
          aesthetic = cell_aes,
          value = aes_list[[paste0("cell_", cell_aes)]],
          group = "cells"
          )
      
    }
    
    # - Looping through the column name aesthetics and applying them to the grob matrix
    for (colname_aes in supplied_colname_aesthetics) {
      
      x = x %>%
        add_aesthetic(
          aesthetic = colname_aes,
          value = aes_list[[paste0("colname_", colname_aes)]],
          group = "column_names"
          )
      
    }

    
  }
  
  else if (to %in% "grob_image") {
    
    supplied_image_structures = supplied_aesthetics[supplied_aesthetics %in% image_structures]
    
    # - Looping through the image structures and applying them to the grob image
    for (image_str in supplied_image_structures) {

      x = x %>%
        add_structure(structure = image_str, value = aes_list[[image_str]][1,1])

    }
    
  }
  
  return(x)
  
}



add_caption_grob = function(grob,
                            caption,
                            caption_aes_list,
                            caption_height) {

  white_space_p = 0.05
  grob_height = sum(as.numeric(grob$heights))
  width = sum(as.numeric(grob$widths))
  
  caption_grob_text = distribute_aes_list_to(
    x = caption,
    aes_list = caption_aes_list,
    to = "grob_text"
    )
  
  caption_grob_text$theme = "caption"
  caption_grob_col = grob_col(caption_grob_text, padding = 0)
  caption_grob_col$height = caption_height - caption_height*white_space_p
  caption_grob_col$width = width
  caption_grob = caption_grob_col$grob

  gridExtra::arrangeGrob(
    grobs = grid::gList(grob, grid::nullGrob(), caption_grob),
    layout_matrix = matrix(c(1, 2, 3), ncol = 1),
    heights = grid::unit(
      units = "mm",
      x = c(
        grob_height,
        caption_height*white_space_p,
        caption_height - caption_height*white_space_p
        )
      ),
    widths = grid::unit(x = width, units = "mm")
    )

}

add_page_number = function(grob, page_number, padding){

  grob_height = sum(as.numeric(grob$heights))
  grob_width = sum(as.numeric(grob$widths))
  
  pn_pad_pct = (0.2)/2

  pn_grob_text = grob_matrix(page_number) %>%
    add_aesthetic(aesthetic = "text_color", value = "gray40", group = "cells") %>%
    add_aesthetic(aesthetic = "background_color", value = "none", group = "cells")
  
  pn_grob_col = grob_col(pn_grob_text, padding = 0)
  pn_grob_col$height = padding*(1 - 2*pn_pad_pct)
  pn_grob_col$width = padding*(1 - 2*pn_pad_pct)
  pn_grob = pn_grob_col$grob

  layout_matrix_w_pn = matrix(NA, nrow = 5, ncol = 5)
  layout_matrix_w_pn[2,2] = 1
  layout_matrix_w_pn[4,4] = 2

  gridExtra::arrangeGrob(
    grobs = grid::gList(grob, pn_grob),
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
                          title_height) {

  white_space_p = 0.05
  grob_height = sum(as.numeric(grob$heights))
  width = sum(as.numeric(grob$widths))

  title_grob_text = distribute_aes_list_to(
    x = title,
    aes_list = title_aes_list,
    to = "grob_text"
    )
  
  title_grob_text$theme = "title"
  title_grob_col = grob_col(title_grob_text, padding = 0)
  title_grob_col$height = title_height - title_height*white_space_p
  title_grob_col$width = width
  title_grob = title_grob_col$grob
  
  gridExtra::arrangeGrob(
    grobs = grid::gList(title_grob, grid::nullGrob(), grob),
    layout_matrix = matrix(c(1, 2, 3), ncol = 1),
    heights = grid::unit(
      units = "mm",
      x = c(
        title_height - title_height*white_space_p,
        title_height*white_space_p,
        grob_height
        )
      ),
    widths = grid::unit(x = width, units = "mm")
    )

}

create_border_grob = function(border_color, border_width, border_sides){

  def_vals = list(
    border_color = 'gray40',
    border_width = 2,
    border_sides = 'top, bottom, left, right'
    )

  border_color = ifelse(is.null(border_color), def_vals[['border_color']], border_color)
  border_width = ifelse(is.null(border_width), def_vals[['border_width']], border_width)
  border_sides = ifelse(is.null(border_sides), def_vals[['border_sides']], border_sides)
  
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
