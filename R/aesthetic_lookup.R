
options(stringsAsFactors = FALSE)

get_empty_placeholder = function() {
  
  'none'
  
}

get_matrix_aesthetic_lookup_df = function(test,
                                          current,
                                          type,
                                          width = NULL,
                                          height = NULL,
                                          units = NULL,
                                          structure_list = NULL) {

  default_cell_text_cex = 1
  default_non_cell_text_cex = 1

  if (type %in% 'text') {
    
    lines = cex_val_convergence(
      string = current[1,1],
      n_lines = structure_list[['n_lines']],
      sep = '\n',
      height = height,
      width = width,
      units = units
      )
    
    default_cell_text_cex = convert_to_matrix(lines$cex_val)
    
    current = matrix(lines$lines, ncol = 1)
    
    nc = ncol(current)
    nc_cells = nc
    
    nr = nrow(current)
    nr_cells = nr
    nr_column_names = 0
    nr_column_headings = 0
    
    n_elements_cells = nr_cells*nc_cells
    n_elements_column_names = nr_column_names*nc_cells
    n_elements_column_headings = nr_column_headings*nc_cells

  } else {

    nc = ncol(current)
    nc_cells = nc
    
    nr = nrow(current)
    nr_cells = sum(test[['grobblR_group']] %in% c('cells'))
    nr_column_names = sum(test[['grobblR_group']] %in% c('column_names'))
    nr_column_headings = sum(test[['grobblR_group']] %in% c('column_headings'))
    
    n_elements_cells = nr_cells*nc_cells
    n_elements_column_names = nr_column_names*nc_cells
    n_elements_column_headings = nr_column_headings*nc_cells
    
    # Text Sizing, based on the structure of the grob matrix object
    if (all(dim(current) > 0)) {
      
      column_widths = width*(structure_list[['column_widths_p']]/sum(structure_list[['column_widths_p']]))
      column_widths_fit = column_widths - column_widths*structure_list[['padding_p']]
      
      # - We will fit two separate text sizes: (1) Cells and (2) non-cells [column names & column headings]
      # --> Only calculating (2) if column names are present
      
      # (1)
      default_cell_text_cex = find_optimal_text_cex(
        mat = convert_to_matrix(current[test[['grobblR_group']] %in% c('cells'),]),
        column_widths = column_widths_fit,
        height = height*(nr_cells/nr),
        units = units
        )
      
      if (nr_column_names > 0) {
      
        # (2)
        # --> We will use the column names to fit the non-cell text sizes since
        # it's assumed that if the user is using column headings, he/she will be 
        # grouping them together which makes our column-by-column text size optimization
        # less applicable.
        default_non_cell_text_cex = find_optimal_text_cex(
          mat = convert_to_matrix(current[test[['grobblR_group']] %in% c('column_names'), ]),
          column_widths = column_widths_fit,
          height = height*(nr_column_names/nr),
          units = units
          )
        
      }
  
    } 
    
  }
  
  aes_lookup_df = list(
    
    # B ----
    # > Background Alpha ----
    dplyr::tibble(
      aesthetic = 'background_alpha',
      group = 'cells',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = 1,
          nrow = nr_cells,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'background_alpha',
      group = 'column_names',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = 1,
          nrow = nr_column_names,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'background_alpha',
      group = 'column_headings',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = 1,
          nrow = nr_column_headings,
          ncol = nc_cells
          )
        )
      ),
    # > Background Color ----
    dplyr::tibble(
      aesthetic = 'background_color',
      group = 'cells',
      theme = 'default',
      accepted_classes = list(c('character')),
      value = list(
        matrix(
          data = rep(
            x = c(
              rep(get_empty_placeholder(), nc_cells),
              rep(ifelse(type %in% 'text', get_empty_placeholder(), 'gray95'), nc_cells)
              ),
            length = n_elements_cells
            ),
          nrow = nr_cells,
          ncol = nc_cells,
          byrow = TRUE
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'background_color',
      group = 'column_names',
      theme = 'default',
      accepted_classes = list(c('character')),
      value = list(
        matrix(
          data = get_empty_placeholder(),
          nrow = nr_column_names,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'background_color',
      group = 'column_headings',
      theme = 'default',
      accepted_classes = list(c('character')),
      value = list(
        matrix(
          data = get_empty_placeholder(),
          nrow = nr_column_headings,
          ncol = nc_cells
          )
        )
      ),
    # > Border Color ----
    dplyr::tibble(
      aesthetic = 'border_color',
      group = 'cells',
      theme = 'default',
      accepted_classes = list(c('character')),
      value = list(
        matrix(
          data = 'gray40',
          nrow = nr_cells,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'border_color',
      group = 'column_names',
      theme = 'default',
      accepted_classes = list(c('character')),
      value = list(
        matrix(
          data = 'gray40',
          nrow = nr_column_names,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'border_color',
      group = 'column_headings',
      theme = 'default',
      accepted_classes = list(c('character')),
      value = list(
        matrix(
          data = 'gray40',
          nrow = nr_column_headings,
          ncol = nc_cells
          )
        )
      ),
    # > Border Sides ----
    dplyr::tibble(
      aesthetic = 'border_sides',
      group = 'cells',
      theme = 'default',
      accepted_classes = list(c('character')),
      value = list(
        matrix(
          data = c(
            rep(x = ifelse(nr_column_headings + nr_column_names == 0, '', 'top'), nc_cells),
            rep(x = '', n_elements_cells - nc_cells)
            ),
          nrow = nr_cells,
          ncol = nc_cells,
          byrow = TRUE
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'border_sides',
      group = 'column_names',
      theme = 'default',
      accepted_classes = list(c('character')),
      value = list(
        matrix(
          data = 'bottom',
          nrow = nr_column_names,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'border_sides',
      group = 'column_headings',
      theme = 'default',
      accepted_classes = list(c('character')),
      value = list(
        matrix(
          data = 'bottom',
          nrow = nr_column_headings,
          ncol = nc_cells
          )
        )
      ),
    # > Border Width ----
    dplyr::tibble(
      aesthetic = 'border_width',
      group = 'cells',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = 1,
          nrow = nr_cells,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'border_width',
      group = 'column_names',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = 3,
          nrow = nr_column_names,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'border_width',
      group = 'column_headings',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = 3,
          nrow = nr_column_headings,
          ncol = nc_cells
          )
        )
      ),
    # F ----
    # > Font Face ----
    dplyr::tibble(
      aesthetic = 'font_face',
      group = 'cells',
      theme = 'default',
      accepted_classes = list(c('numeric', 'character')),
      value = list(
        matrix(
          data = 1,
          nrow = nr_cells,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'font_face',
      group = 'column_names',
      theme = 'default',
      accepted_classes = list(c('numeric', 'character')),
      value = list(
        matrix(
          data = 2,
          nrow = nr_column_names,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'font_face',
      group = 'column_headings',
      theme = 'default',
      accepted_classes = list(c('numeric', 'character')),
      value = list(
        matrix(
          data = 2,
          nrow = nr_column_headings,
          ncol = nc_cells
          )
        )
      ),
    # G ----
    # > Group Elements ----
    dplyr::tibble(
      aesthetic = 'group_elements',
      group = 'cells',
      theme = 'default',
      accepted_classes = list(c('logical')),
      value = list(
        matrix(
          data = FALSE,
          nrow = nr_cells,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'group_elements',
      group = 'column_names',
      theme = 'default',
      accepted_classes = list(c('logical')),
      value = list(
        matrix(
          data = FALSE,
          nrow = nr_column_names,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'group_elements',
      group = 'column_headings',
      theme = 'default',
      accepted_classes = list(c('logical')),
      value = list(
        matrix(
          data = TRUE,
          nrow = nr_column_headings,
          ncol = nc_cells
          )
        )
      ),
    # R ----
    # > Round Rectangle Radius ----
    dplyr::tibble(
      aesthetic = 'round_rect_radius',
      group = 'cells',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = 0,
          nrow = nr_cells,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'round_rect_radius',
      group = 'column_names',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = 0,
          nrow = nr_column_names,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'round_rect_radius',
      group = 'column_headings',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = 0.2,
          nrow = nr_column_headings,
          ncol = nc_cells
          )
        )
      ),
    # T ----
    # > Text Align ----
    # --> Horizontally
    dplyr::tibble(
      aesthetic = 'text_align',
      group = 'cells',
      theme = 'default',
      accepted_classes = list(c('numeric', 'character')),
      value = list(
        matrix(
          data = ifelse(type %in% 'text', 'left', 'center'),
          nrow = nr_cells,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_align',
      group = 'column_names',
      theme = 'default',
      accepted_classes = list(c('numeric', 'character')),
      value = list(
        matrix(
          data = 'center',
          nrow = nr_column_names,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_align',
      group = 'column_headings',
      theme = 'default',
      accepted_classes = list(c('numeric', 'character')),
      value = list(
        matrix(
          data = 'center',
          nrow = nr_column_headings,
          ncol = nc_cells
          )
        )
      ),
    # > Text CEX ----
    # --> CEX is the multiplier applied to font size, found in:
    # https://stat.ethz.ch/R-manual/R-devel/library/grid/html/gpar.html
    dplyr::tibble(
      aesthetic = 'text_cex',
      group = 'cells',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = default_cell_text_cex,
          nrow = nr_cells,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_cex',
      group = 'column_names',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = default_non_cell_text_cex,
          nrow = nr_column_names,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_cex',
      group = 'column_headings',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = default_non_cell_text_cex,
          nrow = nr_column_headings,
          ncol = nc_cells
          )
        )
      ),
    # > Text Font ----
    dplyr::tibble(
      aesthetic = 'text_font',
      group = 'cells',
      theme = 'default',
      accepted_classes = list(c('character')),
      value = list(
        matrix(
          data = 'sans',
          nrow = nr_cells,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_font',
      group = 'column_names',
      theme = 'default',
      accepted_classes = list(c('character')),
      value = list(
        matrix(
          data = 'sans',
          nrow = nr_column_names,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_font',
      group = 'column_headings',
      theme = 'default',
      accepted_classes = list(c('character')),
      value = list(
        matrix(
          data = 'sans',
          nrow = nr_column_headings,
          ncol = nc_cells
          )
        )
      ),
    # > Text Color ----
    dplyr::tibble(
      aesthetic = 'text_color',
      group = 'cells',
      theme = 'default',
      accepted_classes = list(c('character')),
      value = list(
        matrix(
          data = 'black',
          nrow = nr_cells,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_color',
      group = 'column_names',
      theme = 'default',
      accepted_classes = list(c('character')),
      value = list(
        matrix(
          data = 'gray40',
          nrow = nr_column_names,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_color',
      group = 'column_headings',
      theme = 'default',
      accepted_classes = list(c('character')),
      value = list(
        matrix(
          data = 'gray40',
          nrow = nr_column_headings,
          ncol = nc_cells
          )
        )
      ),
    # > Text Justification ----
    # --> Horizontally
    dplyr::tibble(
      aesthetic = 'text_just',
      group = 'cells',
      theme = 'default',
      accepted_classes = list(c('numeric', 'character')),
      value = list(
        matrix(
          data = ifelse(type %in% 'text', 'left', 'center'),
          nrow = nr_cells,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_just',
      group = 'column_names',
      theme = 'default',
      accepted_classes = list(c('numeric', 'character')),
      value = list(
        matrix(
          data = 'center',
          nrow = nr_column_names,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_just',
      group = 'column_headings',
      theme = 'default',
      accepted_classes = list(c('numeric', 'character')),
      value = list(
        matrix(
          data = 'center',
          nrow = nr_column_headings,
          ncol = nc_cells
          )
        )
      ),
    # > Text Vertical Align ----
    dplyr::tibble(
      aesthetic = 'text_v_align',
      group = 'cells',
      theme = 'default',
      accepted_classes = list(c('numeric', 'character')),
      value = list(
        matrix(
          data = 'center',
          nrow = nr_cells,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_v_align',
      group = 'column_names',
      theme = 'default',
      accepted_classes = list(c('numeric', 'character')),
      value = list(
        matrix(
          data = 'center',
          nrow = nr_column_names,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_v_align',
      group = 'column_headings',
      theme = 'default',
      accepted_classes = list(c('numeric', 'character')),
      value = list(
        matrix(
          data = 'center',
          nrow = nr_column_headings,
          ncol = nc_cells
          )
        )
      ),
    # > Text Vertical Justification ----
    dplyr::tibble(
      aesthetic = 'text_v_just',
      group = 'cells',
      theme = 'default',
      accepted_classes = list(c('numeric', 'character')),
      value = list(
        matrix(
          data = 'center',
          nrow = nr_cells,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_v_just',
      group = 'column_names',
      theme = 'default',
      accepted_classes = list(c('numeric', 'character')),
      value = list(
        matrix(
          data = 'center',
          nrow = nr_column_names,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_v_just',
      group = 'column_headings',
      theme = 'default',
      accepted_classes = list(c('numeric', 'character')),
      value = list(
        matrix(
          data = 'center',
          nrow = nr_column_headings,
          ncol = nc_cells
          )
        )
      ),
    # > Text Rotation ----
    dplyr::tibble(
      aesthetic = 'text_rot',
      group = 'cells',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = 0,
          nrow = nr_cells,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_rot',
      group = 'column_names',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = 0,
          nrow = nr_column_names,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_rot',
      group = 'column_headings',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = 0,
          nrow = nr_column_headings,
          ncol = nc_cells
          )
        )
      )
    
    ) %>%
    dplyr::bind_rows()
  
 return(list(
   'current' = current,
   'lookup_df' = aes_lookup_df
   ))
  
}

get_matrix_aesthetics = function() {
  
  get_matrix_aesthetic_lookup_df(
    test = data.frame(),
    current = data.frame(),
    type = 'matrix'
    ) %>%
    .[['lookup_df']] %>%
    .[['aesthetic']] %>%
    unique()

}
