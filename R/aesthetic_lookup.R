
`%>%` = dplyr::`%>%`
options(stringsAsFactors = FALSE)

get_aesthetic_lookup_df = function(test,
                                   current,
                                   width = NULL,
                                   height = NULL,
                                   units = NULL,
                                   structure_list = NULL) {

  nc = ncol(current)
  nc_cells = nc
  
  nr = nrow(current)
  nr_cells = sum(test[['grobblR_group']] %in% c('cells'))
  nr_column_names = sum(test[['grobblR_group']] %in% c('column_names'))
  nr_column_headings = sum(test[['grobblR_group']] %in% c('column_headings'))
  
  n_elements_cells = nr_cells*nc_cells
  n_elements_column_names = nr_column_names*nc_cells
  n_elements_column_headings = nr_column_headings*nc_cells
  
  default_cell_text_cex = 1
  default_non_cell_text_cex = 1
  
  # Text Sizing, based on the structure of the grob matrix object
  if (all(dim(current) > 0)) {
    
    column_widths = width*(structure_list[['column_widths_p']]/sum(structure_list[['column_widths_p']]))
    column_widths_fit = column_widths - column_widths*structure_list[['padding_p']]
    
    # - We will fit two separate text sizes: (1) Cells and (2) non-cells 
    # [column names & column headings]
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
  
  aes_lookup_df = list(
    
    # B ----
    # > Background Alpha ----
    dplyr::tibble(
      aesthetic = 'background_alpha',
      group = 'cells',
      theme = 'default',
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
      value = list(
        matrix(
          data = rep(
            x = c(rep(NA_character_, nc_cells), rep('gray95', nc_cells)),
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
      value = list(
        matrix(
          data = NA_character_,
          nrow = nr_column_names,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'background_color',
      group = 'column_headings',
      theme = 'default',
      value = list(
        matrix(
          data = NA_character_,
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
      value = list(
        matrix(
          data = c(rep('top', nc_cells), rep('', n_elements_cells - nc_cells)),
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
      value = list(
        matrix(
          data = 'center',
          nrow = nr_cells,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_align',
      group = 'column_names',
      theme = 'default',
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
      value = list(
        matrix(
          data = 'center',
          nrow = nr_cells,
          ncol = nc_cells
          )
        )
      ),
    dplyr::tibble(
      aesthetic = 'text_just',
      group = 'column_names',
      theme = 'default',
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
  
 return(aes_lookup_df) 
  
}

get_matrix_aesthetics = function() {
  
  get_aesthetic_lookup_df(
    test = data.frame(),
    current = data.frame()
    ) %>%
    .[['aesthetic']] %>%
    unique()

}
