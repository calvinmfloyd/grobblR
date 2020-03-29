
grob_layout_class = R6::R6Class(
  classname = "grob_layout",
  public = list(
    height = 100,
    width = 100,
    units = 'mm',
    padding = NA_real_,
    padding_proportion = 0.1,
    contents = list(),
    title = character(),
    title_p = 0.2,
    title_aes_list = ga_list(),
    caption = character(),
    caption_p = 0.15,
    caption_aes_list = ga_list(),
    page_number = '',
    initialize = function(contents,
                          height,
                          width,
                          units,
                          padding,
                          padding_proportion,
                          page_number,
                          title,
                          title_p,
                          title_aes_list,
                          caption,
                          caption_p,
                          caption_aes_list){
      self$contents = contents
      self$height = height
      self$width = width
      self$units = units
      self$padding = padding
      self$padding_proportion = padding_proportion
      self$page_number = page_number
      self$title = title
      self$title_p = title_p
      self$title_aes_list = title_aes_list
      self$caption = caption
      self$caption_p = caption_p
      self$caption_aes_list = caption_aes_list

    }),
  active = list(
    grob = function(contents = self$contents,
                    height = self$height,
                    width = self$width,
                    units = self$units,
                    padding = self$padding,
                    padding_proportion = self$padding_proportion,
                    row_number = self$row_number,
                    page_number = self$page_number,
                    title = self$title,
                    title_p = self$title_p,
                    title_aes_list = self$title_aes_list,
                    caption = self$caption,
                    caption_p = self$caption_p,
                    caption_aes_list = self$caption_aes_list) {

      if(!all(unlist(lapply(contents, class)) %in% c('R6', 'grob_row'))) {
        stop(
          call. = FALSE,
          paste0(
            "All objects on the outermost grob layer must either be a grob-layout or they ",
            "must be wrapped with grob_row(), and its objects must be wrapped with grob_col()."
            )
          )
      }
      
      if(title_p < 0 | title_p > 0.25 | !is.numeric(title_p)) {
        stop("title_p in grob_row() must be a numeric value between 0 and 0.25.", .call = FALSE)
      }
      
      if(!is.character(title) | length(title) > 1) {
        stop('title in grob_layout() must be a single character string.', .call = FALSE)
      }

      if(caption_p < 0 | caption_p > 0.25 | !is.numeric(caption_p)) {
        stop("caption_p in grob_row() must be a numeric value between 0 and 0.25.", .call = FALSE)
      }
      
      if(!is.character(caption) | length(caption) > 1) {
        stop('caption in grob_layout() must be a single character string.', .call = FALSE)
      }
            
      if(length(page_number) != 1) {
        stop('page_number in grob_layout() must be a single value that can be converted to an integer.', .call = FALSE)
      }
      
      if(any(!is.numeric(height)) | length(height) != 1) {
        stop(paste0('height in grob_layout() must be a single numeric value in ', units, '.'), .call = FALSE)
      }
      
      if(any(!is.numeric(width)) | length(width) != 1) {
        stop(paste0('width in grob_layout() must be a single numeric value in ', units, '.'), .call = FALSE)
      }
      
      if(any(!is.numeric(padding)) | length(padding) != 1) {
        stop(paste0('padding in grob_layout() must be a single numeric value in ', units, '.'), .call = FALSE)
      }
      
      if(class(title_aes_list) != 'grob_aes_list') {
        stop('Did you use ga_list() for the title_aes_list in grob_layout()?', .call = FALSE)
      }
      
      if(class(caption_aes_list) != 'grob_aes_list') {
        stop('Did you use ga_list() for the caption_aes_list in grob_layout()?', .call = FALSE)
      }

      page_number = suppressWarnings(as.integer(page_number))
      page_number = ifelse(is.na(page_number), '', as.character(page_number))
      
      # Initializing Variables ----
      title_present = nchar(title) > 0
      caption_present = nchar(caption) > 0
      padding = ifelse(!is.na(padding), padding, padding_proportion*min(c(height, width)))
      width_w_padding = width - 2*padding
      height_w_padding = height - 2*padding
      title_height = height_w_padding*title_p*title_present
      caption_height = height_w_padding*caption_p*caption_present
      grob_height = height_w_padding - title_height - caption_height

      # Creating the Layout Matrix ----
      nr = length(contents)
      layout_matrix = matrix(1:nr, ncol = 1)
      inputted_row_heights = sapply(1:nr, function(i) contents[[i]]$height)
      inputted_proportions = sapply(1:nr, function(i) contents[[i]]$proportion)
      
      row_heights = allot_sizes(
        space_size = grob_height,
        inputted_proportions = inputted_proportions,
        inputted_sizes = inputted_row_heights,
        grob_layout_location = 'grob-layout',
        affected_grobs = 'outermost grob-rows',
        measurement = 'height',
        units = units
        )

      # Readjusting Grob Widths to fit in the given Page Height and Page Width ----
      raw_grobs = grid::gList()
      for(i in 1:nr){
        
        grob_row_clone = contents[[i]]$clone()
        grob_row_clone$height = row_heights[i]
        grob_row_clone$width = width_w_padding
        grob_row_clone$units = units
        grob_row_clone$grob_layout_location = trimws(paste0(scales::ordinal(i), " grob-row"))
        raw_grobs = grid::gList(raw_grobs, grob_row_clone$grob)
        
      }

      grob = gridExtra::arrangeGrob(
        grobs = raw_grobs,
        heights = grid::unit(row_heights, units),
        widths = grid::unit(width_w_padding, units),
        layout_matrix = layout_matrix
        )

      if (title_present) {
        
        grob = add_title_grob(
          grob = grob,
          title = title,
          title_aes_list = title_aes_list,
          title_p = title_p,
          title_height = title_height,
          units = units
          )
        
      }
      
      if (caption_present) {
        
        grob = add_caption_grob(
          grob = grob,
          caption = caption,
          caption_aes_list = caption_aes_list,
          caption_p = caption_p,
          caption_height = caption_height,
          units = units
          )
        
      }
      
      grob = add_page_number(
        grob = grob,
        page_number = page_number,
        padding = padding
        )

      grob

    })
  )
