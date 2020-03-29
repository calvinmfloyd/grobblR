

grob_row_class = R6::R6Class(
  classname = "grob_row",
  public = list(
    height = 100,
    width = 100,
    units = 'mm',
    padding = NA_real_,
    padding_proportion = 0.05,
    proportion = 1,
    contents = list(),
    border = F,
    border_aes_list = ga_list(),
    title = '',
    title_p = 0.1,
    title_aes_list = ga_list(),
    caption = '',
    caption_p = 0.05,
    caption_aes_list = ga_list(),
    grob_layout_location = '',
    initialize = function(contents,
                          proportion,
                          border,
                          border_aes_list,
                          padding,
                          padding_proportion,
                          height,
                          title,
                          title_p,
                          title_aes_list,
                          caption,
                          caption_p,
                          caption_aes_list){
      self$contents = contents
      self$proportion = proportion
      self$border = border
      self$border_aes_list = border_aes_list
      self$padding = padding
      self$padding_proportion = padding_proportion
      self$height = height
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
                    border = self$border,
                    border_aes_list = self$border_aes_list,
                    title = self$title,
                    title_p = self$title_p,
                    title_aes_list = self$title_aes_list,
                    caption = self$caption,
                    caption_p = self$caption_p,
                    caption_aes_list = self$caption_aes_list,
                    location = self$grob_layout_location){

      if(!all(unlist(lapply(contents, class)) %in% c('R6', 'grob_col'))) {
        stop(paste0("Did you remember to wrap all of your objects within the ", location, " with grob_col()?"), .call = FALSE)
      }
      
      if(title_p < 0 | title_p > 0.25 | !is.numeric(title_p)) {
        stop(paste0("title_p in the ", location, " must be a numeric value between 0 and 0.25."), .call = FALSE)
      }
      
      if(!is.character(title) | length(title) > 1) {
        stop(paste0("title in the ", location, " must be a single character string."), .call = FALSE)
      }

      if(caption_p < 0 | caption_p > 0.25 | !is.numeric(title_p)) {
        stop(paste0("caption_p in the ", location, " must be a numeric value between 0 and 0.25."), .call = FALSE)
      }
      
      if(!is.character(caption) | length(caption) > 1) {
        stop(paste0('caption in the ', location, ' must be a single character string.'), .call = FALSE)
      }
      
      if(!is.logical(border)) {
        stop(paste0('border in the ', location, ' must be a TRUE/FALSE value.'), .call = FALSE)
      }
      
      if(!is.list(border_aes_list)) {
        stop(paste0('border_aes_list in the ', location, ' must be a list.', call. = FALSE))
      }
      
      if(!is.list(title_aes_list)) {
        stop(paste0('title_aes_list in the ', location, ' must be a list.', call. = FALSE))
      }
      
      if(class(border_aes_list) != 'grob_aes_list') {
        stop(paste0('Did you use ga_list() for the border_aes_list in the ', location,'?', call. = FALSE))
      }
      
      if(class(title_aes_list) != 'grob_aes_list') {
        stop(paste0('Did you use ga_list() for the title_aes_list in the ', location, '?', call. = FALSE))
      }
      
      if(!is.numeric(padding) | length(padding) != 1) {
        stop(paste0('padding in the ', location, ' must be a single numeric value in ', units, '.'), call. = FALSE)
      }

      title_present = nchar(title) > 0
      caption_present = nchar(caption) > 0
      padding = ifelse(!is.na(padding), padding, padding_proportion*min(c(height, width)))
      
      inputted_widths = sapply(1:length(contents), function(i) contents[[i]]$width)
      inputted_proportions = sapply(1:length(contents), function(i) contents[[i]]$proportion)
      
      widths = allot_sizes(
        space_size = width - 2*padding,
        inputted_proportions = inputted_proportions, 
        inputted_sizes = inputted_widths,
        grob_layout_location = location,
        affected_grobs = 'grob-columns',
        measurement = 'width',
        units = units
        )
      
      height_w_padding = height - 2*padding
      title_height = height_w_padding*title_p*title_present
      caption_height = height_w_padding*caption_p*caption_present
      grob_height = height_w_padding - title_height - caption_height
      raw_grobs = grid::gList()

      for(i in 1:length(contents)){
        grob_col_clone = contents[[i]]$clone()
        grob_col_clone$height = grob_height
        grob_col_clone$width = widths[i]
        grob_col_clone$units = units
        grob_col_clone$grob_layout_location = trimws(paste0(location, paste0(", ", scales::ordinal(i), " grob-column")))
        raw_grobs = grid::gList(raw_grobs, grob_col_clone$grob)
      }

      grob = gridExtra::arrangeGrob(
        grobs = raw_grobs,
        layout_matrix = matrix(1:length(raw_grobs), nrow = 1),
        heights = grid::unit(grob_height, units),
        widths = grid::unit(widths, units)
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

      if (border) {
        
        grob = grid::grobTree(
          grob,
          create_border_grob(
            border_color = border_aes_list$border_color,
            border_width = border_aes_list$border_width,
            border_sides = border_aes_list$border_sides
            )
          )
        
      }

      grob

    })
  )

