
grob_col_class = R6::R6Class(
  classname = "grob_col",
  public = list(
    contents = list(),
    proportion = 1,
    height = NA_real_,
    width = NA_real_,
    units = 'mm',
    hjust = 0.5,
    vjust = 0.5,
    padding = NA_real_,
    padding_proportion = 0.05,
    aes_list = ga_list(),
    border = F,
    border_aes_list = ga_list(),
    title = character(),
    title_p = 0.1,
    title_aes_list = ga_list(),
    caption = character(),
    caption_p = 0.05,
    caption_aes_list = ga_list(),
    grob_layout_location = '',
    initialize = function(contents,
                          aes_list,
                          proportion,
                          border,
                          border_aes_list,
                          padding,
                          padding_proportion,
                          title,
                          title_p,
                          title_aes_list,
                          caption,
                          caption_p,
                          caption_aes_list,
                          width,
                          hjust,
                          vjust){
      self$contents = contents
      self$proportion = proportion
      self$aes_list = aes_list
      self$border = border
      self$border_aes_list = border_aes_list
      self$padding = padding
      self$padding_proportion = padding_proportion
      self$title = title
      self$title_p = title_p
      self$title_aes_list = title_aes_list
      self$caption = caption
      self$caption_p = caption_p
      self$caption_aes_list = caption_aes_list
      self$width = width
      self$hjust = hjust
      self$vjust = vjust
    }),
  active = list(
    grob = function(contents = self$contents,
                    aes_list = self$aes_list,
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
                    hjust = self$hjust,
                    vjust = self$vjust,
                    location = self$grob_layout_location){

      content_classes = unlist(lapply(contents, class))
      if (any(content_classes %in% 'grob_row') & !all(content_classes %in% c('grob_row', 'R6'))) {
        stop(
          call. = FALSE,
          paste0(
            "When creating grob-row's within a grob-col, all objects, x, must be wrapped with grob_row(grob_col(x)).\n",
            sprintf(
              'The following classes found were not wrapped with grob_row(grob_col(x)): %s',
              paste(unique(content_classes[!content_classes %in% c('R6', 'grob_row')]), collapse = ', ')
              )
            )
          )
      }
      
      if(!any(content_classes %in% c('R6', 'grob_row')) & length(contents) > 1){
        stop(
          call. = FALSE,
          paste0(
            'Only one non grob_row object allowed within ', location, '.\n',
            paste0(
              'Multiple grob_row classes found within ', location, ': ',
              paste(unique(content_classes), collapse = ', ')
              )
            )
          )
      }
      
      if(title_p < 0 | title_p > 0.5) {
        stop(paste0('title_p in the ', location, ' must be a numeric value between 0 and 0.5.'), call. = FALSE)
      } 
      
      if(!is.character(title)) {
        stop('title in the ', location, ' must be a character string.', call. = FALSE)
      }
      
      if(!is.character(caption)) {
        stop('caption in the ', location, ' must be a character string.', call. = FALSE)
      }

      if(!is.logical(border)) {
        stop(paste0('border in the ', location, ' must be a TRUE/FALSE value.'), call. = FALSE)
      }
      
      if(class(aes_list) != 'grob_aes_list') {
        stop(paste0('Did you use ga_list() for the aes_list in the ', location, '?'), call. = FALSE)
      }
      
      if(class(border_aes_list) != 'grob_aes_list') {
        stop(paste0('Did you use ga_list() for the border_aes_list in the ', location, '?'), call. = FALSE)
      }
      
      if(class(title_aes_list) != 'grob_aes_list') {
        stop(paste0('Did you use ga_list() for the title_aes_list in the ', location, '?'), call. = FALSE)
      }
      
      if(!is.numeric(padding) | length(padding) != 1) {
        stop(paste0('padding in the ', location, ' must be a single numeric value in ', units, '.'), call. = FALSE)
      }

      title_present = nchar(title) > 0
      caption_present = nchar(caption) > 0
      padding = ifelse(!is.na(padding), padding, padding_proportion*min(c(height, width)))
      width_w_padding = width - 2*padding
      height_w_padding = height - 2*padding
      title_height = height_w_padding*title_p*title_present
      caption_height = height_w_padding*caption_p*caption_present
      grob_height = height_w_padding - title_height - caption_height
      raw_grobs = grid::gList()

      if (is(contents[[1]], 'grob_row')) {
        
        inputted_heights = sapply(1:length(contents), function(i) contents[[i]]$height)
        inputted_proportions = sapply(1:length(contents), function(i) contents[[i]]$proportion)
        heights = allot_sizes(
          space_size = grob_height,
          inputted_proportions = inputted_proportions,
          inputted_sizes = inputted_heights,
          grob_layout_location = location,
          affected_grobs = 'grob-rows',
          measurement = 'height',
          units = units
          )

      } else {
        
        heights = grob_height
        
      }

      for(i in 1:length(contents)){

        if (is(contents[[i]], 'grob_row')) {
          
          grob_row_clone = contents[[i]]$clone()
          grob_row_clone$height = heights[i]
          grob_row_clone$width = width
          grob_row_clone$units = units
          grob_row_clone$grob_layout_location = trimws(paste0(location, paste0(", ", scales::ordinal(i), " grob-row")))
          raw_grobs = grid::gList(raw_grobs, grob_row_clone$grob)

        } else {

          ctg = convert_to_grob(
            x = contents[[i]],
            height = heights,
            width = width_w_padding,
            aes_list = aes_list
            )

          g = gridExtra::arrangeGrob(
            grobs = grid::gList(grid::nullGrob(), grid::nullGrob(), grid::nullGrob(), grid::nullGrob(), ctg),
            layout_matrix = cbind(3, rbind(1, 5, 2), 4),
            heights = grid::unit(c(2*padding*(1-vjust), heights, 2*padding*vjust), units),
            widths = grid::unit(c(2*padding*hjust, width_w_padding, 2*padding*(1-hjust)), units)
            )
          
          raw_grobs = grid::gList(raw_grobs, g)
        }
      }

      grob = gridExtra::arrangeGrob(
        grobs = raw_grobs,
        layout_matrix = matrix(1:length(raw_grobs), ncol = 1),
        heights = grid::unit(heights, units),
        widths = grid::unit(width, units)
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

      if(border) grob = grid::grobTree(
        grob,
        create_border_grob(
          border_color = border_aes_list$border_color,
          border_width = border_aes_list$border_width,
          border_sides = border_aes_list$border_sides
          )
        )

      return(grob)

    })
  )
