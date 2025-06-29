
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
    border = FALSE,
    border_aes_list = ga_list(),
    title = character(),
    title_p = 0.1,
    title_aes_list = ga_list(),
    title_height = numeric(),
    caption = character(),
    caption_p = 0.05,
    caption_aes_list = ga_list(),
    caption_height = numeric(),
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
                          title_height,
                          caption,
                          caption_p,
                          caption_aes_list,
                          caption_height,
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
      self$title_height = title_height
      self$caption = caption
      self$caption_p = caption_p
      self$caption_aes_list = caption_aes_list
      self$caption_height = caption_height
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
                    title_height = self$title_height,
                    title_aes_list = self$title_aes_list,
                    caption = self$caption,
                    caption_p = self$caption_p,
                    caption_height = self$caption_height,
                    caption_aes_list = self$caption_aes_list,
                    hjust = self$hjust,
                    vjust = self$vjust,
                    location = self$grob_layout_location){

      content_classes = unlist(lapply(contents, class))
      if (any(content_classes %in% 'grob_row') & !all(content_classes %in% c('grob_row', 'R6'))) {
        
        errored_classes = unique(content_classes[!content_classes %in% c('R6', 'grob_row')])
        error_msg = glue::glue("
          When creating grob-row's within a grob-col, all objects, x, must be wrapped with grob_row(grob_col(x)).
          The following classes found were not wrapped with grob_row(grob_col(x)): {paste(errored_classes, collapse = ', ')}
          ")
        stop(error_msg, call. = FALSE)
        
      }
      
      if (!any(content_classes %in% c('R6', 'grob_row')) & length(contents) > 1) {
        
        error_msg = glue::glue("
          Only one non grob_row object allowed with {location}.
          Multiple non grob_row classes found: {paste(unique(content_classes), collapse = ', ')}
          ")
        stop(error_msg, call. = FALSE)
        
      }
      
      if (title_p < 0 | title_p > 0.5) {
        
        error_msg = glue::glue("title_p in the {location} must be a numeric value between 0 and 0.5.")
        stop(error_msg, call. = FALSE)
        
      } 
      
      if (!is.character(title)) {
        
        error_msg = glue::glue("title in the {location} must be a character string.")
        stop(error_msg, call. = FALSE)
        
      }
      
      if (!is.character(caption)) {
        
        error_msg = glue::glue("caption in the {location} must be a character string.")
        stop(error_msg, call. = FALSE)
        
      }

      if (!is.logical(border)) {
        
        error_msg = glue::glue("border in the {location} must be a TRUE/FALSE value.")
        stop(error_msg, call. = FALSE)
        
      }
      
      if (!inherits(aes_list, 'grob_aes_list')) {
        
        error_msg = glue::glue("Did you use ga_list() for the aes_list in the {location}?")
        stop(error_msg, call. = FALSE)
        
      }
      
      if (!inherits(border_aes_list, 'grob_aes_list')) {
        
        error_msg = glue::glue("Did you use ga_list() for the border_aes_list in the {location}?")
        stop(error_msg, call. = FALSE)
        
      }
      
      if (!inherits(title_aes_list, 'grob_aes_list')) {
        
        error_msg = glue::glue("Did you use ga_list() for the title_aes_list in the {location}?")
        stop(error_msg, call. = FALSE)
        
      }
      
      if (!is.numeric(padding) | length(padding) != 1) {
        
        error_msg = glue::glue("padding in the {location} must be a single numeric value in millimeters.")
        stop(error_msg, call. = FALSE)
        
      }

      title_present = nchar(title) > 0
      caption_present = nchar(caption) > 0
      padding = ifelse(!is.na(padding), padding, padding_proportion*min(c(height, width)))
      width_w_padding = width - 2*padding
      height_w_padding = height - 2*padding

      title_grob_caption_heights = allot_sizes(
        space_size = height_w_padding,
        inputted_proportions = c(
          ifelse(title_present, title_p, 0),
          1, 
          ifelse(caption_present, caption_p, 0)
          ),
        inputted_sizes = c(
          title_height*title_present,
          NA_real_,
          caption_height*caption_present
          ),
        grob_layout_location = location,
        affected_grobs = "title / grob / caption",
        measurement = 'height',
        units = units
        )
      
      title_height = title_grob_caption_heights[1]
      grob_height = title_grob_caption_heights[2]
      caption_height = title_grob_caption_heights[3]
      
      raw_grobs = grid::gList()

      if (methods::is(contents[[1]], 'grob_row')) {
        
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

        if (methods::is(contents[[i]], 'grob_row')) {
          
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
          title_height = title_height
          )
        
      }
      
      if (caption_present) {
        
        grob = add_caption_grob(
          grob = grob,
          caption = caption,
          caption_aes_list = caption_aes_list,
          caption_height = caption_height
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

      return(grob)

    })
  )
