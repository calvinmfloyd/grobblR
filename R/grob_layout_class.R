
grob_layout_class = R6::R6Class(
  "grob_layout",
  public = list(
    height = 100,
    width = 100,
    padding = 0,
    contents = list(),
    title = character(),
    title_p = 0.2,
    title_aes_list = ga_list(),
    page_number = '',
    row_heights = c(),
    initialize = function(contents,
                          height,
                          width,
                          padding,
                          page_number,
                          row_heights,
                          title,
                          title_p,
                          title_aes_list){
      self$contents = contents
      self$height = height
      self$width = width
      self$padding = padding
      self$row_heights = row_heights
      self$page_number = page_number
      self$title = title
      self$title_p = title_p
      self$title_aes_list = title_aes_list
    }),
  active = list(
    grob = function(contents = self$contents,
                    height = self$height,
                    width = self$width,
                    padding = self$padding,
                    row_number = self$row_number,
                    row_heights = self$row_heights,
                    page_number = self$page_number,
                    title = self$title,
                    title_p = self$title_p,
                    title_aes_list = self$title_aes_list){

      if(!all(unlist(lapply(contents, class)) %in% c('R6', 'grob_row'))) stop(paste0(
        "All objects on the outermost grob layer must be wrapped with grob_row(), ",
        "and its objects must be wrapped with grob_col()."),
        call. = F)
      if(title_p < 0 | title_p > 0.5 | !is.numeric(title_p)) stop(
        "title_p in grob_row() must be a numeric value between 0 and 0.5.",
        call. = F)
      if(!is.character(title) | length(title) > 1) stop(
        'title in grob_layout() must be a single character string.',
        call. = F)
      if(!is.list(title_aes_list)) stop(
        'title_aes_list in grob_layout() must be a list.',
        call. = F)
      if(length(page_number) != 1) stop(
        'page_number in grob_layout() must be a single value that can be converted to an integer.',
        call. = F)
      if(length(row_heights) > 0) if(any(!is.numeric(row_heights))) stop(
        'row_heights in grob_layout() must all be numeric values in millimeters.',
        call. = F)
      if(any(!is.numeric(height)) | length(height) != 1) stop(
        'height in grob_layout() must be a single numeric value in millimeters.',
        call. = F)
      if(any(!is.numeric(width)) | length(width) != 1) stop(
        'width in grob_layout() must be a single numeric value in millimeters.',
        call. = F)
      if(any(!is.numeric(padding)) | length(padding) != 1) stop(
        'padding in grob_layout() must be a single numeric value in millimeters.',
        call. = F)
      if(class(title_aes_list) != 'grob_aes_list') stop(
        'Did you use ga_list() for the title_aes_list in grob_layout()?',
        call. = F)

      page_number = suppressWarnings(as.integer(page_number))
      page_number = ifelse(is.na(page_number), '', as.character(page_number))

      # Initializing Variables ----
      width_w_padding = width - 2*padding
      height_w_padding = height - 2*padding
      title_present = nchar(title) > 0
      title_height = height_w_padding*title_present*title_p
      grob_height_w_padding = height_w_padding - title_height

      # Creating the Layout Matrix ----
      nr = length(contents)
      layout_matrix = matrix(1:nr, ncol = 1)

      # Calculating Row Heights ----
      rh_inputted = length(row_heights) > 0
      rh_wrong_length = length(row_heights) != nr

      if(rh_inputted & rh_wrong_length) warning(paste0(
        sprintf("row_heights param in grob_layout() must have length of %d, ", nr),
        sprintf("since there are %d grob-rows on the outermost layer. ", nr),
        "Using proportions to calculate row heights."),
        call. = F)

      if(rh_wrong_length){
        row_proportions = sapply(1:nr, function(i) contents[[i]]$proportion)
        row_heights = grob_height_w_padding*(row_proportions/sum(row_proportions))
      }

      # Readjusting Grob Widths to fit in the given Page Height and Page Width ----
      raw_grobs = grid::gList()
      for(i in 1:nr){
        contents[[i]]$height = row_heights[i]
        contents[[i]]$width = width_w_padding
        raw_grobs = grid::gList(raw_grobs, contents[[i]]$grob)
      }

      grob = gridExtra::arrangeGrob(
        grobs = raw_grobs,
        heights = grid::unit(row_heights, 'mm'),
        widths = grid::unit(width_w_padding, 'mm'),
        layout_matrix = layout_matrix
        )

      if(title_present) grob = add_title_grob(
        grob = grob,
        title = title,
        title_aes_list = title_aes_list,
        title_p = title_p,
        title_height = title_height,
        padding = padding
        )

      grob = add_page_number(
        grob = grob,
        page_number = page_number,
        padding = padding
        )

      grob

    })
  )
