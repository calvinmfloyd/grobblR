

grob_row_class = R6::R6Class(
  "grob_row",
  public = list(
    height = 100,
    width = 100,
    padding = 0,
    proportion = 1,
    contents = list(),
    border = F,
    border_aes_list = ga_list(),
    title = '',
    title_p = 0.2,
    title_aes_list = list(),
    initialize = function(contents,
                          proportion,
                          border,
                          border_aes_list,
                          padding,
                          title,
                          title_p,
                          title_aes_list){
      self$contents = contents
      self$proportion = proportion
      self$border = border
      self$border_aes_list = border_aes_list
      self$padding = padding
      self$title = title
      self$title_p = title_p
      self$title_aes_list = title_aes_list
    }),
  active = list(
    grob = function(contents = self$contents,
                    height = self$height,
                    width = self$width,
                    padding = self$padding,
                    border = self$border,
                    border_aes_list = self$border_aes_list,
                    title = self$title,
                    title_p = self$title_p,
                    title_aes_list = self$title_aes_list){

      if(!all(unlist(lapply(contents, class)) %in% c('R6', 'grob_col'))) stop(
        "Did you remember to wrap all of your objects within grob_row() with grob_col()?",
        call. = F)
      if(title_p < 0 | title_p > 0.5 | !is.numeric(title_p)) stop(
        "title_p in grob_row() must be a numeric value between 0 and 0.5.",
        call. = F)
      if(!is.character(title) | length(title) > 1) stop(
        'title in grob_row() must be a single character string.',
        call. = F)
      if(!is.logical(border)) stop(
        'border in grob_row() must be a TRUE/FALSE value.',
        call. = F)
      if(!is.list(border_aes_list)) stop(
        'border_aes_list in grob_row() must be a list.',
        call. = F)
      if(!is.list(title_aes_list)) stop(
        'title_aes_list in grob_row() must be a list.',
        call. = F)
      if(class(border_aes_list) != 'grob_aes_list') stop(
        'Did you use ga_list() for the border_aes_list in grob_row()?',
        call. = F)
      if(class(title_aes_list) != 'grob_aes_list') stop(
        'Did you use ga_list() for the title_aes_list in grob_row()?',
        call. = F)
      if(!is.numeric(padding) | length(padding) != 1) stop(
        'padding in grob_row() must be a single numeric value in millimeters.',
        call. = F)

      title_present = nchar(title) > 0

      proportions = unlist(lapply(1:length(contents), function(i) contents[[i]]$proportion))
      widths = (proportions/sum(proportions))*(width - 2*padding)
      title_height = height*title_p*title_present
      grob_height = height - title_height
      grob_height_w_padding = grob_height - 2*padding
      title_height_w_padding = title_height - 2*padding
      raw_grobs = grid::gList()

      for(i in 1:length(contents)){
        contents[[i]]$height = grob_height_w_padding
        contents[[i]]$width = widths[i]
        raw_grobs = grid::gList(raw_grobs, contents[[i]]$grob)
      }

      grob = gridExtra::arrangeGrob(
        grobs = raw_grobs,
        layout_matrix = matrix(1:length(raw_grobs), nrow = 1),
        heights = grid::unit(grob_height_w_padding, 'mm'),
        widths = grid::unit(widths, 'mm')
        )
      
      if(title_present) grob = add_title_grob(
        grob = grob,
        title = title,
        title_aes_list = title_aes_list,
        title_p = title_p,
        title_height = title_height,
        padding = padding
        )

      if(border) grob = grid::grobTree(
        grob,
        create_border_grob(
          border_color = border_aes_list$border_color,
          border_width = border_aes_list$border_width,
          border_sides = border_aes_list$border_sides
          )
        )

      grob

    })
  )

