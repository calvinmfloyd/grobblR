
grob_col_class <- R6::R6Class(
  "grob_col",
  public = list(
    contents = list(),
    proportion = 1,
    height = 100,
    width = 100,
    hjust = 0.5,
    vjust = 0.5,
    padding = 0,
    aes_list = list(),
    border = F,
    border_sides = 'top, bottom, left, right',
    border_aes_list = list(),
    title = character(),
    title_p = 0.2,
    title_aes_list = list(),
    initialize = function(contents,
                          aes_list,
                          proportion,
                          border,
                          border_sides,
                          border_aes_list,
                          padding,
                          title,
                          title_p,
                          title_aes_list,
                          hjust,
                          vjust){
      self$contents <- contents
      self$proportion <- proportion
      self$aes_list <- aes_list
      self$border <- border
      self$border_aes_list <- border_aes_list
      self$border_sides <- border_sides
      self$padding <- padding
      self$title <- title
      self$title_p <- title_p
      self$title_aes_list <- title_aes_list
      self$hjust <- hjust
      self$vjust <- vjust
    }),
  active = list(
    grob = function(contents = self$contents,
                    aes_list = self$aes_list,
                    height = self$height,
                    width = self$width,
                    padding = self$padding,
                    border = self$border,
                    border_sides = self$border_sides,
                    border_aes_list = self$border_aes_list,
                    title = self$title,
                    title_p = self$title_p,
                    title_aes_list = self$title_aes_list,
                    hjust = self$hjust,
                    vjust = self$vjust){

      content_classes <- unlist(lapply(contents, class))
      if(any(content_classes %in% c('R6', 'grob_row')) & !all(content_classes %in% c('R6', 'grob_row'))){
        stop(paste0(
          "When creating grob_row's within a grob_col, all objects, x, must be wrapped with grob_row(grob_col(x)).\n",
          sprintf(
            'Following classes found were not wrapped with grob_row(grob_col(x)): %s',
            paste(unique(content_classes[!content_classes %in% c('R6', 'grob_row')]), collapse = ', '))),
          call. = F)}
      if(!any(content_classes %in% c('R6', 'grob_row')) & length(contents) > 1){
        stop(paste0(
          "Only one non grob_row object allowed within grob_col.\n",
          sprintf(
            'Multiple grob_row classes found within grob_col: %s',
            paste(unique(content_classes), collapse = ', '))),
          call. = F)}
      if(hjust < 0 | hjust > 1) stop(
        "hjust in grob_col() must be a numeric value between 0 and 1.",
        call. = F)
      if(vjust < 0 | vjust > 1) stop(
        "vjust in grob_col() must be a numeric value between 0 and 1.",
        call. = F)
      if(title_p < 0 | title_p > 0.5) stop(
        "title_p in grob_col() must be a numeric value between 0 and 0.5.",
        call. = F)
      if(!is.character(title)) stop(
        "title in grob_col() must be a character string.",
        call. = F)
      if(!is.logical(border)) stop(
        "border in grob_col() must be a TRUE/FALSE value.",
        call. = F)
      if(!is.list(aes_list)) stop(
        "aes_list in grob_col() must be a list.",
        call. = F)
      if(!is.list(border_aes_list)) stop(
        "border_aes_list in grob_col() must be a list.",
        call. = F)
      if(!is.list(title_aes_list)) stop(
        "title_aes_list in grob_col() must be a list.",
        call. = F)
      if(!is.character(border_sides)) stop(
        "border_sides in grob_col() must be a character string with 'top', 'bottom', 'left' or 'right' separated with ', '.",
        call. = F)
      if(class(aes_list) != 'grob_aes_list') stop(
        'Did you use ga_list() for the aes_list in grob_col()?',
        call. = F)
      if(class(border_aes_list) != 'grob_aes_list') stop(
        'Did you use ga_list() for the border_aes_list in grob_col()?',
        call. = F)
      if(class(title_aes_list) != 'grob_aes_list') stop(
        'Did you use ga_list() for the title_aes_list in grob_col()?',
        call. = F)
      if(!is.numeric(padding) | length(padding) != 1) stop(
        'padding in grob_row() must be a single numeric value in millimeters.',
        call. = F)

      title_present <- nchar(title) > 0
      width_w_padding <- width - 2*padding
      title_height <- height*title_p*title_present
      grob_height <- height - title_height
      grob_height_w_padding <- grob_height - 2*padding
      title_height_w_padding <- title_height - 2*padding
      raw_grobs <- grid::gList()

      for(i in 1:length(contents)){

        if(R6::is.R6(contents[[i]])){

          height_proportions <- sapply(1:length(contents), function(i) contents[[i]]$proportion)
          contents[[i]]$height <- grob_height*(height_proportions/sum(height_proportions))[i]
          contents[[i]]$width <- width
          contents[[i]]$padding <- padding
          raw_grobs <- grid::gList(raw_grobs, contents[[i]]$grob)

        } else {

          height_proportions <- 1

          ctg <- convert_to_grob(
            x = contents[[i]],
            height = grob_height_w_padding,
            width = width_w_padding,
            aes_list = aes_list)

          g <- gridExtra::arrangeGrob(
            grobs = grid::gList(grid::nullGrob(), grid::nullGrob(), grid::nullGrob(), grid::nullGrob(), ctg)
            ,layout_matrix = cbind(3,rbind(1, 5, 2), 4)
            ,heights = grid::unit(c(2*padding*(1-vjust), grob_height_w_padding, 2*padding*vjust), 'mm')
            ,widths = grid::unit(c(2*padding*hjust, width_w_padding, 2*padding*(1-hjust)), 'mm'))

          raw_grobs <- grid::gList(raw_grobs, g)

        }
      }

      grob <- gridExtra::arrangeGrob(
        grobs = raw_grobs,
        layout_matrix = matrix(1:length(raw_grobs), ncol = 1),
        heights = grid::unit(grob_height*(height_proportions/sum(height_proportions)), 'mm'),
        widths = grid::unit(width, 'mm'))

      if(title_present) grob <- add_title_grob(
          grob = grob,
          title = title,
          title_aes_list = title_aes_list,
          title_p = title_p,
          title_height = title_height_w_padding,
          total_height = height,
          width = width_w_padding)

      if(border) grob <- grid::grobTree(
        grob,
        create_border_grob(
          border_color = border_aes_list$border_color,
          border_width = border_aes_list$border_width,
          border_sides = border_sides))

      grob

    }))
