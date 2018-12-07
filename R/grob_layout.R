#' The main \code{grobblR} function which contains and organizes \code{\link{grob_col}}'s and \code{\link{grob_row}}'s, giving the overall grob-layout its shape.
#'
#' @param ... The combination of grob_row's and grob_col's which will help give the main grob outputted its shape and look.
#' @param height The numeric height of the grob in mm. Default is 280 mm - which is the height of an upright 8.5 x 11 in piece of copy paper.
#' @param width The numeric width of the grob in mm. Default is 216 mm - which is the width of an upright 8.5 x 11 in piece of copy paper.
#' @param padding The numeric amount of padding around the edge of the grob  in mm. Default is 10 mm.
#' @param grob_padding The numeric amount of padding for each individual grob within their designated grid of area in mm. Default is 2 mm.
#' @param title A character string which will be displayed as the title of the grob layout.
#' @param title_aes_list A list which contains desired aesthetics for the title of the grob layout. Elements of this list are treated the same way as \code{aes_list} - see \code{\link{grob_matrix}} for more details on its possible elements.
#' @param title_p The numeric proportion of height within the grob layout and its allotted space which will be used by the title grob. A numeric value between 0 and 1. Defaults to 0.1.
#' @param row_heights If the user wants to designate specific row heights instead relying on the proportions within the outermost grob_row functions, set this parameter equal to a vector of numeric values corresponding to the individual row heights in mm. Length must be equal to the number of grob_row function's on the upper most level of the grob.
#' @return An R6 class containing all information necessary to create the overall grob-layout. The grob itself is called with \code{grob_layout()$grob}.
#' @example
#' gl <- grob_layout(
#'   grob_row(grob_col(1), grob_col(2)),
#'   grob_row(grob_col(3)))
#' # to retrieve the grob-layout
#' gl$grob
#' @export

grob_layout <- function(...,
                        height = 280,
                        width = 216,
                        padding = 10,
                        grob_padding = 2,
                        title = '',
                        title_aes_list = list(),
                        title_p = 0.1,
                        page_number = '',
                        row_heights = c()){

  grob_layout_class <- R6::R6Class(
    "grob_layout",
    public = list(
      height = 0,
      width = 0,
      padding = 0,
      grob_padding = 0,
      grob_classes = list(),
      title = character(),
      title_proportion = 0.2,
      title_aes_list = list(),
      page_number = '',
      row_heights = c(),
      initialize = function(grob_classes,
                            height,
                            width,
                            padding,
                            grob_padding,
                            page_number,
                            row_heights,
                            title,
                            title_proportion,
                            title_aes_list){

        self$grob_classes <- grob_classes
        self$height <- height
        self$width <- width
        self$padding <- padding
        self$grob_padding <- grob_padding
        self$row_heights <- row_heights
        self$page_number <- page_number
        self$title <- title
        self$title_proportion <- title_proportion
        self$title_aes_list <- title_aes_list

      }),
    active = list(
      grob = function(gc = self$grob_classes,
                      ht = self$height,
                      wth = self$width,
                      pad = self$padding,
                      grob_pad = self$grob_padding,
                      row_number = self$row_number,
                      row_heights = self$row_heights,
                      page_number = self$page_number,
                      title = self$title,
                      title_proportion = self$title_proportion,
                      title_aes_list = self$title_aes_list){

        if(!all(unlist(lapply(gc, class)) %in% c('R6', 'grob_row'))) stop(paste0(
          "All objects on the outermost grob layer must be wrapped with grob_row(), ",
          "and its objects must be wrapped with grob_col()."),
          call. = FALSE)

        if(title_proportion < 0 | title_proportion > 0.5 | !is.numeric(title_proportion)) stop(
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

        if(any(!is.numeric(ht)) | length(ht) != 1) stop(
          'height in grob_layout() must be a single numeric value in millimeters.',
          call. = F)

        if(any(!is.numeric(width)) | length(width) != 1) stop(
          'width in grob_layout() must be a single numeric value in millimeters.',
          call. = F)

        if(any(!is.numeric(padding)) | length(padding) != 1) stop(
          'padding in grob_layout() must be a single numeric value in millimeters.',
          call. = F)

        if(any(!is.numeric(grob_padding)) | length(grob_padding) != 1) stop(
          'grob_padding in grob_layout() must be a single numeric value in millimeters.',
          call. = F)

        page_number <- suppressWarnings(as.integer(page_number))
        page_number <- ifelse(is.na(page_number), '', as.character(page_number))

        # Initializing Variables ----
        ph <- ht - 2*pad
        pw <- wth - 2*pad
        gl_title_present <- nchar(title) > 0

        # Creating the Layout Matrix ----
        nr <- length(gc)
        layout_matrix <- matrix(1:nr, ncol = 1)

        # Calculating Row Heights ----
        rh_inputted <- length(row_heights) > 0
        rh_wrong_length <- length(row_heights) != nr

        if(rh_inputted & rh_wrong_length) warning(paste0(
          sprintf("row_heights param in grob_layout() must have length of %d, ", nr),
          sprintf("since there are %d grob-rows on the outermost layer. ", nr),
          "Using proportions to calculate row heights."),
          call. = F)

        if(rh_wrong_length){
          row_props <- sapply(1:nr, function(i) gc[[i]]$proportion)
          row_heights <- (ph - ph*gl_title_present*title_proportion)*(row_props/sum(row_props))
        }

        # Readjusting Grob Widths to fit in the given Page Height and Page Width ----
        raw_grobs <- grid::gList()
        for(i in 1:nr){
          gc[[i]]$height <- row_heights[i]
          gc[[i]]$width <- pw
          gc[[i]]$padding <- grob_pad
          raw_grobs <- grid::gList(raw_grobs, gc[[i]]$grob)
        }

        grob <- gridExtra::arrangeGrob(
          grobs = raw_grobs,
          heights = grid::unit(row_heights, 'mm'),
          widths = grid::unit(pw, 'mm'),
          layout_matrix = layout_matrix)

        if(gl_title_present){

          white_space_p <- 0.15

          title_grob <- grob_matrix(
            df = matrix(title, ncol = 1, nrow = 1),
            height = ph*title_proportion - ph*title_proportion*white_space_p,
            width = pw,
            aes_list = title_aes_list,
            m_type = 5)

          grob <- gridExtra::arrangeGrob(
            grobs = grid::gList(title_grob, grid::nullGrob(), grob),
            heights = grid::unit(c(
              ph*title_proportion - ph*title_proportion*white_space_p,
              ph*title_proportion*white_space_p,
              ph - ph*title_proportion), 'mm'),
            widths = grid::unit(pw, 'mm'),
            layout_matrix = matrix(1:3, ncol = 1))

        }

        pn_pad_pct <- 0.9
        page_number_grob <- convert_to_grob(
          x = page_number,
          height = pad*pn_pad_pct,
          width = pad*pn_pad_pct,
          aes_list = list(
            txt_color = 'gray40',
            bg_color = NA))

        layout_matrix_w_pn <- matrix(NA, nrow = 4, ncol = 4)
        layout_matrix_w_pn[2,2] <- 1
        layout_matrix_w_pn[3,3] <- 2

        grob <- gridExtra::arrangeGrob(
          grobs = grid::gList(grob, page_number_grob),
          heights = grid::unit(
            c(pad, ph, pad*pn_pad_pct, (pad - pad*pn_pad_pct)),
            'mm'),
          widths = grid::unit(
            c(pad, pw, pad*pn_pad_pct, (pad - pad*pn_pad_pct)),
            'mm'),
          layout_matrix = layout_matrix_w_pn)

        grob

    }))

  grob_layout_class$new(
    grob_classes = unlist(list(...)),
    height = height,
    width = width,
    padding = padding,
    grob_padding = grob_padding,
    page_number = page_number,
    row_heights = row_heights,
    title = title,
    title_proportion = title_p,
    title_aes_list = title_aes_list)

}
