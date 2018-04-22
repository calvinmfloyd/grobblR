
# Grob Layout ----
grob_layout <- function(...,
                        layout_matrix_only = FALSE,
                        page_height = 280,
                        page_width = 216,
                        page_padding = 5,
                        grob_padding = 5,
                        row_heights = c(),
                        column_widths = c()){
  
  # Least Common Multiple Function used during the creation of the Layout Matrix ----
  lcm <- function(v){
    i <- v[which(v == max(v))[1]]
    while(TRUE) {
      if(all(i %% v == 0)) return(i)
      i <- i + 1
    }
  }
  # ----
  
  # Initializing Variables ----
  ph <- page_height - 2*page_padding
  pw <- page_width - 2*page_padding
  g_info <- list(...)
  # ----
  
  # Creating the Layout Matrix, using LCM ----
  nr <- length(g_info)
  # grobs_per_row <- sapply(1:nr, function(i) length(g_info[[i]]))
  # row_width_sums <- sapply(
  #   1:nr,
  #   function(i){
  #     sum(sapply(1:grobs_per_row[i], function(j) g_info[[i]][[j]]$width))
  # })  
  # 
  # row_ind_widths <- sapply(
  #   1:nr,
  #   function(i){
  #     sapply(1:grobs_per_row[i], function(j) g_info[[i]][[j]]$width)
  # })
  # 
  # nc <- lcm(row_width_sums)
  # lm_cols_by_row <- sapply(1:nr, function(i) row_ind_widths[[i]]*(nc %/% row_width_sums[i]))
  # 
  # lm_vec <- unlist(sapply(
  #   1:nr,
  #   function(i){
  #     unlist(sapply(
  #       1:grobs_per_row[i],
  #       function(j) c(1, rep(0, lm_cols_by_row[[i]][j] - 1))
  #     ))
  #   }))
  
  # layout_matrix <- matrix(
  #   cumsum(lm_vec),
  #   nrow = nr,
  #   ncol = nc,
  #   byrow = TRUE)
  
  layout_matrix <- matrix(1:nr, ncol = 1)
  
  # if(layout_matrix_only) return(layout_matrix)
  # ----
  
  # Calculating Row Heights ----
  rh_inputted <- length(row_heights) > 0
  rh_wrong_length <- length(row_heights) != nr
  if(rh_inputted & rh_wrong_length) warning(sprintf("row_heights param must have length of %d.", nr))
  if(rh_wrong_length){
    row_props <- sapply(1:nr, function(i) g_info[[i]]$proportion)
    row_heights <- ph*(row_props/sum(row_props))
  }
  # ----
  
  # Calculating Column Widths ----
  # cw_inputted <- length(column_widths) > 0
  # cw_wrong_length <- length(column_widths) != nc
  # if(cw_inputted & cw_wrong_length){
  #   warning(sprintf("column_widths param must have length of %d.\nUsing automatic column sizing.", nc))
  # }
  # if(cw_wrong_length) column_widths <- rep(pw/nc, nc)
  column_widths <- pw
  # ----
  
  # Readjusting Grob Widths to fit in the given Page Height and Page Width ----
  raw_grobs <- gList()
  # for(i in 1:nr){
  #   for(j in 1:grobs_per_row[i]){
  #     grob_number <- unique(layout_matrix[i,])[j]
  #     g_info[[i]][[j]]$more_args[['tot_height']] <- row_heights[i] - 2*grob_padding
  #     g_info[[i]][[j]]$more_args[['tot_width']] <- sum(column_widths[which(layout_matrix[i,] == grob_number)]) - 2*grob_padding
  #     raw_grobs <- gList(raw_grobs, convert_to_grob(g_info[[i]][[j]]$x, g_info[[i]][[j]]$more_args))
  # }}
  for(i in 1:nr){
    g_info[[i]]$height <- row_heights[i]
    g_info[[i]]$width <- pw
    g_info[[i]]$padding <- grob_padding
    raw_grobs <- gList(raw_grobs, g_info[[i]]$grob)
  }
  # ----
  
  grob <- arrangeGrob(
    grobs = raw_grobs,
    heights = unit(row_heights, 'mm'),
    widths = unit(column_widths, 'mm'),
    layout_matrix = layout_matrix)
  
  list(
    'grob' = grob,
    'row_heights' = unit(row_heights, 'mm'),
    'column_widths' = unit(column_widths, 'mm'),
    'layout_matrix' = layout_matrix,
    'total_height' = unit(page_height, 'mm'),
    'total_width' = unit(page_width, 'mm'),
    'page_padding' = unit(page_padding, 'mm'),
    'grob_padding' = unit(grob_padding, 'mm'))
}
# ----

# is.grobbable function ----
is.grobbable <- function(x){
  is.character(x) | is.ggplot(x) | is.matrix(x) | is.data.frame(x) | is.na(x)
}
# ----

# Grob Row / Grob Row Class ----
grob_row <- function(..., prop = 1){
  
  grob_row_class <- R6Class("grob_row",
    public = list(
      height = 0,
      width = 0,
      padding = 0,
      proportion = 1,
      grob_classes = list(),
      initialize = function(grob_classes, proportion){
        self$grob_classes <- grob_classes
        self$proportion <- proportion
        }
    ),
    active = list(
      grob = function(gc = self$grob_classes,
                      ht = self$height,
                      wth = self$width,
                      pad = self$padding){

        props <- unlist(lapply(1:length(gc), function(i) gc[[i]]$proportion))
        widths <- (props/sum(props))*(wth - 2*pad)
        ht <- ht - 2*pad
        raw_grobs <- gList()
        
        for(i in 1:length(gc)){
          gc[[i]]$height <- ht
          gc[[i]]$width <- widths[i]
          raw_grobs <- gList(raw_grobs, gc[[i]]$grob)
        }  
        
        arrangeGrob(
          grobs = raw_grobs,
          layout_matrix = matrix(1:length(raw_grobs), nrow = 1),
          height = unit(ht, 'mm'),
          width = unit(widths, 'mm'))
      }
    ))
  
  grob_row_class$new(grob_classes = list(...), proportion = prop)
  
}
# ----

# Grob Column / Grob Column Class ----
grob_col <- function(..., prop = 1, more_args = list()){
  
  grob_col_class <- R6Class("grob_col",
    public = list(
      contents = list(),
      proportion = 1,
      height = 0,
      width = 0,
      padding = 0,
      more_args = list(),
      initialize = function(contents, more_args, proportion){
        stopifnot(is.list(contents), is.list(more_args), is.numeric(proportion))
        self$contents <- contents
        self$proportion <- proportion
        self$more_args <- more_args
      }
    ),
    active = list(
      grob = function(contents = self$contents,
                      m_a = self$more_args,
                      ht = self$height,
                      wth = self$width,
                      pad = self$padding){
          
          wth <- wth - 2*pad
          heights <- rep((ht - 2*pad)/length(contents), length(contents))
          raw_grobs <- gList()
          
          for(i in 1:length(contents)){
            if(is.R6(contents[[i]])){
              height_props <- sapply(1:length(contents), function(i) contents[[i]]$proportion)
              contents[[i]]$height <- (ht - 2*pad)*(height_props/sum(height_props))[i]
              contents[[i]]$width <- wth
              raw_grobs <- gList(raw_grobs, contents[[i]]$grob)
            } else {
              raw_grobs <- gList(
                raw_grobs,
                convert_to_grob(x = contents[[i]], height = heights[i], width = wth, more_args = m_a))
            }
          }
          
          arrangeGrob(
            grobs = raw_grobs,
            layout_matrix = matrix(1:length(raw_grobs), ncol = 1),
            height = unit(heights, 'mm'),
            width = unit(wth, 'mm'))
      }
    ))

  grob_col_contents <- list(...)
  grob_col_class$new(contents = grob_col_contents, more_args = more_args, proportion = prop)
}
# ----

# Convert to Grob function ----
convert_to_grob <- function(x, height, width, more_args = list()){

  # Initilization of variables needed for matrix grobbing ----  
  convert_to_matrix <- function(x){
    if(length(x) > 0 & is.null(nrow(x))) x <- t(as.matrix(x))
    if(is.data.frame(x)) x <- as.matrix(x)
    x
  }
  
  cex_vals <- seq(0.05, 50, 0.05)
  logical_slots <- c('group_elements', 'rownames_present', 'colnames_present')
  numeric_slots <- c('cell_sep', 'row_heights', 'col_widths', 'rowname_colname_fnt_face', 'rowname_colname_border_width')
  character_slots <- c('rowname_colname_bg_color', 'rowname_colname_txt_color', 'rowname_colname_border_color', 'rowname_borders', 'colname_borders')
  non_matrix_slots <- c(logical_slots, numeric_slots, character_slots)

  grob_matrix_slots <- c(
    fnt_face = "matrix",
    bg_color = "matrix",
    bg_alpha = "matrix",
    borders = "matrix",
    border_color = "matrix",
    border_width = "matrix",
    txt_color = "matrix",
    txt_align = "matrix",
    txt_v_align = "matrix",
    txt_just = "matrix",
    txt_v_just = "matrix",
    txt_cex = "matrix",
    txt_font = "matrix",
    txt_angle = "matrix",
    group_elements = "logical",
    rownames_present = "logical",
    colnames_present = "logical",
    # tot_height = "numeric",
    # tot_width = "numeric",
    row_heights = "numeric",
    col_widths = "numeric",
    cell_sep = "numeric",
    rowname_colname_fnt_face = "numeric",
    rowname_colname_border_width = "numeric",
    rowname_colname_bg_color = "character",
    rowname_colname_txt_color = "character",
    rowname_colname_border_color = "character",
    rowname_borders = "character",
    colname_borders = "character")
  # ----
  
  if(is.data.frame(x) | is.matrix(x)){
    # converting to matrix grob if x is a dataframe or matrix 
    for(arg_name in names(more_args)[!names(more_args) %in% non_matrix_slots]){
      more_args[[arg_name]] <- convert_to_matrix(more_args[[arg_name]])
    }
    
    if(!is.null(rownames(x)) & !is.null(colnames(x))){
      x <- rbind(
        c('', colnames(x)),
        cbind(rownames(x), x))
      more_args[['colnames_present']] <- T
      more_args[['rownames_present']] <- T
    } else if(!is.null(rownames(x))){
      x <- cbind(rownames(x), x)
      more_args[['rownames_present']] <- T
    } else if(!is.null(colnames(x))){
      x <- rbind(colnames(x), x)
      more_args[['colnames_present']] <- T
    }
    
    setClass(
      "grob_matrix",
      slots = grob_matrix_slots,
      prototype = more_args)
    
    gm_obj <- new('grob_matrix')
    g <- grob_matrix(x, gm_obj, tot_height = height, tot_width = width)

  }
  else if(ifelse(is.character(x), grepl('.png', x), F)){
    # converting to image grob if x is a string with '.png' in it
    if(!file.exists(x)) stop("PNG file does not exist.")
    
    setClass(
      "grob_image",
      slots = c(
        hjust = "numeric",
        vjust = "numeric",
        maintain_aspect_ratio = "logical"
      ),
      prototype = more_args
    )
    
    gi_obj <- new('grob_image')
    g <- grob_image(x, gi_obj, tot_height = height, tot_width = width)
    
  }
  else if(ifelse(is.character(x), !grepl('.png', x), F)){
    
    one_line_tf <- ifelse('one_line' %in% names(more_args), more_args$one_line, F)
    if(!'txt_cex' %in% names(more_args)){
      for(cv in cex_vals){
        validity <- line_creator(cv, x, height, width, one_line = one_line_tf)$valid
        if(validity) optimal_cv <- cv else break
      }
      lines <- line_creator(optimal_cv, x, height, width, one_line = one_line_tf)
      more_args$txt_cex <- optimal_cv
    } else {
      lines <- line_creator(more_args$txt_cex, x, height, width, one_line = one_line_tf)
    }
    
    txt_matrix <- matrix(lines$lines, ncol = 1)
    
    for(arg_name in names(more_args)[!names(more_args) %in% non_matrix_slots]){
      more_args[[arg_name]] <- convert_to_matrix(more_args[[arg_name]])
    }
    
    setClass(
      "grob_matrix",
      slots = grob_matrix_slots,
      prototype = more_args)
    
    gm_obj <- new('grob_matrix')
    g <- grob_matrix(txt_matrix, gm_obj, tot_height = height, tot_width = width)
    
  }
  else if(is.ggplot(x)){
    
    g <- ggplotGrob(x)
    
  } 
  else if(is.na(x)){
    
    g <- nullGrob()
    
  }
  else if(is.grob(x)){
    
    g <- x
    
  }
  
  return(g)
  
}
# ----

