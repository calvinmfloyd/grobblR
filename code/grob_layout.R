
# Grob Layout ----
grob_layout <- function(...,
                        page_height = 280,
                        page_width = 216,
                        page_padding = 5,
                        grob_padding = 2,
                        row_heights = c(),
                        column_widths = c()){
  
  # Initializing Variables ----
  ph <- page_height - 2*page_padding
  pw <- page_width - 2*page_padding
  g_info <- list(...)
  # ----
  
  # Creating the Layout Matrix, using LCM ----
  nr <- length(g_info)
  layout_matrix <- matrix(1:nr, ncol = 1)
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
    'total_height' = unit(page_height, 'mm'),
    'total_width' = unit(page_width, 'mm'),
    'page_padding' = unit(page_padding, 'mm'),
    'grob_padding' = unit(grob_padding, 'mm'))
}
# ----

# Grob Row / Grob Row Class ----
grob_row <- function(..., prop = 1, border = F, border_args = gpar()){
  
  grob_row_class <- R6Class("grob_row",
    public = list(
      height = 0,
      width = 0,
      padding = 0,
      proportion = 1,
      grob_classes = list(),
      border = F,
      border_args = gpar(),
      initialize = function(grob_classes, proportion, border, border_args){
        stopifnot(is.list(grob_classes), is.numeric(proportion), is.logical(border), is.list(border_args))
        self$grob_classes <- grob_classes
        self$proportion <- proportion
        self$border <- border
        self$border_args <- border_args
        }),
    active = list(
      grob = function(gc = self$grob_classes,
                      ht = self$height,
                      wth = self$width,
                      pad = self$padding,
                      bor = self$border,
                      bor_args = self$border_args){

        props <- unlist(lapply(1:length(gc), function(i) gc[[i]]$proportion))
        widths <- (props/sum(props))*(wth - 2*pad)
        height <- ht - 2*pad
        raw_grobs <- gList()
        
        for(i in 1:length(gc)){
          gc[[i]]$height <- height
          gc[[i]]$width <- widths[i]
          gc[[i]]$padding <- pad
          raw_grobs <- gList(raw_grobs, gc[[i]]$grob)
        }  
        
        g <- arrangeGrob(
          grobs = raw_grobs,
          layout_matrix = matrix(1:length(raw_grobs), nrow = 1),
          height = unit(height, 'mm'),
          width = unit(widths, 'mm'))
        
        if(bor){
          # bor_args <- gpar(bor_args, gpar(fill = 'transparent'))
          g <- grobTree(g, rectGrob(height = unit(ht, 'mm'), width = unit(wth, 'mm'), gp = bor_args))
        }
        g
      }
    ))
  
  grob_row_class$new(
    grob_classes = list(...),
    proportion = prop,
    border = border,
    border_args = border_args)
}
# ----

# Grob Column / Grob Column Class ----
grob_col <- function(..., prop = 1, more_args = list(), border = F, border_args = gpar()){
  
  grob_col_class <- R6Class("grob_col",
    public = list(
      contents = list(),
      proportion = 1,
      height = 0,
      width = 0,
      padding = 0,
      more_args = list(),
      border = F,
      border_args = gpar(),
      initialize = function(contents, more_args, proportion, border, border_args){
        stopifnot(is.list(contents), is.list(more_args), is.numeric(proportion))
        self$contents <- contents
        self$proportion <- proportion
        self$more_args <- more_args
        self$border <- border
        self$border_args <- border_args
      }
    ),
    active = list(
      grob = function(contents = self$contents,
                      m_a = self$more_args,
                      ht = self$height,
                      wth = self$width,
                      pad = self$padding,
                      bor = self$border,
                      bor_args = self$bor_args){
          
          wth_w_padding <- wth - 2*pad
          hts_w_padding <- rep((ht - 2*pad)/length(contents), length(contents))
          hts <- rep(ht/length(contents), length(contents))
          raw_grobs <- gList()
          
          for(i in 1:length(contents)){
            if(is.R6(contents[[i]])){
              height_props <- sapply(1:length(contents), function(i) contents[[i]]$proportion)
              contents[[i]]$height <- ht*(height_props/sum(height_props))[i]
              contents[[i]]$width <- wth
              contents[[i]]$padding <- pad
              raw_grobs <- gList(raw_grobs, contents[[i]]$grob)
            } else {
              g <- convert_to_grob(x = contents[[i]], height = hts_w_padding[i], width = wth_w_padding, more_args = m_a)
              if(bor){
                bor_args <- gpar(bor_args, gpar(fill = 'transparent'))
                g <- grobTree(g, rectGrob(height = unit(hts[i], 'mm'), width = unit(wth, 'mm'), gp = bor_args))
              }
              raw_grobs <- gList(raw_grobs, g)
            }
          }
          
          arrangeGrob(
            grobs = raw_grobs,
            layout_matrix = matrix(1:length(raw_grobs), ncol = 1),
            height = unit(hts, 'mm'),
            width = unit(wth, 'mm'))
      }
    ))

  grob_col_contents <- list(...)
  grob_col_class$new(
    contents = grob_col_contents,
    more_args = more_args,
    proportion = prop,
    border = border,
    border_args = border_args)
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
      x <- rbind(c('', colnames(x)), cbind(rownames(x), x))
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
    stopifnot(file.exists(x))
    
    setClass(
      "grob_image",
      slots = c(
        hjust = "numeric",
        vjust = "numeric",
        maintain_aspect_ratio = "logical"),
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
    
    png_name <- sprintf("ggplot_grob_%s_%s.png", format(Sys.time(), '%m_%d_%Y'), format(Sys.time(), "%H_%M_%S"))
    ggsave(png_name, x, height = height, width = width, unit = 'mm')
    # g <- ggplotGrob(x)
    
    setClass(
      "grob_image",
      slots = c(
        hjust = "numeric",
        vjust = "numeric",
        maintain_aspect_ratio = "logical"),
      prototype = more_args
    )
    
    gi_obj <- new('grob_image')
    g <- grob_image(png_name, gi_obj, tot_height = height, tot_width = width)
    file.remove(png_name)
    
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

