
grob_matrix <- function(df, gm_obj, tot_height = numeric(), tot_width = numeric()){

  in_to_mm <- function(x) x*25.4
  
  stopifnot(!is.null(nrow(df)), !is.null(ncol(df)))
  stopifnot(!length(tot_height) == 0, !length(tot_width) == 0)
  
  nr <- nrow(df)
  nc <- ncol(df)
  
  # Adding in default values (non-matrices) if they are missing ----
  def_vals_non_matrices <- list(
    group_elements = FALSE,
    rownames_present = FALSE,
    colnames_present = FALSE,
    cell_sep = 1,
    rowname_colname_bg_color = 'gray40',
    rowname_colname_txt_color = 'white',
    rowname_colname_fnt_face = 1,
    rowname_colname_border_color = 'gray40',
    rowname_colname_border_width = 1,
    rowname_borders = '',
    colname_borders = '')
  
  for(val_name in names(def_vals_non_matrices)){
    if(length(slot(gm_obj, val_name)) == 0){
      slot(gm_obj, val_name) <- def_vals_non_matrices[[val_name]] 
    }
  }
  # ----
  
  # Making adjustments to txt_cex, if need be ----
  adjust_cex <- length(gm_obj@txt_cex) == 0 & 
    (length(tot_height) == 1 & length(gm_obj@row_heights) == 0) &
    (length(tot_width) == 1 & length(gm_obj@col_widths) == 0)

  if(!adjust_cex){
    
    def_txt_cex <- 2
    
  } else {
    
    cex_vals <- seq(0.01, 20, 0.01)
    rh <- tot_height/nr - 2*gm_obj@cell_sep
    cw <- tot_width/nc - 2*gm_obj@cell_sep
    widest_element <- c(df)[which(strwidth(c(df), units = 'in') == max(strwidth(c(df), units = 'in')))[1]]
    tallest_element <- c(df)[which(strheight(c(df), units = 'in') == max(strheight(c(df), units = 'in')))[1]]
    
    poss_str_widths <- sapply(cex_vals, function(c) in_to_mm(strwidth(widest_element, cex = c, units = 'inches')))
    poss_str_heights <- sapply(cex_vals, function(c) in_to_mm(strheight(tallest_element, cex = c, units = 'inches')))
    
    def_txt_cex <- min(c(cex_vals[max(which(poss_str_widths <= cw))], cex_vals[max(which(poss_str_heights <= rh))]))
    
  }
  # ----
  
  # Adding in default values (matrices) if they are missing ----
  def_vals_matrices <- list(
    fnt_face = 1,
    bg_color = 'white',
    bg_alpha = 1,
    borders = '',
    txt_color = 'black',
    txt_align = 0.5,
    txt_v_align = 0.5,
    txt_just = 0.5,
    txt_v_just = 0.5,
    txt_cex = def_txt_cex,
    txt_font = 'sans',
    border_color = 'gray40',
    txt_angle = 0,
    border_width = 1)
  
  for(val_name in names(def_vals_matrices)){
    def_vals_matrices[[val_name]] <- ifelse(
      all(dim(slot(gm_obj, val_name)) == 1),
      slot(gm_obj, val_name)[1,1],
      def_vals_matrices[[val_name]])
    if(all(dim(slot(gm_obj, val_name)) <= 1)){
      slot(gm_obj, val_name) <- matrix(def_vals_matrices[[val_name]], nrow = nr, ncol = nc)
    }
  }
  # ----
  
  # Adding in Row Name / Column Name attributes ----
  rn_cn_shared_vals <- c('bg_color', 'txt_color', 'fnt_face', 'border_color', 'border_width')
  
  if(gm_obj@rownames_present){
    gm_obj@borders[,1] <- gm_obj@rowname_borders
    for(val_name in rn_cn_shared_vals){
      slot(gm_obj, val_name)[,1] <- slot(gm_obj, paste0('rowname_colname_', val_name))
    }
  }
  
  if(gm_obj@colnames_present){
    gm_obj@borders[1,] <- gm_obj@colname_borders
    for(val_name in rn_cn_shared_vals){
      slot(gm_obj, val_name)[1,] <- slot(gm_obj, paste0('rowname_colname_', val_name))
    }
  }  
  
  if(gm_obj@colnames_present & gm_obj@rownames_present){
    gm_obj@borders[1,1] <- ''
    gm_obj@bg_color[1,1] <- NA
  }
  
  # ----
  
  raw_grobs <- gList()
  for(j in 1:nc){
    for(i in 1:nr){
      
      rect_grob <- rectGrob(
        gp = gpar(
          fill = gm_obj@bg_color[i,j],
          col = gm_obj@bg_color[i,j],
          alpha = gm_obj@bg_alpha[i,j]))
      
      text_grob <- textGrob(
        df[i,j],
        x = unit(gm_obj@txt_align[i,j], "npc"),
        y = unit(gm_obj@txt_v_align[i,j], "npc"),
        hjust = gm_obj@txt_just[i,j],
        vjust = gm_obj@txt_v_just[i,j],
        rot = gm_obj@txt_angle[i,j],
        gp = gpar(
          fontface = gm_obj@fnt_face[i,j],
          fontfamily = gm_obj@txt_font[i,j],
          cex = gm_obj@txt_cex[i,j],
          col = gm_obj@txt_color[i,j]))
      
      cell_border_gs <- gList()
      borders_split <- unlist(strsplit(gm_obj@borders[i,j], split = ', ', fixed = TRUE))
      
      if(length(borders_split) > 0){
        for(side in 1:length(borders_split)){
          
          cell_border_gs <- gList(
            cell_border_gs, 
            segmentsGrob(
              x0 = unit(ifelse(borders_split[side] %in% c("right"), 1, 0), "npc"),
              y0 = unit(ifelse(borders_split[side] %in% c("top"), 1, 0), "npc"),
              x1 = unit(ifelse(borders_split[side] %in% c("top", "bottom", "right"), 1, 0), "npc"),
              y1 = unit(ifelse(borders_split[side] %in% c("left", "right", "top"), 1, 0), "npc"),
              gp = gpar(col = gm_obj@border_color[i,j], lwd = gm_obj@border_width[i,j])))
        }
        
        raw_grobs <- gList(raw_grobs, grobTree(rect_grob, cell_border_gs, text_grob))
      
      } else {
        
        raw_grobs <- gList(raw_grobs, grobTree(rect_grob, text_grob))
        
      }
    }}
  
  # ----
  if(gm_obj@group_elements){
    ue <- unique(c(as.matrix(df)))
    level_df <- data.frame(element = ue, level = 1:length(ue), stringsAsFactors = F)
    matched_df <- matrix(level_df$level[match(df, level_df$element)], nrow = nr)
    m.v <- c(matched_df)
    l.v <- rep(0, nr*nc)
    i.v <- 1:(nr*nc)
    for(i in i.v){
      r <- (i-1)%/%nr
      like_indices <- which(m.v == m.v[i])
      left_index <- i.v[i.v == (i-nr)]
      top_bottom_indices <- i.v[(i.v-1)%/%nr == r & i.v %in% c(i-1, i+1)]
      adj_indices <- c(left_index, top_bottom_indices)
          
      if(length(like_indices) == 1 | i == min(like_indices) | all(!like_indices %in% adj_indices)){
        
        l.v[i] <- ifelse(i == 1, 1, max(cumsum(l.v)[1:(i-1)]) - cumsum(l.v)[i-1] + 1)
        
      } else if (i != min(like_indices) & (i+1) %in% like_indices & !(i-nr) %in% like_indices){
        
        l.v[i] <- cumsum(l.v)[min(like_indices)] - cumsum(l.v)[i-1]
        
      } else if ((i-nr) %in% like_indices){
        
        l.v[i] <- cumsum(l.v)[i-nr] - cumsum(l.v)[i-1]
        
      }
    }
    layout_matrix <- matrix(cumsum(l.v), nrow = nr)
    
  } else {
    
    layout_matrix <- matrix(1:(nr*nc), nrow = nr, ncol = nc)
    
  } 
  # ----
  
  if(length(gm_obj@row_heights) == 0 & length(tot_height) == 0){
    
    tmp_row_heights <- c()
    for(i in 1:nr){
      ind_row_heights <- mapply(
        function(txt, cex, face, fam){
          in_to_mm(strheight(txt, units = 'inches', cex = cex, family = fam, font = face))
        },
        df[i,],
        gm_obj@txt_cex[i,],
        gm_obj@fnt_face[i,],
        gm_obj@txt_font[i,]) 
      tmp_row_heights <- c(tmp_row_heights, max(ind_row_heights) + 2*gm_obj@cell_sep)
    }
    gm_obj@row_heights <- rep(max(tmp_row_heights), nr)
    
  } else if(length(tot_height) == 1 & length(gm_obj@row_heights) != nr){
    
    gm_obj@row_heights <- rep(tot_height/nr, nr)
    
  }
  
  if(length(gm_obj@col_widths) == 0 & length(tot_width) == 0){
    
    tmp_col_widths <- c()
    for(i in 1:nc){
      ind_col_widths <- mapply(
        function(txt, cex, face, fam){
          in_to_mm(strwidth(txt, units = 'inches', cex = cex, family = fam, font = face))
        },
        df[,i],
        gm_obj@txt_cex[,i],
        gm_obj@fnt_face[,i],
        gm_obj@txt_font[,i]) 
      tmp_col_widths <- c(tmp_col_widths, max(ind_col_widths) + 2*gm_obj@cell_sep)
    }
    gm_obj@col_widths <- tmp_col_widths
    
  } else if(length(tot_width) == 1 & length(gm_obj@col_widths) != nc){
    
    gm_obj@col_widths <- rep(tot_width/nc, nc)
    
  }
  # ----
  first_element_indices <- unlist(lapply(unique(c(layout_matrix)), function(x) min(which(c(layout_matrix) == x))))
  
  arrangeGrob(
    grobs = raw_grobs[first_element_indices],
    heights = unit(gm_obj@row_heights, 'mm'),
    widths = unit(gm_obj@col_widths, 'mm'),
    layout_matrix = layout_matrix)
  
}