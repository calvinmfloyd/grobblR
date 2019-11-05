

get_layout_matrix = function(df, group_elements_df) {
  
  nr = nrow(df)
  nc = ncol(df)
  num_indices = nr*nc
  df_vec = c(as.matrix(df))
  group_elements_vec = c(group_elements_df)
  mat = as.matrix(df)
  mat_padded = rbind(NA, cbind(NA, mat, NA), NA)

  col_ind = 2:(nc + 1)
  row_ind = 2:(nr + 1)
  
  above_mat = cbind(above = c(mat_padded[row_ind - 1, col_ind]) == df_vec)
  above_mat[is.na(above_mat)] = FALSE
  
  left_mat = cbind(left = c(mat_padded[row_ind, col_ind - 1]) == df_vec)
  left_mat[is.na(left_mat)] = FALSE
  
  layout_vec = rep(0, num_indices)
  
  for (index in 1:num_indices) {
    
    if (index == 1) {
      
      layout_vec[index] = 1
      
    } else if (!group_elements_vec[index] | (!left_mat[index] & !above_mat[index])){
      
      layout_vec[index] = max(layout_vec[1:index]) + 1
      
    } else if (left_mat[index]) {

      # Checking to make sure the element to the left wants to be grouped together
      layout_vec[index] = ifelse(
        group_elements_vec[index - nr],
        layout_vec[index - nr],
        max(layout_vec[1:index]) + 1
        )
      
    } else if (above_mat[index]) {
      
      # Checking to make sure the element above wants to be grouped together,
      # and that it's not already being grouped together with its left element
      left_of_above_element_index = index - 1 - nr
      above_element_index = index - 1
      left_of_above_element_being_grouped = ifelse(
        left_of_above_element_index < 1, 
        FALSE,
        group_elements_vec[left_of_above_element_index]
        )
      above_element_right_of_like_element = left_mat[index - 1]
      
      layout_vec[index] = ifelse(
        group_elements_vec[index - 1] & !(left_of_above_element_being_grouped & above_element_right_of_like_element),
        layout_vec[index - 1],
        max(layout_vec[1:index]) + 1
        )
      
    }

  }

  layout_matrix = matrix(layout_vec, nrow = nr)
  return(layout_matrix)
  
}
