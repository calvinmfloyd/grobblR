
convert_to_matrix <- function(x){
  if(length(x) > 0 & is.null(nrow(x))) x <- t(as.matrix(x))
  if(is.data.frame(x)) x <- as.matrix(x)
  x
}
