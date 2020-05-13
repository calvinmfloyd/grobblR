
devtools::load_all()
image_structures = get_all_image_structures()
matrix_aesthetics = get_matrix_aesthetics()
matrix_structures = get_all_matrix_structures()
usethis::use_data(matrix_structures, matrix_aesthetics, image_structures, overwrite = TRUE, internal = TRUE)


