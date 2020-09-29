
library(grobblR)
library(ggplot2)
library(dplyr)
data(iris)

# - Create the content for the report (data frames and ggplots)
sepal_gg = ggplot(
  data = iris,
  mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)
  ) +
  geom_point() +
  theme_minimal()
  
sepal_df = iris %>%
  group_by(Species) %>%
  summarise(
    `AVG Length` = mean(Sepal.Length),
    `AVG Width` = mean(Sepal.Width),
    .groups = "drop"
    )

petal_gg = ggplot(
  data = iris,
  mapping = aes(x = Petal.Length, y = Petal.Width, color = Species)
  ) +
  geom_point() +
  theme_minimal()

petal_df = iris %>%
  group_by(Species) %>%
  summarise(
    `AVG Length` = mean(Petal.Length),
    `AVG Width` = mean(Petal.Width),
    .groups = "drop"
    )

# - Create a unique title grob-row for the top of the report
title_grob_row = grob_row(
  height = 25,
  grob_col(
    grob_row(
      grob_col(
        "IRIS DATASET",
        aes_list = ga_list(
          font_face = "bold",
          n_lines = 1,
          text_align = "left",
          text_cex = 1.6,
          text_color = "gray40"
          )
        )
      ),
    grob_row(
      grob_col(
        "Comparing Length & Width By Species",
        aes_list = ga_list(
          n_lines = 1,
          text_align = "left",
          text_cex = 1.2,
          text_color = "gray40",
          text_v_align = "top"
          )
        )
      )
    )
  )

  # - Create an aes_list for each of our grob-row titles
  title_ga_list = ga_list(text_color = "purple", text_cex = 1.2)

  # - Creating the overall grob-layout of the report
  gl = grob_layout(
    title_grob_row,
    grob_row(
      height = 50,
      title = "DESCRIPTION",
      title_aes_list = title_ga_list,
      grob_col(
        p = 0.5,
        border = TRUE,
        "https://assets.teleflora.com/images/customhtml/meaning-of-flowers/iris.png"
        ),
      grob_col(
        "Iris is a genus of species of flowering plants with showy flowers. It takes its name from the Greek word for a rainbow, which is also the name for the Greek goddess of the rainbow, Iris. Some authors state that the name refers to the wide variety of flower colors found among the many species. As well as being the scientific name, iris is also widely used as a common name for all Iris species, as well as some belonging to other closely related genera. A common name for some species is 'flags', while the plants of the subgenus Scorpiris are widely known as 'junos', particularly in horticulture. It is a popular garden flower.",
        aes_list = ga_list(font_face = "italic", text_align = "left")
        )
      ),
    grob_row(
      title = "SEPAL INFO",
      title_aes_list = title_ga_list,
      grob_col(sepal_df),
      grob_col(sepal_gg)
      ),
    grob_row(
      title = "PETAL INFO",
      title_aes_list = title_ga_list,
      grob_col(petal_df),
      grob_col(petal_gg)
      )
    ) 
  
  # - Saving the final grob-layout to a PDF
  gl %>%
    grob_to_pdf(
      file_name = "~/grobblR/examples/reports/iris.pdf",
      meta_data_title = "Iris Dataset"
      )

