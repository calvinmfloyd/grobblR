
source('convert_to_grob.R')
source('grob_col.R')
source('grob_row.R')
source('grob_layout.R')
source('grob_matrix.R')
source('grob_image.R')
source('line_creator.R')
source('grob_to_pdf.R')

# library(UsingR)
# library(dplyr)
# library(ggplot2)
# library(graphics)
# library(grid)
# library(gridExtra)
# library(png)
# library(R6)

# Making the package ----
install.packages('roxygen2')
library("devtools")
library(roxygen2)

devtools::use_vignette('grobblR_vignette')

# create('grobblR')
devtools::document()

# ----

# library(graphics)
# library(grid)
# library(gridExtra)
# library(png)
# library(R6)

devtools::install_github('calvinmfloyd/grobblR')
library(grobblR)

library(UsingR)
library(dplyr)
library(ggplot2)

pars <- unlist(strsplit(lorem, "\n\n"))
first_paragraph <- pars[1]
data(iris)

summary_df <- iris %>%
  group_by(Species) %>%
  summarise(
    m = mean(Petal.Length) %>% round(1),
    n = mean(Petal.Width) %>% round(1))

gg <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point()

aes_list <- list(txt_just = matrix(c('right', 'right', 'right', 'left'), nrow = 2))
aes_list <- list(txt_just = matrix(c(0.5, 0.5, 0.5, 0.5), nrow = 2))
l <- list(txt_just = data.frame(matrix(c('right', 'right', 'right', 'left'), nrow = 2)))

pw <- 100

if(2 == 3)
  print(1)


line_creator(2, 'dgfdgdfefg \n sadf sdfdas \n sd yh wk nw \n hello', height = 50, width = 50)

df <- data.frame(x = 0.2, y = 0.5)

gl <- grob_layout(
  grob_row(
    border = T
    ,grob_col(
      # '- Hello My name is Calvin\n- I like long walks on the beach\n- Kobes overrated',
      grob_row(grob_col(df))
      ,grob_row(p = 4, grob_col(NA))
      ,title = 'Title 1'
      # ,aes_list = list(txt_color = 'gray40', txt_just = 'left', bg_color = 'red', round_rect_radius = 0.2)
      )
    ,grob_col(p = 2,
      grob_row(p = 2, grob_col(matrix(c('1', '2', '3', '4'), nrow = 2), aes_list = list(bg_color = 'red'))),
      grob_row(grob_col(matrix(c('1', '2', '3', '4'), nrow = 1), aes_list = list(bg_color = 'blue'))),
      title = 'Title 2',
      title_aes_list = list(fnt_face = 3, bg_color = 'white', txt_color = 'navy'),
      border = T,
      aes_list = list(txt_color = 'navy'))
    # ,grob_col(NA, p = 5)
    # ,grob_col('2', border = T, aes_list = list(bg_color = 'gray40'), hjust = 1, vjust = 1)
    # ,grob_col('3', border = T, aes_list = list(bg_color = 'navy'), hjust = 0, vjust = 0)
  ),
  title = 'grob_layout Title',
  width = 100,
  height = 100)


gl <- grob_layout(
  grob_row(grob_col('1'), grob_col('2')),
  grob_row(grob_col('3'), grob_col('4')),
  height = 100,
  width = 100,
  page_number = 4)

gridExtra::grid.arrange(gl$grob)

g <- grob_layout(
  grob_row(
    border = T
    ,grob_col(
      'txt_just, no txt_align'
      ,border = T
      ,aes_list = list(
        bg_color = 'gray90'
        ,txt_color = 'navy'
        ,txt_just = 1))
    ,grob_col(
      'txt_align, no txt_just'
      ,border = T
      ,aes_list = list(
        bg_color = 'gray90'
        ,txt_color = 'navy'
        ,txt_align = 1))
    ,grob_col(
      'txt_align & txt_just'
      ,border = T
      ,aes_list = list(
        bg_color = 'gray90'
        ,txt_color = 'navy'
        ,txt_align = 1
        ,txt_just = 1))
  ),
  width = pw,
  height = 30)

grid.arrange(g)


grob_to_pdf(g, file_name = 'test')







g <- grob_layout(
  grob_row(border = T,
    grob_col('Iris Dataset Grob', aes_list = list(txt_color = 'gray40'))),
  grob_row(p = 2, border = T, title = 'Section 1',
    grob_col(p = 3,
      head(iris, 20),
      aes_list = list(color_gradient_cols = 3)),
    grob_col(p = 3.5,
      grob_row(
        grob_col(p = 2, gg)
        ,grob_col('NBA is the best', aes_list = list(bg_color = 'gray40', txt_color = 'white'))),
       grob_row(
         grob_col(
            summary_df,
            aes_list = list(
              txt_color = matrix(
                rep(c('red', 'blue'), length = nrow(summary_df)*ncol(summary_df))
                ,nrow = nrow(summary_df))
              ,colname_txt_color = 'black'
              ,colname_bg_color = 'gray80'))))),
  grob_row(
    grob_col('../iris.png', aes_list = list(maintain_aspect_ratio = T))
    ,grob_col('grobblR package', aes_list = list(txt_color = 'navy'))),
  grob_row(border = T, border_aes_list = list(col = 'red', lwd = 5),
    grob_col(first_paragraph, aes_list = list(txt_just = 0, txt_align = 0)))
)

gridExtra::grid.arrange(g)
grob_to_pdf(g, file_name = 'g.pdf')








  grobb <- grob_layout(
    grob_row(grob_col("Hello World")),
    grob_row(grob_col("Go Kings"))
    ,height = 50
    ,width = 50
  )


grob_row(
  grob_col(prop = 1, summary_df, more_args = list(txt_color = 'red')),
  grob_col(prop = 2, 'iris.png', more_args = list(maintain_aspect_ratio = F)),
  grob_col(prop = 1,
    grob_row(
      grob_col(prop = 1, head(as.matrix(iris), 20))),
    grob_row(
      grob_col(prop = 1, head(as.matrix(iris), 10)))
  )
) -> g

pic_grob <- convert_to_grob('iris.png', 100, 100)
rect_grob <- rectGrob(height = unit(100, 'mm'), width = unit(100, 'mm'), gp = x)
grob <- grobTree(rect_grob, pic_grob)
grid.arrange(grob)

ggsave('g.png', gg, height = 100, width = 100, unit = 'mm')
g <- ggplotGrob(gg)

g1 <- arrangeGrob(grobs = gList(g), ncol = 1, nrow = 1, heights = unit(100, 'mm'), widths = unit(100, 'mm'))
ggsave('g1.pdf', g1)

