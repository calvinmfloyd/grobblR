
source('convert_to_grob.R')
source('grob_col.R')
source('grob_row.R')
source('grob_layout.R')
source('grob_matrix.R')
source('grob_image.R')
source('line_creator.R')

library(UsingR)
library(dplyr)
library(ggplot2)
library(graphics)
library(grid)
library(gridExtra)
library(png)
library(R6)

# Making the package ----
install.packages('roxygen2')
library("devtools")
library(roxygen2)

library(devtools)
install_github('grobblR', 'calvinmfloyd')
library(grobblR)

create('grobblR')
document()

# ----

pars <- unlist(strsplit(lorem, "\n\n"))
first_paragraph <- pars[1]
data(iris)
summary_df <- iris %>%
  group_by(Species) %>%
  summarise(
    mean_petal_length = mean(Petal.Length),
    mean_petal_width = mean(Petal.Width)) %>%
  as.matrix()

gg <- ggplot(iris) + geom_point(aes(x = Sepal.Length, y = Sepal.Width, color = Species))

g <- grob_layout(
  grob_row(height_prop = 1,
    border = T,
    border_args = gpar(lwd = 10, col = 'red', alpha = 0.5),
    grob_col(
      'Iris Dataset Grob',
      width_prop = 1,
      more_args = list(
        one_line = T,
        txt_color = 'gray40',
        border_color = 'gray40',
        border_width = 10))),
  grob_row(height_prop = 2,
    grob_col(width_prop = 3, head(as.matrix(iris), 20)),
    grob_col(width_prop = 3,
       grob_row(height_prop = 1,
         grob_col(width_prop = 1, gg)),
       grob_row(height_prop = 1,
         grob_col(width_prop = 1, head(as.matrix(iris), 5), border = T)))),
  grob_row(height_prop = 2,
    grob_col(width_prop = 1, 'iris.png', more_args = list(maintain_aspect_ratio = T))),
  grob_row(height_prop = 1,
    grob_col(width_prop = 1, first_paragraph, more_args = list(txt_just = 0, txt_align = 0), border = T))
)

ggsave('g.pdf', g$grob, height = as.numeric(g$total_height), width = as.numeric(g$total_width), units = 'mm')

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

