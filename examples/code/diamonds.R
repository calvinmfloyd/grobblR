
library(grobblR)
library(ggplot2)
library(gghighlight)
library(dplyr)
library(glue)
data(diamonds)

top_n = 5
diamonds = diamonds %>% mutate(id = row_number())

# - Create the content for the report (data frames and ggplots)
lm.m = lm(price ~ carat, data = diamonds)
lm.summary = summary(lm.m)
r_squared = round(lm.summary$r.squared, 2)
lm.coefficients = lm.summary$coefficients %>% 
  data.frame() %>%
  tibble::rownames_to_column()

carat_coeff = lm.coefficients %>% 
  filter(rowname == "carat") %>%
  pull(Estimate) %>%
  round(1)
  
intercept = lm.coefficients %>% 
  filter(rowname == "(Intercept)") %>%
  pull(Estimate) %>%
  round(1)

lm.gg = ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + 
  geom_point(alpha = 0.25) +
  geom_abline(slope = carat_coeff, intercept = intercept, color = "blue") +
  scale_y_continuous(limits = c(0, 20000)) +
  labs(x = "Carat", y = "Price") + 
  annotate(
    geom = "text",
    x = 4,
    y = 5000,
    size = 5,
    label = glue("
      Price = Carat*{carat_coeff} + {intercept}
      R^2 = {r_squared}
      ")) + 
  theme_minimal() + 
  labs(title = "Price vs. Carat")

value_df = diamonds %>%
  mutate(
    carat_label = sprintf("%.1f", carat),
    price_label = glue("${round(price)}"),
    estimate = pmax(carat_coeff*carat + intercept, 0.0),
    estimate_label = glue("${round(estimate)}"),
    difference = estimate - price,
    difference_label = glue("{ifelse(difference >= 0, '+', '')}{round(difference)}")
    )

overvalued_df = value_df %>%
  arrange(difference) %>%
  head(top_n)

overvalued_grob_matrix = overvalued_df %>%
  select(
    ID = id,
    Carat = carat,
    Estimate = estimate_label,
    Price = price_label,
    `Diff.` = difference_label
    ) %>%
  grob_matrix() %>%
  alter_at(
    ~ "red",
    columns = "Diff.",
    aesthetic = "text_color",
    group = "cells"
    )

overvalued_gg = ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + 
  geom_point(color = "red") +
  gghighlight(id %in% overvalued_df[["id"]]) +
  geom_abline(slope = carat_coeff, intercept = intercept, color = "blue") +
  scale_y_continuous(limits = c(0, 20000)) +
  labs(x = NULL, y = NULL) + 
  theme_minimal() +
  theme(axis.text = element_blank())

undervalued_df = value_df %>%
  arrange(desc(difference)) %>%
  head(top_n)

undervalued_grob_matrix = undervalued_df %>%
  select(
    ID = id,
    Carat = carat,
    Estimate = estimate_label,
    Price = price_label,
    `Diff.` = difference_label
    ) %>%
  grob_matrix() %>%
  alter_at(
    ~ "green4",
    columns = "Diff.",
    aesthetic = "text_color",
    group = "cells"
    )

undervalued_gg = ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + 
  geom_point(color = "green4") +
  gghighlight(id %in% undervalued_df[["id"]]) +
  geom_abline(slope = carat_coeff, intercept = intercept, color = "blue") +
  scale_y_continuous(limits = c(0, 20000)) +
  labs(x = NULL, y = NULL) + 
  theme_minimal() +
  theme(axis.text = element_blank())

# - Create a unique title grob-row for the top of the report
title_grob_row = grob_row(
  height = 25,
  # - Diamond image from the internet
  grob_col(
    p = 0.25,
    "https://assets.stickpng.com/images/580b585b2edbce24c47b23f2.png",
    hjust = 3
    ),
  grob_col(
    grob_row(
      grob_col(
        "DIAMONDS DATASET",
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
        "Estimating Price by Carat",
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

  # - Creating the overall grob-layout of the report
  gl = grob_layout(
    title_grob_row,
    grob_row(
      height = 100,
      grob_col(lm.gg)
      ),
    grob_row(
      height = 15,
      grob_col(
        "Which diamonds in the dataset provide the worst / best value based on their carat?",
        aes_list = ga_list(
          font_face = "italic",
          text_color = "gray50",
          n_lines = 1
          )
        )
      ),
    grob_row(
      grob_col(
        title = glue("WORST VALUE"),
        title_p = 0.1,
        border = TRUE,
        border_aes_list = ga_list(border_sides = "right"),
        grob_row(grob_col(overvalued_gg)),
        grob_row(grob_col(overvalued_grob_matrix))
        ),
      grob_col(
        title = glue("BEST VALUE"),
        title_p = 0.1,
        grob_row(grob_col(undervalued_gg)),
        grob_row(grob_col(undervalued_grob_matrix))
        )
      )
    ) 

# - Saving the final grob-layout to a PDF
gl %>%
  grob_to_pdf(
    file_name = "diamonds.pdf",
    meta_data_title = "Diamonds Dataset"
    )


