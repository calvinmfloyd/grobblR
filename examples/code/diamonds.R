
library(grobblR)
library(ggplot2)
library(dplyr)
library(glue)
data(diamonds)

lm.m = lm(price ~ carat, data = diamonds)
lm.summary = summary(x)
r_squared = round(lm.summary$r.squared, 2)
lm.coefficients = lm.summary$coefficients %>% 
  data.frame() %>%
  tibble::rownames_to_column()

carat_coeff = lm.coefficients %>% 
  filter(rowname == "carat") %>%
  pull(Estimate) %>%
  round(2)
  
intercept = lm.coefficients %>% 
  filter(rowname == "(Intercept)") %>%
  pull(Estimate) %>%
  round(2)

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + 
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  scale_y_continuous(limits = c(0, 20000)) +
  labs(x = "Carat", y = "Price") + 
  annotate(
    geom = "text",
    x = 4,
    y = 5000,
    size = 5,
    label = glue("
      price = carat*{carat_coeff} + {intercept}
      R^2 = {r_squared}
      ")) + 
  theme_minimal()

value_df = diamonds %>%
  mutate(
    estimate = carat_coeff*carat + intercept,
    difference = price - estimate
    )



