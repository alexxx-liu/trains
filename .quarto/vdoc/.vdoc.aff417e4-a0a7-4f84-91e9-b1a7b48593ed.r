#
#
#
#
#
#
#
#
#
#
#
#
#| label: setup
#| include: false
#| message: false
library(tidyverse)
library(primer.data)
library(broom)
library(knitr)
library(marginaleffects)
#
#
#
#| echo: true
library(primer.data)
library(tidyverse)

data("trains", package = "primer.data")

ggplot(trains, aes(x = treatment, y = att_end)) +
  geom_jitter(width = 0.1, alpha = 0.7, color = "steelblue") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") +
  labs(
    title = "Individual att_end values by treatment group",
    subtitle = "Each point is one person; the red point shows the group mean",
    x = "Treatment",
    y = "att_end",
    caption = "Data from the trains tibble"
  ) +
  theme_minimal()
#
#
#
glimpse(trains)
#
#
#
library(parsnip)

model_spec <- linear_reg(engine = "lm") |>
  fit(att_end ~ treatment, data = trains)
model_spec
#
#
#
linear_reg(engine = "lm") |>
  fit(att_end ~ treatment + age, data = trains)
#
#
#
#| cache: true
fit_att <- linear_reg(engine = "lm") |>
  fit(att_end ~ treatment + age, data = trains)
#
#
#
#| echo: true
predictions(fit_att)
#
#
#
tidy(fit_att, conf.int = TRUE)
#
#
#
model_table <- tidy(fit_att, conf.int = TRUE) |>
  select(term, estimate, conf.low, conf.high) |>
  mutate(
    estimate = signif(estimate, 3),
    conf.low = signif(conf.low, 3),
    conf.high = signif(conf.high, 3)
  )

kable(
  model_table,
  caption = "Model estimates from the trains data source (primer.data::trains)",
  col.names = c("Term", "Estimate", "95% CI Lower", "95% CI Upper")
)
#
#
#
#
#
