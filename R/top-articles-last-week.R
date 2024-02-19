# libraries

library(pageviews)
library(wikkitidy)
library(tidyverse)
library(lubridate)
library(rvest)
library(ggtext)

source("R/funcs.R")

# setting params

options(lubridate.week.start = 1)
date_from <- floor_date(today() - 7, unit = "week")
date_to <- date_from + 6
lang <- "cs"

# top articles summary

top_articles <- top_articles_by_day(date_from, date_to, lang) |>
  summarise_top_articles(10)

# fetch top article info

page_summary <- wikkitidy::get_page_summary(
  top_articles$article[1] |> as.character(), language = "cs"
)
page_h1 <- page_summary$title
page_description <- page_summary$extract

top10_pv <- sum(top_articles$views)
total_pv <- total_pageviews(date_from, date_to, "cs")
summary_line <- str_glue(
  "Česká Wikipedie měla v týdnu od {format_date(date_from)} ",
  "celkem {format(total_pv, big.mark = ' ')} zhlédnutí. ",
  "10 nejčtenějších stránek tedy představuje ",
  "{percents} %.",
  percents = format(
    round(top10_pv / total_pv * 100, digits = 2),
    decimal.mark = ","
  )
)

# plot top articles

p <- top_articles |>
  ggplot(aes(x = views, y = article)) +
  coord_fixed(ratio = 9 / 16) +
  geom_col(fill = hsv(0.3, 0.6, 0.4), alpha = 0.8, width = 0.27) +
  geom_text(
    aes(x = 0, y = article, label = str_replace_all(article, fixed("_"), " ")),
    hjust = 0, position = position_nudge(y = 0.44), size = 7, color = "gray20"
  ) +
  scale_x_continuous(
    expand = c(0, 0), labels = scales::label_number(scale = 0.001, suffix = "K")
  ) +
  labs(
    title = paste(
      "Nejčtenější články české Wikipedie v týdnu od", format_date(date_from)
    ),
    x = "počet zhlédnutí",
    y = NULL,
    caption = summary_line
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(hjust = 0, size = 16),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(margin = margin(0, 0, 15, 0), size = 40),
    aspect.ratio = 9 / 16
  )

# plot text box

box_width <- 0.98 - top_articles |>
  mutate(
    height = rank(article),
    width = max(views) - views,
    area = height * width,
    prop = views / max(views)
  ) |>
  slice_max(area) |>
  pull(prop)

p <- p +
  geom_textbox(
    aes(x = max(views), y = article[10], label = paste0(
      "**", page_h1, "**\n\n", page_description
    )),
    vjust = 0, hjust = 1,
    width = unit(box_width, "npc"), stat = "unique",
    alpha = 0.7,
    size = 6, color = "black"
  )

p <- p +
  theme(
    plot.title = element_text(size = 22, face = "bold", color = "gray20"),
    plot.caption = element_text(size = 14),
    axis.text = element_text(size = 14, color = "gray20")
  )

ggsave(
  "top-articles-last-week.png", p,
  width = 1200, height = 675, units = "px", dpi = "screen",
  bg = "white"
)
