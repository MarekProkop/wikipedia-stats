# libraries

library(pageviews)
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

page_url <- paste0(
  "https://cs.wikipedia.org/wiki/",
  URLencode(str_replace_all(top_articles$article[1], fixed(" "), "_"))
)
page_content <- read_html(page_url)
page_h1 <- page_content |>
  html_element("h1") |>
  html_text()
page_description <- page_content |>
  html_element("p") |>
  html_text() |>
  str_remove_all("\\([^)]+\\)|\\[[^]]+\\]") |>
  str_squish()
if (nchar(page_description) < 30) {
  page_description <- page_content |>
    html_element("p:nth-of-type(2)") |>
    html_text()
}
page_description <- page_description |>
  str_replace_all("\\[[0-9]+\\]", "") |>
  str_trunc(500, "right")

# get summary

top10_pv <- sum(top_articles$views)
total_pv <- total_pageviews(date_from, date_to, "cs")
summary_line <- str_glue("Česká Wikipedie měla v týdnu od {format(date_from, '%x')} celkem {format(total_pv, big.mark = ' ')} zhlédnutí. 10 nejčtenějších stránek tedy představuje jen {format(round(top10_pv / total_pv * 100, digits = 2), decimal.mark = ',')} %.")

# plot top articles

p <- top_articles |>
  ggplot(aes(x = views, y = fct_reorder(article, views))) +
  coord_fixed(ratio = 9 / 16) +
  geom_col(fill = hsv(0.3, 0.6, 0.4), alpha = 0.8, width = 0.27) +
  geom_text(
    aes(x = 0, y = fct_reorder(article, views), label = str_replace_all(article, fixed("_"), " ")),
    hjust = 0, position = position_nudge(y = 0.44), size = 3.6, color = "gray20"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_textbox(
    aes(x = max(views), y = article[10], label = paste0(
      "**", page_h1, "**\n\n", page_description
    )),
    vjust = 0, hjust = 1,
    width = unit(18, "lines"), stat = "unique",
    alpha = 0.7
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(margin = margin(0, 0, 15, 0)),
    aspect.ratio = 9 / 16
  ) +
  labs(
    title = paste("Nejčtenější články české Wikipedie v týdnu od", format(date_from, "%x")),
    x = NULL, y = NULL,
    caption = summary_line
  )

print(p)
