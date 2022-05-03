# libraries

library(pageviews)
library(tidyverse)
library(lubridate)
library(scales)
library(tsibble)
library(rvest)
library(httr)
library(ggtext)
library(magick)


# setting params

options(lubridate.week.start = 1)
date_from <- floor_date(today() - 7, unit = "week")
date_to <- date_from + 6
wiki_project <- "cs.wikipedia"

# top articles by day

top_articles_by_day <- seq(date_from, date_to, by = "day") |>
  map_dfr(~ top_articles(project = wiki_project, start = .x)) |>
  filter(
    article != "Hlavní_strana",
    !str_starts(article, fixed("Speciální:"))
  )

# top articles total

top_articles <- top_articles_by_day |>
  group_by(article) |>
  summarise(views = sum(views)) |>
  slice_max(order_by = views, n = 20) |>
  mutate(
    article = map_chr(
      article,
      \(x) {
        read_html(paste0("https://cs.wikipedia.org/wiki/", x)) |>
          html_element("h1") |>
          html_text()
      }
    )
  ) |>
  group_by(article) |>
  summarise(views = sum(views)) |>
  slice_max(order_by = views, n = 10) |>
  arrange(desc(views))

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
  html_text()
if (nchar(page_description) < 30) {
  page_description <- page_content |>
    html_element("p:nth-of-type(2)") |>
    html_text()
}
# page_image_url <- page_content |>
#   html_element("meta[property='og:image']") |>
#   html_attr("content")
# img <- image_read(page_image_url)


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
    width = unit(13, "lines"), stat = "unique"
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
    x = NULL, y = NULL
  )

print(p)
