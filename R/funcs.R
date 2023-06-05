top_articles_by_day <- function(date_from, date_to, lang) {
  wiki_project <- paste(lang, "wikipedia", sep = ".")
  seq(date_from, date_to, by = "day") |>
    map_dfr(~ top_articles(project = wiki_project, start = .x)) |>
    filter(
      article != "Hlavní_strana",
      !str_starts(article, fixed("Speciální:")),
      !str_starts(article, fixed("Wikipedie:"))
    ) |>
    as_tibble()
}

top_articles_by_month <- function(date_from, date_to, lang) {
  wiki_project <- paste(lang, "wikipedia", sep = ".")
  seq(date_from, date_to, by = "month") |>
    map_dfr(~ top_articles(project = wiki_project, start = .x, granularity = "monthly")) |>
    filter(
      article != "Hlavní_strana",
      !str_starts(article, fixed("Speciální:"))
    ) |>
    as_tibble()
}

summarise_top_articles <- function(articles, n) {
  base_url <- paste0("https://", articles$language[1], ".wikipedia.org/wiki/")
  articles |>
    group_by(article) |>
    summarise(views = sum(views)) |>
    slice_max(order_by = views, n = n * 2) |>
    mutate(
      article = map_chr(
        article,
        \(x) {
          read_html(paste0(base_url, x)) |>
            html_element("h1") |>
            html_text()
        }
      )
    ) |>
    group_by(article) |>
    summarise(views = sum(views)) |>
    slice_max(order_by = views, n = n) |>
    mutate(article = fct_reorder(article, views))
}

total_pageviews <- function(date_from, date_to, lang) {
  project_pageviews(
    project = "cs.wikipedia",
    start = pageview_timestamps(date_from, first = TRUE),
    end = pageview_timestamps(date_to, first = FALSE)
  ) |>
    summarise(views = sum(views)) |>
    pull(views)

}

format_date <- function(x) {
  x |>
    format("%x") |>
    str_remove_all("\\b0") |>
    str_replace_all("\\.", ". ")
}
