source(here::here("R", "paths_and_packages.R"))

corpus <- readRDS(here(clean_data_path, "corpus.rds")) %>%
  filter(year <= 2010) %>%
  ungroup

graph <- readRDS(here(clean_data_path, "graph_with_color.rds"))

# Top journals
top_journals <- corpus %>%
  count(journal, sort = TRUE) %>%
  slice_head(n = 20) %>%
  mutate(journal = str_to_lower(journal)) %>%
  rename(Journal = journal, `Number of articles` = n)

# Top authors (all authors, not just first)
top_authors <- corpus %>%
  unnest(authors) %>%
  count(authors, sort = TRUE) %>%
  slice_head(n = 20) %>%
  mutate(authors = str_to_lower(authors)) %>%
  rename(Authors = authors, `Number of articles` = n)

# Top articles by total WoS citations
top_articles <- corpus %>%
  arrange(desc(n)) %>%
  slice_head(n = 20) %>%
  mutate(
    authors = map_chr(authors, ~str_c(str_to_lower(.x), collapse = ", ")),
    title   = str_to_lower(title),
    journal = str_to_lower(journal)
  ) %>%
  select(title, authors, year, journal, n) %>%
  rename(
    Title = title,
    Authors = authors,
    Year = year,
    Journal = journal,
    `WoS citations` = n
  )


saveRDS(top_journals, here(clean_data_path, "top_journals.rds"))
saveRDS(top_authors, here(clean_data_path, "top_authors.rds"))
saveRDS(top_articles, here(clean_data_path, "top_articles.rds"))
