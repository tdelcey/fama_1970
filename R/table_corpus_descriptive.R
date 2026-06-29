source(here::here("paths_and_packages.R"))

corpus <- readRDS(here(clean_data_path, "corpus.rds")) %>%
  filter(year <= 2010) %>%
  ungroup

graph <- readRDS(here(clean_data_path, "graph_with_color.rds"))

# Top journals
top_journals <- corpus %>%
  count(journal, sort = TRUE) %>%
  slice_head(n = 20) %>%
  rename(Journal = journal, `N articles` = n)

# Top authors (all authors, not just first)
top_authors <- corpus %>%
  unnest(authors) %>%
  count(authors, sort = TRUE) %>%
  slice_head(n = 20) %>%
  rename(Author = authors, `N articles` = n)

# Top articles by total WoS citations
top_articles <- corpus %>%
  arrange(desc(n)) %>%
  slice_head(n = 20) %>%
  select(title, first_author, year, journal, n) %>%
  rename(
    Title = title,
    Author = first_author,
    Year = year,
    Journal = journal,
    `WoS citations` = n
  )


saveRDS(top_journals, here(clean_data_path, "top_journals.rds"))
saveRDS(top_authors, here(clean_data_path, "top_authors.rds"))
saveRDS(top_articles, here(clean_data_path, "top_articles.rds"))
