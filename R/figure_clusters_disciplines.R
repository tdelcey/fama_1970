# load paths and packages

source(here::here("paths_and_packages.R"))

corpus <- readRDS(here(clean_data_path, "corpus.rds")) %>%
  ungroup %>%
  filter(year < 2011)
list_journal <- xlsx::read.xlsx(
  here(clean_data_path, "list_journal.xlsx"),
  sheetIndex = 1
)

graphs <- readRDS(here(clean_data_path, "graph_with_color.rds"))

corpus <- corpus %>%
  left_join(list_journal, by = "journal")


data_plot <- corpus %>%
  mutate(
    field = ifelse(
      !str_detect(field, "Economics|Finance|Management|Law"),
      "Others",
      field
    )
  ) %>%
  group_by(year, field) %>%
  summarise(n = n()) %>%
  group_by(year) %>%
  mutate(n_perc = n / sum(n))

label_data <- data_plot %>%
  group_by(field) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(label_x = year + 0.5)

gg1 <- ggplot(
  data_plot,
  aes(x = year, y = n_perc, group = field, color = field)
) +
  geom_smooth(se = F, method = 'loess', span = 0.75, linewidth = 1.5) +
  scale_color_manual(
    name = "",
    values = c(
      "Economics" = "#2171b5",
      "Finance" = "#a63603",
      "Management" = "#B8860B",
      "Law" = "#31a354",
      "Others" = "#bdbdbd"
    )
  ) +
  geom_label_repel(
    data = label_data,
    aes(x = label_x, y = n_perc, label = field),
    hjust = 0,
    size = 3,
    label.size = 0.2,
    show.legend = FALSE
  ) +
  # increase x limits to make space for labels
  xlim(1970, 2015.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Share of documents (%)") +
  theme_light(base_size = 16) +
  theme(
    legend.position = "none"
  )

# Save the plot
ggsave(
  here(project_path, "image", "field_over_time.png"),
  gg1,
  width = 8,
  height = 4.5,
  dpi = 300
)

# field by community

# compute first the chi-sq test

chi_data <- graphs %>%
  activate(nodes) %>%
  as_tibble() %>%
  left_join(list_journal, by = "journal") %>%
  filter(!is.na(cluster_label)) %>%
  filter(cluster_label != "Varia") %>%
  filter(str_detect(field, "Economics|Finance|Law|Management"))

chi_tbl <- table(chi_data$cluster_label, chi_data$field)
# chisq_test <- chisq.test(chi_tbl, simulate.p.value = TRUE, B = 5000)
chisq_test <- chisq.test(chi_tbl, simulate.p.value = TRUE, B = 5000)

# prepare data for plotting

data_summary <- graphs %>%
  activate(nodes) %>%
  as_tibble() %>%
  left_join(list_journal, by = "journal") %>%
  group_by(cluster_label, field) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(total = sum(count), percentage = (count / total) * 100) %>%
  filter(cluster_label != "Varia") %>%
  mutate(
    field = ifelse(
      !str_detect(field, "Economics|Finance|Management|Law"),
      "Others",
      field
    )
  )

p_label <- paste0(
  "Chi-square (MC p-value): ",
  formatC(chisq_test$p.value, format = "f", digits = 4)
)

# plot
gg <- ggplot(data_summary, aes(x = field, y = percentage, fill = field)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.1)) +
  scale_fill_manual(
    name = "",
    values = c(
      "Economics" = "#2171b5",
      "Finance" = "#a63603",
      "Management" = "#B8860B",
      "Law" = "#31a354",
      "Others" = "#bdbdbd"
    )
  ) +
  facet_wrap(~cluster_label, ncol = 3) +
  labs(
    subtitle = p_label,
    x = "",
    y = "%"
  ) +
  theme_light() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 26),
    axis.text.x = element_blank(), # Remove x-axis text
    # panel.background = element_blank(),  # Remove panel background
    strip.background = element_rect(fill = "white", colour = "black"), # White background for facet labels
    strip.text.x = element_text(color = "black") # Customize facet label text
  )

ggsave(
  "field_communities.jpg",
  device = "jpg",
  plot = gg,
  path = here(project_path, "image"),
  width = 16,
  height = 9,
  dpi = 300
)

# -------------------------------------------------------------------
# Robustness checks: alternative discipline classification rules
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 0) Baseline field (collapse everything outside big 4 into Others)
# -------------------------------------------------------------------
corpus0 <- corpus %>%
  mutate(
    field_base = ifelse(
      !str_detect(field, "Economics|Finance|Management|Law"),
      "Others",
      field
    )
  )

# -------------------------------------------------------------------
# 1) Robustness rule A: "Finance always wins"
# -------------------------------------------------------------------
corpus_fin <- corpus0 %>%
  mutate(
    field_fin_wins = case_when(
      str_detect(
        journal,
        regex("\\bfinance\\b|\\bfinancial\\b", ignore_case = TRUE)
      ) ~ "Finance",
      TRUE ~ field_base
    )
  )

# -------------------------------------------------------------------
# 2) Robustness rule B: "Economics always wins"
# -------------------------------------------------------------------
corpus_econ <- corpus0 %>%
  mutate(
    field_econ_wins = case_when(
      str_detect(
        journal,
        regex("\\beconomics\\b|\\beconomic\\b", ignore_case = TRUE)
      ) ~ "Economics",
      TRUE ~ field_base
    )
  )

# -------------------------------------------------------------------
# 3) Robustness rule C: "Management always wins"
# -------------------------------------------------------------------
corpus_mgmt <- corpus0 %>%
  mutate(
    field_mgmt_wins = case_when(
      str_detect(
        journal,
        regex(
          "\\bmanagement\\b|\\bmanagerial\\b|\\bbusiness\\b|\\borganization\\b|\\borganizational\\b|\\bstrategy\\b",
          ignore_case = TRUE
        )
      ) ~ "Management",
      TRUE ~ field_base
    )
  )

# -------------------------------------------------------------------
# Helper: build time series data
# -------------------------------------------------------------------
build_data_plot <- function(df, field_var, rule_name) {
  df %>%
    mutate(field_plot = .data[[field_var]]) %>%
    group_by(year, field_plot) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(year) %>%
    mutate(n_perc = n / sum(n)) %>%
    ungroup() %>%
    rename(field = field_plot) %>%
    mutate(rule = rule_name)
}

data_plot_base <- build_data_plot(corpus0, "field_base", "Baseline")
data_plot_fin <- build_data_plot(
  corpus_fin,
  "field_fin_wins",
  "Finance always wins"
)
data_plot_econ <- build_data_plot(
  corpus_econ,
  "field_econ_wins",
  "Economics always wins"
)
data_plot_mgmt <- build_data_plot(
  corpus_mgmt,
  "field_mgmt_wins",
  "Management always wins"
)

data_plot_all <- bind_rows(
  data_plot_base,
  data_plot_fin,
  data_plot_econ,
  data_plot_mgmt
)

label_data_all <- data_plot_all %>%
  group_by(rule, field) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(label_x = year + 0.5)

# -------------------------------------------------------------------
# Plot: 4 panels
# -------------------------------------------------------------------
gg_all <- ggplot(
  data_plot_all,
  aes(x = year, y = n_perc, group = field, color = field)
) +
  geom_smooth(se = FALSE, method = "loess", span = 0.75, linewidth = 1.5) +
  scale_color_manual(
    name = "",
    values = c(
      "Economics" = "#2171b5",
      "Finance" = "#a63603",
      "Management" = "#B8860B",
      "Law" = "#31a354",
      "Others" = "#bdbdbd"
    )
  ) +
  geom_label_repel(
    data = label_data_all,
    aes(x = label_x, y = n_perc, label = field),
    hjust = 0,
    size = 3,
    label.size = 0.2,
    show.legend = FALSE
  ) +
  xlim(1970, 2015.5) +
  labs(x = NULL, y = "Proportion of documents (%)") +
  facet_wrap(~rule, ncol = 1) +
  theme_light(base_size = 16) +
  theme(legend.position = "none")

ggsave(
  here(project_path, "image", "field_over_time_robustness_4rules.png"),
  gg_all,
  width = 8,
  height = 13,
  dpi = 300
)

# -------------------------------------------------------------------
# Optional diagnostics
# -------------------------------------------------------------------
changed_fin_wins <- corpus_fin %>%
  distinct(journal, field_base, field_fin_wins) %>%
  filter(field_base != field_fin_wins)

changed_econ_wins <- corpus_econ %>%
  distinct(journal, field_base, field_econ_wins) %>%
  filter(field_base != field_econ_wins)

changed_mgmt_wins <- corpus_mgmt %>%
  distinct(journal, field_base, field_mgmt_wins) %>%
  filter(field_base != field_mgmt_wins)

changed_fin_wins
changed_econ_wins
changed_mgmt_wins
