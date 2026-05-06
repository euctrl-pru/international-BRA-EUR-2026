#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(fs)
  library(glue)
  library(lubridate)
  library(readr)
  library(stringr)
  library(tidyr)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE))
} else {
  "scripts"
}

report_root <- normalizePath(path(script_dir, ".."), mustWork = TRUE)
source(path(report_root, "_chapter-setup.R"))

args <- commandArgs(trailingOnly = TRUE)
totalbr_path <- if (length(args) >= 1) {
  args[[1]]
} else {
  "/Users/rainerkoelle/RProjects/PBWG-BRA/data-src/raw/totalbr.parquet"
}

years <- 2023:2025
flow_year <- 2025
top_n_bump <- 20
nonstudy_threshold <- 90

report_data_dir <- path(report_root, "data")
dir_create(report_data_dir)

normalise_indicator <- function(x) {
  str_to_upper(str_trim(as.character(x)))
}

is_brazil_indicator <- function(x) {
  str_detect(x, "^(SB|SD|SI|SJ|SN|SS|SW)")
}

is_europe_indicator <- function(x) {
  str_detect(x, "^(E|L|BI)")
}

airport_names <- bind_rows(
  eur_apts_names |> mutate(REGION = "EUR"),
  bra_apts_names |> mutate(REGION = "BRA"),
  tibble::tribble(
    ~ICAO, ~NAME, ~REGION,
    "ELLX", "Luxembourg", "EUR",
    "LFPO", "Paris Orly", "EUR",
    "LIMC", "Milan", "EUR",
    "LPPR", "Porto", "EUR",
    "SBSG", "Natal", "BRA",
    "SBFZ", "Fortaleza", "BRA",
    "SBBE", "Belém", "BRA",
    "SBFL", "Florianópolis", "BRA"
  )
) |>
  distinct(ICAO, .keep_all = TRUE)

format_node_label <- function(code) {
  airport_name <- dplyr::recode(code, !!!stats::setNames(airport_names$NAME, airport_names$ICAO), .default = code)
  case_when(
    code == "Other European airports" ~ "Other European\nairports",
    code == "Other Brazilian airports" ~ "Other Brazilian\nairports",
    airport_name == code ~ code,
    TRUE ~ paste0(airport_name, "\n(", code, ")")
  )
}

flow_pairs <- read_parquet(
  normalizePath(totalbr_path, mustWork = TRUE),
  col_select = c(dt_dia, co_addep, co_addes)
) |>
  transmute(
    YEAR = year(as.Date(dt_dia)),
    ADEP = normalise_indicator(co_addep),
    ADES = normalise_indicator(co_addes)
  ) |>
  filter(YEAR %in% years) |>
  mutate(
    ADEP_REGION = case_when(
      is_brazil_indicator(ADEP) ~ "BRA",
      is_europe_indicator(ADEP) ~ "EUR",
      TRUE ~ NA_character_
    ),
    ADES_REGION = case_when(
      is_brazil_indicator(ADES) ~ "BRA",
      is_europe_indicator(ADES) ~ "EUR",
      TRUE ~ NA_character_
    )
  ) |>
  filter(
    (ADEP_REGION == "EUR" & ADES_REGION == "BRA") |
      (ADEP_REGION == "BRA" & ADES_REGION == "EUR")
  ) |>
  count(YEAR, ADEP, ADES, ADEP_REGION, ADES_REGION, name = "FLTS") |>
  mutate(
    EUR_APT = if_else(ADEP_REGION == "EUR", ADEP, ADES),
    BRA_APT = if_else(ADEP_REGION == "BRA", ADEP, ADES),
    EUR_STUDY = EUR_APT %in% eur_apts,
    BRA_STUDY = BRA_APT %in% bra_apts,
    DIRECTION = if_else(ADEP_REGION == "EUR", "Europe to Brazil", "Brazil to Europe")
  )

readr::write_csv(
  flow_pairs,
  path(report_data_dir, "PBWG-BRA-EUR-study-flow-pairs-2023-2025.csv")
)

bump_rank_all <- flow_pairs |>
  filter(DIRECTION == "Europe to Brazil") |>
  summarise(FLTS = sum(FLTS), .by = c(YEAR, EUR_APT, BRA_APT)) |>
  mutate(
    PAIR = paste(EUR_APT, BRA_APT, sep = "-"),
    PAIR_CLASS = case_when(
      EUR_APT %in% eur_apts & BRA_APT %in% bra_apts ~ "Study-Study",
      EUR_APT %in% eur_apts | BRA_APT %in% bra_apts ~ "Study-Non-study",
      TRUE ~ "Non-study-Non-study"
    )
  ) |>
  group_by(YEAR) |>
  arrange(desc(FLTS), PAIR, .by_group = TRUE) |>
  mutate(RANK = row_number()) |>
  ungroup()

bump_pairs <- bump_rank_all |>
  filter(RANK <= top_n_bump) |>
  pull(PAIR) |>
  unique()

bump_rank_plot <- bump_rank_all |>
  filter(PAIR %in% bump_pairs) |>
  complete(YEAR = years, PAIR = bump_pairs) |>
  left_join(
    bump_rank_all |> distinct(PAIR, EUR_APT, BRA_APT, PAIR_CLASS),
    by = "PAIR",
    suffix = c("", "_lookup")
  ) |>
  mutate(
    EUR_APT = coalesce(EUR_APT, EUR_APT_lookup, str_extract(PAIR, "^[^-]+")),
    BRA_APT = coalesce(BRA_APT, BRA_APT_lookup, str_extract(PAIR, "[^-]+$")),
    FLTS = replace_na(FLTS, 0L),
    RANK = if_else(is.na(RANK), top_n_bump + 1L, RANK),
    PAIR = paste(EUR_APT, BRA_APT, sep = "-"),
    PAIR_CLASS = coalesce(PAIR_CLASS, PAIR_CLASS_lookup)
  ) |>
  select(YEAR, PAIR, EUR_APT, BRA_APT, PAIR_CLASS, FLTS, RANK) |>
  group_by(YEAR) |>
  arrange(RANK, PAIR, .by_group = TRUE) |>
  ungroup()

readr::write_csv(
  bump_rank_plot,
  path(report_data_dir, "PBWG-BRA-EUR-study-flow-rank-2023-2025.csv")
)

flow_collapsed <- flow_pairs |>
  filter(YEAR == flow_year, DIRECTION == "Europe to Brazil") |>
  mutate(
    ORIG_NODE = case_when(
      EUR_STUDY ~ ADEP,
      !EUR_STUDY & FLTS >= nonstudy_threshold ~ ADEP,
      TRUE ~ "Other European airports"
    ),
    DEST_NODE = case_when(
      BRA_STUDY ~ ADES,
      !BRA_STUDY & FLTS >= nonstudy_threshold ~ ADES,
      TRUE ~ "Other Brazilian airports"
    ),
    FLOW_CLASS = case_when(
      EUR_STUDY & BRA_STUDY ~ "Study-Study",
      EUR_STUDY | BRA_STUDY ~ "Study-Non-study",
      TRUE ~ "Non-study-Non-study"
    )
  ) |>
  summarise(FLTS = sum(FLTS), .by = c(YEAR, ORIG_NODE, DEST_NODE, FLOW_CLASS)) |>
  mutate(
    ORIG_LABEL = format_node_label(ORIG_NODE),
    DEST_LABEL = format_node_label(DEST_NODE),
    ORIG_STUDY = ORIG_NODE %in% eur_apts,
    DEST_STUDY = DEST_NODE %in% bra_apts,
    THRESHOLD = nonstudy_threshold
  ) |>
  arrange(desc(FLTS), ORIG_NODE, DEST_NODE)

readr::write_csv(
  flow_collapsed,
  path(report_data_dir, "PBWG-BRA-EUR-study-flow-parallel-2025.csv")
)

flow_collapsed |>
  summarise(FLTS = sum(FLTS), .by = FLOW_CLASS) |>
  arrange(desc(FLTS)) |>
  print(n = Inf)
