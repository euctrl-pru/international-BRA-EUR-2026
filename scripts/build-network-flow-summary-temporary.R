#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(fs)
  library(lubridate)
  library(purrr)
  library(readr)
  library(stringr)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE))
} else {
  "scripts"
}

report_root <- normalizePath(path(script_dir, ".."), mustWork = TRUE)
pbwg_root <- normalizePath(path(report_root, "..", "PBWG"), mustWork = TRUE)
pbwg_bra_root <- normalizePath(path(report_root, "..", "PBWG-BRA"), mustWork = TRUE)

totalbr_path <- if (length(args) >= 1) {
  args[[1]]
} else {
  path(pbwg_bra_root, "data-src", "raw", "totalbr.parquet")
}
nm_zip_path <- if (length(args) >= 2) {
  args[[2]]
} else {
  "/Users/rainerkoelle/RProjects/__DATA/NM-flight-table/NM-flt-2025.zip"
}
airport_lookup_path <- if (length(args) >= 3) {
  args[[3]]
} else {
  "/Users/rainerkoelle/RProjects/international-CHN-EUR/01_data-raw/ourairports.csv"
}
country_lookup_path <- if (length(args) >= 4) {
  args[[4]]
} else {
  "/Users/rainerkoelle/RProjects/BRA-EUR-2024/data/country-icao-iso-etc.csv"
}

devtools::load_all(pbwg_root, quiet = TRUE)

report_data_dir <- path(report_root, "data")
pbwg_bra_output_dir <- path(pbwg_bra_root, "outputs", "network-flow")
dir_create(report_data_dir)
dir_create(pbwg_bra_output_dir)

normalise_indicator <- function(x) {
  str_to_upper(str_trim(as.character(x)))
}

is_special_indicator <- function(x) {
  x %in% c("AFIL", "ZZZZ", "ZZZ")
}

world_region_from_country <- function(country, continent, eurocontrol_pru) {
  case_when(
    country == "BR" ~ "Brazil",
    !is.na(eurocontrol_pru) & eurocontrol_pru == "Eurocontrol" ~ "Europe",
    continent == "Europe" ~ "Europe",
    country %in% c("AR", "BO", "CL", "CO", "EC", "FK", "GF", "GY", "PE", "PY", "SR", "UY", "VE") ~ "South America",
    continent == "Africa" ~ "Africa",
    country %in% c("CA", "GL", "PM", "US") ~ "North America",
    continent == "Americas" ~ "Latin America and Caribbean",
    country %in% c("AE", "BH", "IL", "IQ", "IR", "JO", "KW", "LB", "OM", "PS", "QA", "SA", "SY", "YE") ~ "Middle East",
    continent == "Asia" ~ "Asia/Pacific",
    continent == "Oceania" ~ "Asia/Pacific",
    TRUE ~ "Unmapped"
  )
}

airport_lookup <- read_csv(airport_lookup_path, show_col_types = FALSE) |>
  transmute(
    ICAO = normalise_indicator(ident),
    ISO_COUNTRY = iso_country
  ) |>
  filter(!is.na(ICAO), !is.na(ISO_COUNTRY)) |>
  distinct(ICAO, .keep_all = TRUE)

country_lookup <- read_csv(country_lookup_path, show_col_types = FALSE) |>
  transmute(
    ISO_COUNTRY = iso2c,
    COUNTRY_NAME = country.name.en,
    CONTINENT = continent,
    EUROCONTROL_PRU = eurocontrol_pru
  ) |>
  distinct(ISO_COUNTRY, .keep_all = TRUE)

enrich_pairs <- function(data, reg, classifier) {
  data |>
    mutate(
      ADEP = normalise_indicator(ADEP),
      ADES = normalise_indicator(ADES)
    ) |>
    left_join(airport_lookup, by = c("ADEP" = "ICAO")) |>
    rename(ADEP_COUNTRY_LOOKUP = ISO_COUNTRY) |>
    left_join(airport_lookup, by = c("ADES" = "ICAO")) |>
    rename(ADES_COUNTRY_LOOKUP = ISO_COUNTRY) |>
    mutate(
      ADEP_COUNTRY_LOOKUP = if_else(is_special_indicator(ADEP), NA_character_, ADEP_COUNTRY_LOOKUP),
      ADES_COUNTRY_LOOKUP = if_else(is_special_indicator(ADES), NA_character_, ADES_COUNTRY_LOOKUP),
      ADEP_RAW_IN_REGION = classifier(ADEP),
      ADES_RAW_IN_REGION = classifier(ADES),
      ADEP_IN_REGION = if_else(ADEP == "AFIL" & ADES_RAW_IN_REGION, TRUE, ADEP_RAW_IN_REGION),
      ADES_IN_REGION = if_else(ADES == "AFIL" & ADEP_RAW_IN_REGION, TRUE, ADES_RAW_IN_REGION),
      ADEP_COUNTRY = case_when(
        reg == "BRA" & ADEP_IN_REGION ~ "BR",
        !is.na(ADEP_COUNTRY_LOOKUP) ~ ADEP_COUNTRY_LOOKUP,
        TRUE ~ NA_character_
      ),
      ADES_COUNTRY = case_when(
        reg == "BRA" & ADES_IN_REGION ~ "BR",
        !is.na(ADES_COUNTRY_LOOKUP) ~ ADES_COUNTRY_LOOKUP,
        TRUE ~ NA_character_
      ),
      DAIO = case_when(
        ADEP_IN_REGION & !ADES_IN_REGION ~ "D",
        !ADEP_IN_REGION & ADES_IN_REGION ~ "A",
        ADEP_IN_REGION & ADES_IN_REGION ~ "I",
        !ADEP_IN_REGION & !ADES_IN_REGION ~ "O",
        TRUE ~ NA_character_
      ),
      REG = reg
    ) |>
    left_join(country_lookup, by = c("ADEP_COUNTRY" = "ISO_COUNTRY")) |>
    rename(
      ADEP_COUNTRY_NAME = COUNTRY_NAME,
      ADEP_CONTINENT = CONTINENT,
      ADEP_EUROCONTROL_PRU = EUROCONTROL_PRU
    ) |>
    left_join(country_lookup, by = c("ADES_COUNTRY" = "ISO_COUNTRY")) |>
    rename(
      ADES_COUNTRY_NAME = COUNTRY_NAME,
      ADES_CONTINENT = CONTINENT,
      ADES_EUROCONTROL_PRU = EUROCONTROL_PRU
    ) |>
    mutate(
      ADEP_WORLD_REGION = world_region_from_country(ADEP_COUNTRY, ADEP_CONTINENT, ADEP_EUROCONTROL_PRU),
      ADES_WORLD_REGION = world_region_from_country(ADES_COUNTRY, ADES_CONTINENT, ADES_EUROCONTROL_PRU),
      ADEP_COUNTRY_NAME = if_else(is.na(ADEP_COUNTRY), NA_character_, ADEP_COUNTRY_NAME),
      ADES_COUNTRY_NAME = if_else(is.na(ADES_COUNTRY), NA_character_, ADES_COUNTRY_NAME)
    ) |>
    select(
      REG, YEAR, ADEP, ADES, FLTS,
      ADEP_IN_REGION, ADES_IN_REGION, DAIO,
      ADEP_COUNTRY, ADEP_COUNTRY_NAME, ADEP_WORLD_REGION,
      ADES_COUNTRY, ADES_COUNTRY_NAME, ADES_WORLD_REGION
    )
}

totalbr_pairs <- read_parquet(
  normalizePath(totalbr_path, mustWork = TRUE),
  col_select = c(dt_dia, co_addep, co_addes)
) |>
  transmute(
    YEAR = year(as.Date(dt_dia)),
    ADEP = co_addep,
    ADES = co_addes
  ) |>
  filter(YEAR == 2025) |>
  count(YEAR, ADEP, ADES, name = "FLTS")

bra_pairs <- enrich_pairs(
  totalbr_pairs,
  reg = "BRA",
  classifier = PBWG::is_brazil_airport_indicator
)

nm_files <- PBWG::check_zip_content(
  path = dirname(normalizePath(nm_zip_path, mustWork = TRUE)),
  archive = basename(nm_zip_path)
)$Name

eur_pairs <- map(
  nm_files,
  function(file_name) {
    PBWG::read_nm_flights_zip(
      zipped_archive_path = nm_zip_path,
      files = file_name,
      type = "parquet"
    ) |>
      transmute(
        YEAR = year(as.Date(LOBT)),
        ADEP,
        ADES
      ) |>
      filter(YEAR == 2025) |>
      count(YEAR, ADEP, ADES, name = "FLTS")
  }
) |>
  bind_rows() |>
  summarise(FLTS = sum(FLTS), .by = c(YEAR, ADEP, ADES))

eur_pairs <- enrich_pairs(
  eur_pairs,
  reg = "EUR",
  classifier = PBWG::is_eurocontrol_airport
)

network_pairs <- bind_rows(bra_pairs, eur_pairs) |>
  arrange(REG, YEAR, desc(FLTS), ADEP, ADES)

write_csv(network_pairs, path(report_data_dir, "PBWG-BRA-EUR-network-flow-pairs-2025.csv"))
write_csv(network_pairs, path(pbwg_bra_output_dir, "PBWG-BRA-EUR-network-flow-pairs-2025.csv"))

airport_rank <- network_pairs |>
  filter(ADEP_IN_REGION) |>
  summarise(
    DEPS = sum(FLTS),
    NBR_DES = n_distinct(ADES),
    .by = c(REG, YEAR, ADEP)
  ) |>
  arrange(REG, YEAR, desc(DEPS), ADEP) |>
  group_by(REG, YEAR) |>
  mutate(
    RANK = row_number(),
    SHARE = DEPS / sum(DEPS),
    CUM_SHARE = cumsum(SHARE)
  ) |>
  ungroup()

daio_share <- network_pairs |>
  summarise(FLTS = sum(FLTS), .by = c(REG, YEAR, DAIO)) |>
  group_by(REG, YEAR) |>
  mutate(SHARE = FLTS / sum(FLTS)) |>
  ungroup()

world_region_share <- network_pairs |>
  filter(ADEP_IN_REGION, !ADES_IN_REGION) |>
  summarise(FLTS = sum(FLTS), .by = c(REG, YEAR, ADES_WORLD_REGION)) |>
  group_by(REG, YEAR) |>
  mutate(SHARE = FLTS / sum(FLTS)) |>
  ungroup()

country_share <- network_pairs |>
  filter(ADEP_IN_REGION, !ADES_IN_REGION) |>
  summarise(FLTS = sum(FLTS), .by = c(REG, YEAR, ADES_COUNTRY, ADES_COUNTRY_NAME, ADES_WORLD_REGION)) |>
  group_by(REG, YEAR) |>
  mutate(SHARE = FLTS / sum(FLTS)) |>
  ungroup()

readr::write_csv(airport_rank, path(report_data_dir, "PBWG-BRA-EUR-airport-departure-rank-2025.csv"))
readr::write_csv(daio_share, path(report_data_dir, "PBWG-BRA-EUR-daio-share-2025.csv"))
readr::write_csv(world_region_share, path(report_data_dir, "PBWG-BRA-EUR-world-region-departures-2025.csv"))
readr::write_csv(country_share, path(report_data_dir, "PBWG-BRA-EUR-country-departures-2025.csv"))

print(network_pairs |> summarise(FLTS = sum(FLTS), .by = c(REG, YEAR)))
print(airport_rank |> filter(RANK %in% c(10, 50, 100)) |> select(REG, YEAR, RANK, CUM_SHARE))
print(daio_share)
