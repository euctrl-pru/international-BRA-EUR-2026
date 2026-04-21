# Temporary Brazil airport traffic count builder for the fleet mix section.
#
# This script keeps the current report reproducible while the Brazil WTC counts
# are not yet produced by the standard PBWG APDF airport-traffic pipeline.
# Replace this file and data/BRA-apt-tfc-2023-2025.csv once the upstream APDF
# pipeline emits the proper daily airport traffic counts with WTC classes.

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
  library(lubridate)
  library(tibble)
})

repo_root <- normalizePath(file.path(getwd()), mustWork = TRUE)
if (!file.exists(file.path(repo_root, "03-traffic_characterisation.qmd"))) {
  stop("Run this script from the BRA-EUR-2025 report repository root.", call. = FALSE)
}

source_dir <- "/Users/rainerkoelle/RProjects/PBWG-BRA/data/apdf"
lookup_path <- "/Users/rainerkoelle/RProjects/BRA-EUR-2023/data/ac_wtc_class.csv"
output_path <- file.path(repo_root, "data", "BRA-apt-tfc-2023-2025.csv")

bra_apts <- c(
  "SBGR", "SBGL", "SBRJ", "SBCF", "SBBR", "SBSV",
  "SBKP", "SBSP", "SBCT", "SBPA", "SBRF", "SBEG"
)

ac_wtc <- readr::read_csv(lookup_path, show_col_types = FALSE) |>
  transmute(
    ARCTYP = toupper(trimws(TYPE)),
    CLASS
  )

# Temporary aliases for aircraft type strings observed in the PBWG-BRA APDF
# extracts but not present in the older report lookup table.
type_aliases <- tibble::tribble(
  ~ARCTYP, ~CLASS,
  "B7M8", "MJ",
  "B73M", "MJ",
  "A32N", "MJ",
  "A32F", "MJ",
  "A330", "HJ",
  "A350", "HJ",
  "B747", "HJ",
  "B767", "HJ",
  "B777", "HJ",
  "B787", "HJ",
  "B78",  "HJ",
  "B78M", "HJ",
  "KC39", "HJ",
  "A76",  "MT",
  "PA28", "LP",
  "BH06", "HEL",
  "BH07", "HEL",
  "H145", "HEL",
  "SK76", "HEL",
  "RH66", "HEL",
  "ULAC", "LP",
  "C98",  "LT",
  "MA2",  "LT",
  "T27",  "LT"
)

ac_wtc <- bind_rows(ac_wtc, type_aliases) |>
  distinct(ARCTYP, .keep_all = TRUE)

build_year <- function(year) {
  input_path <- file.path(
    source_dir,
    sprintf("PBWG-BRA-dsTaxi-apdf-%s.parquet", year)
  )

  if (!file.exists(input_path)) {
    stop("Missing APDF extract: ", input_path, call. = FALSE)
  }

  arrow::read_parquet(input_path) |>
    filter(ICAO %in% bra_apts) |>
    mutate(
      ARCTYP = toupper(trimws(ARCTYP)),
      DATE = as.Date(coalesce(BLOCK_TIME, MVT_TIME))
    ) |>
    left_join(ac_wtc, by = "ARCTYP") |>
    group_by(ICAO, DATE) |>
    summarise(
      ARRS = sum(PHASE == "ARR", na.rm = TRUE),
      DEPS = sum(PHASE == "DEP", na.rm = TRUE),
      SRC_NA = sum(is.na(PHASE)),
      ARRS_REG = NA_integer_,
      DEPS_REG = NA_integer_,
      HEL = sum(CLASS %in% "HEL", na.rm = TRUE),
      H = sum(substr(CLASS, 1, 1) %in% "H", na.rm = TRUE),
      M = sum(substr(CLASS, 1, 1) %in% "M", na.rm = TRUE),
      L = sum(substr(CLASS, 1, 1) %in% "L", na.rm = TRUE),
      WTC_NA = sum(is.na(CLASS)),
      .groups = "drop"
    ) |>
    filter(lubridate::year(DATE) == year)
}

out <- bind_rows(lapply(2023:2025, build_year)) |>
  arrange(ICAO, DATE)

readr::write_csv(out, output_path)

message("Wrote ", nrow(out), " daily airport rows to ", output_path)

out |>
  mutate(YEAR = lubridate::year(DATE)) |>
  summarise(
    AIRPORTS = n_distinct(ICAO),
    DAYS = n_distinct(DATE),
    TOTAL = sum(ARRS + DEPS),
    WTC_NA = sum(WTC_NA),
    WTC_NA_SHARE = WTC_NA / TOTAL,
    .by = YEAR
  ) |>
  print(n = Inf)
