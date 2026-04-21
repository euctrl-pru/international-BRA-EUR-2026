#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(fs)
  library(lubridate)
  library(readr)
  library(zoo)
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

source_path <- if (length(args) >= 1) {
  args[[1]]
} else {
  path(pbwg_bra_root, "data-src", "raw", "totalbr.parquet")
}
source_path <- normalizePath(source_path, mustWork = TRUE)

devtools::load_all(pbwg_root, quiet = TRUE)

output_dir <- path(pbwg_bra_root, "outputs", "network-traffic")
data_dir <- path(pbwg_bra_root, "data", "network-traffic")
report_data_dir <- path(report_root, "data")
dir_create(output_dir)
dir_create(data_dir)
dir_create(report_data_dir)

totalbr <- read_parquet(
  source_path,
  col_select = c(
    dt_dia, co_indicativo, co_addep, co_addes, co_modelo, li_tipovoo,
    TP_VOO_VALIDADO, year
  )
)

apdf_like <- PBWG::coerce_totalbr_to_apdf_network(totalbr)
write_parquet(
  apdf_like,
  path(data_dir, "PBWG-BRA-totalbr-network-apdf-2023-2025.parquet")
)

bra_daily <- PBWG::prepare_totalbr_regional_traffic(totalbr) |>
  filter(year(DATE) %in% 2023:2025)

for (yr in 2023:2025) {
  PBWG::write_pbwg_network_traffic(
    data = filter(bra_daily, year(DATE) == yr),
    year = yr,
    output_dir = output_dir,
    region = "BRA"
  )
}

PBWG::combine_pbwg_network_traffic_years(
  years = 2023:2025,
  annual_dir = output_dir,
  region = "BRA"
)

file_copy(
  path(output_dir, "PBWG-BRA-network-traffic-2023-2025.csv"),
  path(report_data_dir, "PBWG-BRA-network-traffic-2023-2025.csv"),
  overwrite = TRUE
)

daily_report <- bra_daily |>
  transmute(
    DATE,
    DLY_FLTS = FLTS,
    MVTS_NORM_ROLLAVG = rollmedian(FLTS, k = 7, fill = NA, align = "center")
  )
write_parquet(daily_report, path(report_data_dir, "ndf_bra_totalbr_daily.parquet"))

yoy_report <- bra_daily |>
  transmute(
    DATE,
    DLY_FLTS = FLTS,
    YEAR = factor(year(DATE)),
    DOY = yday(DATE)
  ) |>
  group_by(YEAR) |>
  mutate(
    MVTS_ROLLAVG = rollapply(
      DLY_FLTS, width = 28, FUN = median,
      partial = TRUE, align = "center"
    )
  ) |>
  ungroup()
write_parquet(yoy_report, path(report_data_dir, "ndf_bra_totalbr_annually.parquet"))

bra_daily |>
  mutate(YEAR = year(DATE)) |>
  summarise(
    DAYS = n(),
    FLTS = sum(FLTS),
    I = sum(I),
    D = sum(D),
    A = sum(A),
    O = sum(O),
    .by = YEAR
  ) |>
  print()
