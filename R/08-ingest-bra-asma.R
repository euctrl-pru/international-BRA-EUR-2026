library(dplyr)
library(readr)
library(lubridate)
library(purrr)
library(fs)
library(arrow)
library(data.table)
library(stringr)
library(tidyr)

devtools::load_all("/Users/rainerkoelle/RProjects/PBWG", quiet = TRUE)
source("/Users/rainerkoelle/RProjects/BRA-EUR-2025/_chapter-setup.R")

zip_path <- "/Users/rainerkoelle/Downloads/OneDrive_1_22-04-2026.zip"
archive_root <- "/Users/rainerkoelle/RProjects/PBWG-BRA"
report_data_dir <- "/Users/rainerkoelle/RProjects/BRA-EUR-2025/data"

years <- 2023:2025
ref_year <- 2024L
variant <- "icao_ganp_p20"
min_n <- 5L
max_asma <- 180
study_airports <- bra_apts

dir_create(path(archive_root, "data", "asma"))
dir_create(path(archive_root, "outputs", "asma-reference-2024"))
walk(years, ~ dir_create(path(archive_root, "outputs", paste0("asma-daily-", .x))))
dir_create(report_data_dir)

normalise_text <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | x == "NA"] <- NA_character_
  x
}

parse_bra_timestamp <- function(x) {
  x <- normalise_text(x)
  parse_date_time(
    x,
    orders = c("ymd HMS", "ymd HMS OS", "ymd_HMS", "ymd_HMS_OS"),
    tz = "UTC",
    quiet = TRUE
  )
}

parse_duration_minutes <- function(x) {
  x <- normalise_text(x)
  sign <- ifelse(grepl("^-", x), -1, 1)
  x <- sub("^-", "", x)

  parts <- str_match(x, "^([0-9]+):([0-9]{2}):([0-9]{2}(?:\\.[0-9]+)?)$")
  out <- suppressWarnings(
    as.numeric(parts[, 2]) * 60 +
      as.numeric(parts[, 3]) +
      as.numeric(parts[, 4]) / 60
  )
  sign * out
}

normalise_rwy <- function(validated, raw) {
  out <- coalesce(normalise_text(validated), normalise_text(raw))
  out <- str_to_upper(out)
  out[out %in% c("", "NA")] <- NA_character_
  out
}

normalise_range <- function(x) {
  x <- normalise_text(x)
  case_when(
    x %in% "40" ~ "C40",
    x %in% "100" ~ "C100",
    !is.na(x) ~ str_c("C", x),
    TRUE ~ NA_character_
  )
}

normalise_sector <- function(x) {
  x <- suppressWarnings(as.integer(normalise_text(x)))
  if_else(is.na(x), NA_character_, str_c("S", str_pad(x, width = 3, pad = "0")))
}

calc_reference_value <- function(x, variant) {
  switch(
    variant,
    icao_ganp_p20 = as.numeric(quantile(x, probs = 0.20, names = FALSE, na.rm = TRUE)),
    pbwg_avg_p05_p15 = {
      q05 <- as.numeric(quantile(x, probs = 0.05, names = FALSE, na.rm = TRUE))
      q15 <- as.numeric(quantile(x, probs = 0.15, names = FALSE, na.rm = TRUE))
      (q05 + q15) / 2
    },
    rlang::abort(str_c("Unsupported reference variant: ", variant))
  )
}

read_bra_asma_year <- function(year) {
  member <- paste0("kpi08_", year, ".csv")
  message("Reading ", member)

  raw <- fread(
    cmd = sprintf("unzip -p %s %s", shQuote(zip_path), shQuote(member)),
    sep = ";",
    na.strings = c("", "NA"),
    colClasses = "character",
    showProgress = FALSE,
    data.table = FALSE
  ) |>
    as_tibble()

  harmonised <- raw |>
    transmute(
      IM_SAMAD_ID = normalise_text(id),
      FLTID = normalise_text(fltid),
      REG = normalise_text(reg),
      ADEP = normalise_text(adep),
      ADES = normalise_text(ades),
      ICAO = ADES,
      PHASE = "ARR",
      CLASS = normalise_text(CLASS),
      ARCTYP = normalise_text(type),
      FLTRUL = normalise_text(fltrul),
      RWY = normalise_rwy(drwy_validado, drwy),
      STND = normalise_text(stnd),
      RANGE = normalise_range(c),
      SECTOR = normalise_text(setor),
      SECTOR_GROUP = normalise_sector(setor),
      BEARING = suppressWarnings(as.numeric(normalise_text(bear))),
      CROSS_TIME = parse_bra_timestamp(c_time),
      BLOCK_TIME = parse_bra_timestamp(aibt),
      MVT_TIME = parse_bra_timestamp(aldt),
      SCHED_TIME = parse_bra_timestamp(sibt),
      ASMA_TIME = parse_duration_minutes(transito),
      BRA_REFERENCE_TIME = parse_duration_minutes(desimp),
      BRA_KPI08 = parse_duration_minutes(kpi08),
      YEAR = year
    ) |>
    filter(.data$ICAO %in% study_airports)

  write_parquet(
    harmonised,
    path(archive_root, "data", "asma", paste0("PBWG-BRA-ASMA-apdf-like-", year, ".parquet"))
  )

  harmonised
}

prepare_asma_reference_input <- function(asma, max_asma = 180) {
  asma |>
    mutate(
      DATE = as.Date(coalesce(MVT_TIME, BLOCK_TIME, CROSS_TIME)),
      RANGE_KNOWN = !is.na(RANGE),
      CLASS_KNOWN = !is.na(CLASS),
      RWY_KNOWN = !is.na(RWY),
      SECTOR_GROUP_KNOWN = !is.na(SECTOR_GROUP),
      VALID_ASMA = !is.na(ASMA_TIME) &
        ASMA_TIME > 0 &
        ASMA_TIME <= max_asma &
        RANGE_KNOWN &
        CLASS_KNOWN &
        RWY_KNOWN &
        SECTOR_GROUP_KNOWN
    )
}

build_asma_reference <- function(asma_samples, ref_year, variant, min_n = 5) {
  ref_start <- ymd_hms(paste0(ref_year, "-01-01 00:00:00"), tz = "UTC")
  ref_end <- ymd_hms(paste0(ref_year, "-12-31 23:59:59"), tz = "UTC")

  asma_samples |>
    filter(
      .data$MVT_TIME >= ref_start,
      .data$MVT_TIME <= ref_end,
      .data$VALID_ASMA
    ) |>
    summarise(
      N = n(),
      REF_ASMA = calc_reference_value(.data$ASMA_TIME, variant = variant),
      .by = c("ICAO", "PHASE", "RANGE", "CLASS", "RWY", "SECTOR_GROUP")
    ) |>
    mutate(
      REF_START = ref_start,
      REF_END = ref_end,
      REF_PERIOD = as.character(ref_year),
      REF_VARIANT = variant,
      MIN_N = min_n,
      IS_VALID_SAMPLE = .data$N >= min_n
    ) |>
    arrange(.data$ICAO, .data$PHASE, .data$RANGE, .data$CLASS, .data$RWY, .data$SECTOR_GROUP)
}

apply_asma_reference <- function(asma_samples, reference_data, valid_reference_only = TRUE) {
  reference_lookup <- reference_data

  if (valid_reference_only) {
    reference_lookup <- filter(reference_lookup, .data$IS_VALID_SAMPLE)
  }

  asma_samples |>
    left_join(
      select(
        reference_lookup,
        "ICAO", "PHASE", "RANGE", "CLASS", "RWY", "SECTOR_GROUP",
        "REF_ASMA", "REF_PERIOD", "REF_VARIANT", "MIN_N", "IS_VALID_SAMPLE"
      ),
      by = c("ICAO", "PHASE", "RANGE", "CLASS", "RWY", "SECTOR_GROUP")
    ) |>
    mutate(
      HAS_REFERENCE = !is.na(.data$REF_ASMA),
      ASMA_NA = !.data$VALID_ASMA | !.data$HAS_REFERENCE,
      ADD_ASMA = if_else(.data$VALID_ASMA & .data$HAS_REFERENCE, .data$ASMA_TIME - .data$REF_ASMA, NA_real_)
    )
}

summarise_asma_daily <- function(augmented_asma, year = NULL) {
  out <- augmented_asma |>
    mutate(METRIC_VALID = .data$VALID_ASMA & .data$HAS_REFERENCE) |>
    summarise(
      MVTS_VALID = sum(.data$METRIC_VALID, na.rm = TRUE),
      MVTS_NA = sum(!.data$METRIC_VALID, na.rm = TRUE),
      TOT_ASMA = sum(if_else(.data$METRIC_VALID, .data$ASMA_TIME, NA_real_), na.rm = TRUE),
      TOT_REF = sum(if_else(.data$METRIC_VALID, .data$REF_ASMA, NA_real_), na.rm = TRUE),
      TOT_ADD_TIME = sum(if_else(.data$METRIC_VALID, .data$ADD_ASMA, NA_real_), na.rm = TRUE),
      TOT_BRA_REF = sum(if_else(.data$METRIC_VALID, .data$BRA_REFERENCE_TIME, NA_real_), na.rm = TRUE),
      TOT_BRA_KPI08 = sum(if_else(.data$METRIC_VALID, .data$BRA_KPI08, NA_real_), na.rm = TRUE),
      .by = c("ICAO", "PHASE", "DATE", "RANGE", "CLASS", "RWY", "SECTOR_GROUP")
    ) |>
    arrange(.data$ICAO, .data$PHASE, .data$DATE, .data$RANGE, .data$CLASS, .data$RWY, .data$SECTOR_GROUP)

  if (is.null(year)) {
    return(out)
  }

  filter(out, lubridate::year(.data$DATE) == year)
}

build_asma_reference_filename <- function(airport, ref_period, variant, min_n, region = "BRA") {
  str_c("PBWG-", region, "-", airport, "-ref-asma-", ref_period, "-", variant, "-n", min_n, ".csv")
}

build_asma_daily_filename <- function(years, airport = NULL, ref_period, variant, region = "BRA") {
  year_label <- if (length(years) == 1) as.character(years) else str_c(min(years), max(years), sep = "-")
  pieces <- c("PBWG", region, airport, "asma-analytic", year_label, str_c("ref", ref_period), variant)
  pieces <- pieces[!is.na(pieces) & nzchar(pieces)]
  str_c(str_c(pieces, collapse = "-"), ".csv")
}

samples_by_year <- map(years, function(year) {
  read_bra_asma_year(year) |>
    prepare_asma_reference_input(max_asma = max_asma)
}) |>
  set_names(as.character(years))

asma_samples <- bind_rows(samples_by_year)

reference_data <- build_asma_reference(
  asma_samples = asma_samples,
  ref_year = ref_year,
  variant = variant,
  min_n = min_n
)

walk(unique(reference_data$ICAO), function(airport) {
  readr::write_csv(
    filter(reference_data, .data$ICAO == airport),
    path(
      archive_root,
      "outputs",
      "asma-reference-2024",
      build_asma_reference_filename(
        airport = airport,
        ref_period = ref_year,
        variant = variant,
        min_n = min_n,
        region = "BRA"
      )
    )
  )
})

daily_by_year <- imap(samples_by_year, function(samples, year_chr) {
  year <- as.integer(year_chr)
  message("Applying 2024 ASMA reference to ", year)

  daily <- samples |>
    apply_asma_reference(reference_data = reference_data, valid_reference_only = TRUE) |>
    summarise_asma_daily(year = year)

  daily_dir <- path(archive_root, "outputs", paste0("asma-daily-", year))

  walk(unique(daily$ICAO), function(airport) {
    write_csv(
      filter(daily, .data$ICAO == airport),
      path(
        daily_dir,
        build_asma_daily_filename(
          years = year,
          airport = airport,
          ref_period = ref_year,
          variant = variant,
          region = "BRA"
        )
      )
    )
  })

  write_csv(
    daily,
    path(
      daily_dir,
      build_asma_daily_filename(
        years = year,
        airport = NULL,
        ref_period = ref_year,
        variant = variant,
        region = "BRA"
      )
    )
  )

  daily
})

combined_daily <- bind_rows(daily_by_year)

write_csv(
  combined_daily,
  path(
    archive_root,
    "outputs",
    build_asma_daily_filename(
      years = years,
      airport = NULL,
      ref_period = ref_year,
      variant = variant,
      region = "BRA"
    )
  )
)

write_csv(
  combined_daily,
  path(
    report_data_dir,
    build_asma_daily_filename(
      years = years,
      airport = NULL,
      ref_period = ref_year,
      variant = variant,
      region = "BRA"
    )
  )
)

coverage_summary <- asma_samples |>
  summarise(
    N_TOTAL = n(),
    N_VALID_ASMA = sum(.data$VALID_ASMA, na.rm = TRUE),
    N_MISSING_RANGE = sum(!.data$RANGE_KNOWN, na.rm = TRUE),
    N_MISSING_CLASS = sum(!.data$CLASS_KNOWN, na.rm = TRUE),
    N_MISSING_RWY = sum(!.data$RWY_KNOWN, na.rm = TRUE),
    N_MISSING_SECTOR = sum(!.data$SECTOR_GROUP_KNOWN, na.rm = TRUE),
    PCT_VALID_ASMA = .data$N_VALID_ASMA / .data$N_TOTAL,
    .by = c("ICAO", "PHASE", "YEAR")
  ) |>
  arrange(.data$ICAO, .data$PHASE, .data$YEAR)

write_csv(coverage_summary, path(archive_root, "outputs", "BRA-asma-coverage-summary-2023-2025.csv"))
write_csv(coverage_summary, path(report_data_dir, "BRA-asma-coverage-summary-2023-2025.csv"))

reference_coverage <- reference_data |>
  summarise(
    N_REFERENCE_GROUPS = n(),
    N_VALID_REFERENCE_GROUPS = sum(.data$IS_VALID_SAMPLE, na.rm = TRUE),
    .by = c("ICAO", "PHASE", "RANGE")
  ) |>
  arrange(.data$ICAO, .data$PHASE, .data$RANGE)

write_csv(reference_coverage, path(archive_root, "outputs", "BRA-asma-reference-coverage-2024.csv"))
write_csv(reference_coverage, path(report_data_dir, "BRA-asma-reference-coverage-2024.csv"))

message("Wrote ASMA APDF-like parquet files, 2024 references, and daily analytic outputs.")
