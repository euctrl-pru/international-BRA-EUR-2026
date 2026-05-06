source(here::here("_chapter-setup.R"))

#01 ___ATCO graphic with index_____________________________________________________


# ── ATCOs in OPS ──────────────────────────────────────────────────────────────
atco <- table_bra_eur |>
  dplyr::filter(KPA == "number of ATCOs in OPS") |>
  dplyr::select(KPA, Brazil_2023, Brazil_2024, Brazil_2025,
                Europe_2023, Europe_2024, Europe_2025) |>
  dplyr::mutate(
    dplyr::across(Brazil_2023:Europe_2025,
                  ~ suppressWarnings(as.numeric(.x)))
  ) |>
  tidyr::pivot_longer(
    cols      = -KPA,
    names_to  = c("REGION", "YEAR"),
    names_sep = "_",
    values_to = "ATCO"
  ) |>
  dplyr::mutate(
    YEAR   = as.integer(YEAR),
    REGION = stringr::str_to_title(REGION)
  ) |>
  dplyr::filter(!is.na(ATCO))

# ── CONTROLLED FLIGHTS — traffic index base 2023 = 100 ───────────────────────
index <- table_bra_eur |>
  dplyr::filter(KPA == "controlled flights") |>
  dplyr::select(KPA, Brazil_2023, Brazil_2024, Brazil_2025,
                Europe_2023, Europe_2024, Europe_2025) |>
  dplyr::mutate(
    dplyr::across(Brazil_2023:Europe_2025,
                  ~ suppressWarnings(as.numeric(.x)))
  ) |>
  tidyr::pivot_longer(
    cols      = -KPA,
    names_to  = c("REGION", "YEAR"),
    names_sep = "_",
    values_to = "FLIGHTS"
  ) |>
  dplyr::mutate(
    YEAR   = as.integer(YEAR),
    REGION = stringr::str_to_title(REGION)
  ) |>
  dplyr::filter(!is.na(FLIGHTS)) |>
  dplyr::group_by(REGION) |>
  dplyr::mutate(
    BASE  = dplyr::first(FLIGHTS),
    INDEX = FLIGHTS / BASE * 100
  ) |>
  dplyr::ungroup() |>
  dplyr::select(REGION, YEAR, INDEX)

# ── JOIN ──────────────────────────────────────────────────────────────────────
df <- atco |>
  dplyr::left_join(index, by = c("REGION", "YEAR")) |>
  dplyr::mutate(YEAR = factor(YEAR))

# ── PLOT FUNCTION ─────────────────────────────────────────────────────────────
plot_atco_panel <- function(data, region_name, bar_color) {
  
  df_reg    <- data |> dplyr::filter(REGION == region_name)
  scale_fac <- max(df_reg$ATCO, na.rm = TRUE) /
    max(df_reg$INDEX, na.rm = TRUE)
  
  ggplot2::ggplot(df_reg, ggplot2::aes(x = YEAR)) +
    
    ggplot2::geom_col(
      ggplot2::aes(y = ATCO),
      fill  = bar_color,
      alpha = 0.85,
      width = 0.5
    ) +
    
    ggplot2::geom_text(
      ggplot2::aes(y = ATCO, label = scales::comma(ATCO)),
      vjust  = -0.4,
      size   = 2.8,
      colour = "#444444"
    ) +
    
    ggplot2::geom_line(
      ggplot2::aes(y = INDEX * scale_fac, group = 1),
      colour    = bar_color,
      linewidth = 0.8,
      linetype  = "dashed"
    ) +
    
    ggplot2::geom_point(
      ggplot2::aes(y = INDEX * scale_fac),
      colour = bar_color,
      size   = 2.5,
      shape  = 21,
      fill   = "white",
      stroke = 1.2
    ) +
    
    ggplot2::scale_y_continuous(
      labels   = scales::comma,
      expand   = ggplot2::expansion(mult = c(0, 0.12)),
      sec.axis = ggplot2::sec_axis(
        ~ . / scale_fac,
        name   = "Traffic index (base 2023 = 100)",
        labels = scales::number_format(accuracy = 1)
      )
    ) +
    
    ggplot2::labs(
      title = region_name,
      x     = NULL,
      y     = "ATCOs in operations"
    ) +
    
    bra_eur_theme_minimal
}

# ── BUILD & SAVE ──────────────────────────────────────────────────────────────
p_bra <- plot_atco_panel(df, "Brazil", bra_col)
p_eur <- plot_atco_panel(df, "Europe", eur_col)

p_fig22 <- (p_bra / p_eur) 

ggplot2::ggsave(
  filename = here::here("figures", "02_system_ATCO_new_comparison.png"),
  plot     = p_fig22,
  width    = 15,
  height   = 14,
  units    = "cm",
  dpi      = 300
)







#02 # ── FLIGHTS PER ATCO ──────────────────────────────────────────────────────────

# hlc_raw <- readr::read_csv2(
#   here::here("data", "table_BRA_EUR.csv"),
#   col_types = readr::cols(.default = readr::col_character())
# )


flights_atco <- table_bra_eur |>
  dplyr::filter(KPA == "flights ATCO") |>
  dplyr::select(KPA, Brazil_2023, Brazil_2024, Brazil_2025,
                Europe_2023, Europe_2024, Europe_2025) |>
  dplyr::mutate(
    dplyr::across(Brazil_2023:Europe_2025,
                  ~ suppressWarnings(as.numeric(.x)))
  ) |>
  tidyr::pivot_longer(
    cols      = -KPA,
    names_to  = c("REGION", "YEAR"),
    names_sep = "_",
    values_to = "FLT_ATCO"
  ) |>
  dplyr::mutate(
    YEAR   = factor(as.integer(YEAR)),
    REGION = stringr::str_to_title(REGION)
  ) |>
  dplyr::filter(!is.na(FLT_ATCO))

# ── PLOT FUNCTION ─────────────────────────────────────────────────────────────
plot_flt_atco <- function(data,
                          y_max   = 750,
                          y_break = 150) {
  
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = YEAR, y = FLT_ATCO, fill = REGION)
  ) +
    
    # MUDAR AQUI: position = "dodge" agrupa as barras por ano
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.6),
      alpha    = 0.85,
      width    = 0.5
    ) +
    
    ggplot2::geom_text(
      ggplot2::aes(label = FLT_ATCO),
      position = ggplot2::position_dodge(width = 0.6),
      vjust    = -0.4,
      size     = 2.8,
      colour   = "#444444"
    ) +
    
    ggplot2::scale_fill_manual(
      values = c("Brazil" = bra_col, "Europe" = eur_col),
      name   = NULL
    ) +
    
    ggplot2::scale_y_continuous(
      limits = c(0, y_max),
      expand = ggplot2::expansion(mult = c(0, 0.05)),
      breaks = seq(0, y_max, y_break)
    ) +
    
    ggplot2::labs(
      x       = NULL,
      y       = "Flights per ATCO in operations"
      ) +
    bra_eur_theme_minimal
}

# ── BUILD ─────────────────────────────────────────────────────────────────────
p_fig23 <- plot_flt_atco(
  data    = flights_atco,
  y_max   = 750,  
  y_break = 150  
)

# ── BUILD & SAVE ──────────────────────────────────────────────────────────────
ggplot2::ggsave(
  filename = here::here("figures", "02_system_ATCO_new_ratio.png"),
  plot     = p_fig23,
  width    = 15,
  height   = 7,
  units    = "cm",
  dpi      = 300
)



