bli_pli_stress_palette <- c(
  "0.0-0.2" = "#D7ECFA",
  "0.2-0.4" = "#D8F0D2",
  "0.4-0.6" = "#79C36A",
  "0.6-0.8" = "#F0A35E",
  ">0.8" = "#D95F5F",
  "missing" = "#E2E2E2"
)

bli_pli_stress_band <- function(.value) {
  bands <- cut(
    .value,
    breaks = c(-Inf, 0.2, 0.4, 0.6, 0.8, Inf),
    labels = c("0.0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", ">0.8"),
    right = TRUE
  )

  factor(
    dplyr::if_else(is.na(.value), "missing", as.character(bands)),
    levels = names(bli_pli_stress_palette)
  )
}

bli_pli_compact_heatmap_plot <- function(.bli_pli_df, .years = sort(unique(.bli_pli_df$YEAR))) {
  heatmap_data <- .bli_pli_df |>
    dplyr::select(REG, ICAO, YEAR, BLI, PLI) |>
    tidyr::pivot_longer(cols = c(BLI, PLI), names_to = "INDEX", values_to = "VALUE") |>
    dplyr::mutate(
      INDEX = factor(INDEX, levels = c("BLI", "PLI")),
      AIRPORT_INDEX = factor(
        paste(ICAO, INDEX),
        levels = unlist(lapply(sort(unique(ICAO)), function(.apt) paste(.apt, c("BLI", "PLI"))))
      ),
      YEAR_FCT = factor(YEAR, levels = .years),
      STRESS = bli_pli_stress_band(VALUE)
    )

  row_annotations <- heatmap_data |>
    dplyr::distinct(REG, ICAO, AIRPORT_INDEX, INDEX) |>
    dplyr::mutate(
      YEAR_FCT = factor(min(.years), levels = .years),
      ANNO_X = 0.62,
      ANNO_LABEL = as.character(INDEX)
    )

  heatmap_data |>
    ggplot2::ggplot(ggplot2::aes(x = YEAR_FCT, y = AIRPORT_INDEX, fill = STRESS)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::geom_text(
      data = row_annotations,
      ggplot2::aes(x = ANNO_X, y = AIRPORT_INDEX, label = ANNO_LABEL),
      inherit.aes = FALSE,
      hjust = 0,
      size = 2.7,
      color = "grey15"
    ) +
    ggplot2::facet_wrap(. ~ REG, scales = "free_y", ncol = 2) +
    ggplot2::scale_fill_manual(values = bli_pli_stress_palette, drop = FALSE) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
    ggplot2::scale_y_discrete(
      labels = function(x) {
        parts <- strsplit(x, " ")
        vapply(
          parts,
          function(.parts) {
            if (.parts[2] == "PLI") .parts[1] else ""
          },
          character(1)
        )
      }
    ) +
    ggplot2::labs(x = NULL, y = NULL, fill = "index range") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 8),
      axis.text.y = ggplot2::element_text(size = 7, lineheight = 1.0),
      panel.grid = ggplot2::element_blank(),
      legend.position = "top",
      panel.spacing.y = grid::unit(0.8, "cm"),
      plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 28)
    )
}

bli_pli_panel_heatmap_plot <- function(.bli_pli_df, .years = sort(unique(.bli_pli_df$YEAR))) {
  heatmap_data <- .bli_pli_df |>
    dplyr::select(REG, ICAO, YEAR, BLI, PLI) |>
    tidyr::pivot_longer(cols = c(BLI, PLI), names_to = "INDEX", values_to = "VALUE") |>
    dplyr::mutate(
      REG = factor(REG, levels = c("BRA", "EUR")),
      INDEX = factor(
        INDEX,
        levels = c("BLI", "PLI"),
        labels = c("Base load index (BLI)", "Peak load index (PLI)")
      ),
      ICAO = factor(ICAO, levels = sort(unique(ICAO))),
      YEAR_FCT = factor(YEAR, levels = .years),
      STRESS = bli_pli_stress_band(VALUE)
    )

  heatmap_data |>
    ggplot2::ggplot(ggplot2::aes(x = YEAR_FCT, y = ICAO, fill = STRESS)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::facet_wrap(INDEX ~ REG, ncol = 2, scales = "free_y") +
    ggplot2::scale_fill_manual(values = bli_pli_stress_palette, drop = FALSE) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
    ggplot2::labs(x = NULL, y = NULL, fill = "index range") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 8),
      axis.text.y = ggplot2::element_text(size = 7, lineheight = 1.0),
      panel.grid = ggplot2::element_blank(),
      legend.position = "top",
      panel.spacing = grid::unit(0.65, "cm"),
      strip.text = ggplot2::element_text(size = 8.5, face = "bold")
    )
}
