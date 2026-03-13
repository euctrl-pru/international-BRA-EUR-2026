# --- Cache paths ---
fir_path   <- here::here("data", "fir_brazil.gpkg")
juris_path <- here::here("data", "aga_jurisdicao.gpkg")

# --- Download once ---
if (!file.exists(fir_path)) {
  fir_url <- "https://geoaisweb.decea.mil.br/geoserver/ICA/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=ICA%3Afir"
  sf::st_read(fir_url, quiet = TRUE) |>
    sf::st_set_crs(4326) |>
    dplyr::select(name = nam, geom) |>
    sf::st_write(fir_path, quiet = TRUE)
}

if (!file.exists(juris_path)) {
  juris_url <- "https://geoaisweb.decea.mil.br/geoserver/ICA/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=ICA%3Aaga_jurisdicao"
  sf::st_read(juris_url, quiet = TRUE) |>
    sf::st_set_crs(4326) |>
    dplyr::select(name, snippet, geom) |>
    sf::st_write(juris_path, quiet = TRUE)
}

fir_sf   <- sf::st_read(fir_path,   quiet = TRUE) |> sf::st_make_valid()
juris_sf <- sf::st_read(juris_path, quiet = TRUE) |> sf::st_make_valid()

# Explode, keep only POLYGON type (removes stray points/lines), drop tiny islands
fir_sf <- fir_sf |>
  sf::st_cast("MULTIPOLYGON") |>
  sf::st_cast("POLYGON") |>
  dplyr::filter(
    sf::st_geometry_type(geom) %in% c("POLYGON", "MULTIPOLYGON")
  ) |>
  sf::st_transform(3857) |>
  dplyr::filter(as.numeric(sf::st_area(geom)) > 5e8) |>  # > 500 km²
  sf::st_transform(4326) |>
  dplyr::group_by(name) |>
  dplyr::summarise(geom = sf::st_union(geom), .groups = "drop")

juris_sf <- juris_sf |>
  sf::st_cast("MULTIPOLYGON") |>
  sf::st_cast("POLYGON") |>
  dplyr::filter(
    sf::st_geometry_type(geom) %in% c("POLYGON", "MULTIPOLYGON")
  ) |>
  sf::st_transform(3857) |>
  dplyr::filter(as.numeric(sf::st_area(geom)) > 5e8) |>  # > 500 km²
  sf::st_transform(4326) |>
  dplyr::group_by(name, snippet) |>
  dplyr::summarise(geom = sf::st_union(geom), .groups = "drop")

sbxp_sf  <- juris_sf |> dplyr::filter(snippet == "SBXP")

# --- Brazil border (remove tiny islands) ---
brazil <- geobr::read_country(year = 2020, showProgress = FALSE) |>
  sf::st_cast("POLYGON") |>
  sf::st_transform(3857) |>
  dplyr::filter(as.numeric(sf::st_area(geom)) > 1e9) |>  # > 1000 km²
  sf::st_transform(4326) |>
  dplyr::summarise(geom = sf::st_union(geom))

# --- FIR label positions ---
fir_labels <- data.frame(
  lon   = c(-59.0, -40.0, -48.0, -51.2, -25.0),
  lat   = c( -5.0,  -8.0, -16.0, -24.0, -15.0),
  label = c("FIR\nAmazônica", "FIR\nRecife", "FIR\nBrasília", "FIR\nCuritiba", "FIR\nAtlântico")
)

# --- TMA SP/RJ annotation: label to the right in blue ocean area ---
tma_label <- data.frame(
  x     = -35.0, y    = -27.0,   # label position (blue area to the right)
  xend  = -42.5, yend = -23.5    # arrow tip (inside SBXP)
)

# --- FIR palette ---
fir_colors <- c(
  "Amazônica"  = "#A9DFBF",   # verde
  "Recife"     = "#F5CBA7",   # laranja (era Curitiba)
  "Brasília"   = "#F9E79F",   # amarelo (inalterado)
  "Curitiba"   = "#AED6F1",   # azul (era Amazônica)
  "Atlântico"  = "#D6EAF8"    # azul claro (inalterado)
)

# --- Legend data ---
legend_df <- data.frame(
  label    = c("FIR Brasília", "FIR Curitiba", "FIR Recife",
               "FIR Atlântico", "FIR Amazônica", "TMA SP/RJ"),
  sublabel = c("CINDACTA I", "CINDACTA II", "CINDACTA III",
               "CINDACTA III", "CINDACTA IV", "CRCEA-SE"),
  color    = c("#F9E79F", "#AED6F1", "#F5CBA7",
               "#D6EAF8", "#A9DFBF", "#F1948A"),
  border   = c("#2C3E50", "#2C3E50", "#2C3E50",
               "#2C3E50", "#2C3E50", "#C0392B"),
  stringsAsFactors = FALSE
)

# --- Map ---
map_plot <- ggplot2::ggplot() +
  
  ggplot2::geom_sf(
    data = fir_sf,
    ggplot2::aes(fill = name),
    color = "#2C3E50", linewidth = 0.5
  ) +
  
  ggplot2::geom_sf(
    data = juris_sf |> dplyr::filter(snippet != "SBXP"),
    fill = NA, color = "#2C3E50", linewidth = 0.5
  ) +
  
  ggplot2::geom_sf(
    data = sbxp_sf,
    fill = "#F1948A", color = "#C0392B", linewidth = 0.6
  ) +
  
  ggplot2::geom_sf(
    data = brazil,
    fill = NA, color = "#2C3E50", linewidth = 0.8
  ) +
  
  # FIR labels
  ggplot2::geom_text(
    data = fir_labels,
    ggplot2::aes(x = lon, y = lat, label = label),
    size = 3.0, fontface = "bold", color = "#2C3E50",
    lineheight = 0.85, family = "sans"
  ) +
  
  # Arrow from label to SBXP polygon
  ggplot2::annotate(
    "segment",
    x = tma_label$x - 2.5, y = tma_label$y,
    xend = tma_label$xend, yend = tma_label$yend,
    color = "#C0392B", linewidth = 0.4,
    arrow = grid::arrow(length = grid::unit(0.15, "cm"), type = "closed")
  ) +
  
  # TMA SP/RJ label to the right
  ggplot2::annotate(
    "text",
    x = tma_label$x, y = tma_label$y,
    label = "TMA SP/RJ",
    size = 2.5, fontface = "bold", color = "#C0392B",
    hjust = 0, family = "sans"
  ) +
  
  ggplot2::scale_fill_manual(values = fir_colors, na.value = "#EAECEE") +
  
  # Restore full extent to keep FIR Atlântico visible
  ggplot2::coord_sf(xlim = c(-75, -5), ylim = c(-36, 8), expand = FALSE) +
  
  ggplot2::labs(x = NULL, y = NULL) +
  
  ggplot2::theme_void(base_size = 11) +
  
  ggplot2::theme(
    legend.position  = "none",
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.border     = ggplot2::element_blank(),
    plot.background  = ggplot2::element_rect(fill = "white", color = NA),
    plot.margin      = ggplot2::margin(5, 2, 5, 5)
  )

# --- Legend (compact) ---
n     <- nrow(legend_df)
y_pos <- -seq_len(n)

legend_plot <- ggplot2::ggplot(legend_df) +
  
  ggplot2::geom_tile(
    ggplot2::aes(x = 0.08, y = y_pos, width = 0.13, height = 0.40),
    fill = legend_df$color, color = legend_df$border, linewidth = 0.35
  ) +
  
  ggplot2::geom_text(
    ggplot2::aes(x = 0.18, y = y_pos + 0.10, label = label),
    hjust = 0, size = 2.5, fontface = "bold", color = "#2C3E50", family = "sans"
  ) +
  
  ggplot2::geom_text(
    ggplot2::aes(x = 0.18, y = y_pos - 0.14, label = sublabel),
    hjust = 0, size = 2.1, color = "#888888", family = "sans"
  ) +
  
  ggplot2::xlim(0, 1.0) +
  ggplot2::ylim(-n - 0.4, -0.5) +
  
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    plot.margin     = ggplot2::margin(5, 10, 5, 0)
  )

# --- Combine ---
patchwork::wrap_plots(map_plot, legend_plot, widths = c(3.8, 1))