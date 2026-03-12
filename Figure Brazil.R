# --- Load official FIR boundaries from DECEA WFS ---
fir_url <- "https://geoaisweb.decea.mil.br/geoserver/ICA/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=ICA%3Afir"

fir_sf <- sf::st_read(fir_url, quiet = TRUE) |>
  sf::st_set_crs(4326) |>
  dplyr::select(name = nam, geom)

# --- Brazil border from geobr (for outline) ---
brazil <- geobr::read_country(year = 2020, showProgress = FALSE)

# Manual label positions
fir_labels <- data.frame(
  name  = c("Amazônica", "Recife", "Brasília", "Curitiba", "Atlântico"),
  lon   = c(-62.0,        -40.0,     -48.0,      -52.0,      -20.0),
  lat   = c( -3.0,         -8.0,     -15.0,      -27.0,      -15.0),
  label = c("FIR\nAmazônica", "FIR\nRecife", "FIR\nBrasília", "FIR\nCuritiba", "FIR\nAtlântico")
)

# Europa-style soft palette
fir_colors <- c(
  "Amazônica"  = "#A9DFBF",
  "Recife"     = "#F5CBA7",
  "Brasília"   = "#F9E79F",
  "Curitiba"   = "#AED6F1",
  "Atlântico"  = "#D6EAF8"
)

# --- Plot ---
ggplot2::ggplot() +
  
  # FIR polygons
  ggplot2::geom_sf(
    data      = fir_sf,
    ggplot2::aes(fill = name),
    color     = "#2C3E50",
    linewidth = 0.5
  ) +
  
  # Brazil border on top of FIRs
  ggplot2::geom_sf(
    data      = brazil,
    fill      = NA,
    color     = "#2C3E50",
    linewidth = 0.8
  ) +
  
  # FIR labels
  ggplot2::geom_text(
    data     = fir_labels,
    ggplot2::aes(x = lon, y = lat, label = label),
    size     = 3.2,
    fontface = "bold",
    color    = "#2C3E50",
    lineheight = 0.85
  ) +
  
  ggplot2::scale_fill_manual(values = fir_colors, na.value = "#EAECEE") +
  
  ggplot2::coord_sf(
    xlim   = c(-75, -5),
    ylim   = c(-36, 8),
    expand = FALSE
  ) +
  
  ggplot2::labs(x = NULL, y = NULL) +
  
  ggplot2::theme_void(base_size = 11) +
  
  ggplot2::theme(
    legend.position  = "none",
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.border     = ggplot2::element_rect(fill = NA, color = "#2C3E50", linewidth = 0.6),
    plot.background  = ggplot2::element_rect(fill = "white", color = NA),
    plot.margin      = ggplot2::margin(10, 10, 10, 10)
  )