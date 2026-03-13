# --- Read original CSV (unchanged) ---
airports_raw <- readr::read_csv(here::here("data", "study_airports_BRA_EUR.csv"),
                                show_col_types = FALSE)

# --- Extract ICAO codes from "Name (ICAO)" format ---
icao_brazil <- stringr::str_extract(airports_raw$Brazil,  "(?<=\\()\\w+(?=\\))")
icao_europe <- stringr::str_extract(airports_raw$Europe, "(?<=\\()\\w+(?=\\))")

name_brazil <- stringr::str_remove(airports_raw$Brazil,  "\\s*\\(\\w+\\)")
name_europe <- stringr::str_remove(airports_raw$Europe, "\\s*\\(\\w+\\)")

# --- Coordinates lookup (add more rows here if new airports are added) ---
coords <- data.frame(
  icao = c("SBBR","SBGR","SBSP","SBKP","SBRJ","SBGL","SBCF","SBSV","SBPA","SBCT","SBRF","SBEG",
           "EHAM","LFPG","EGLL","EDDF","EDDM","LEMD","LPPT","LEBL","EGKK","LSZH","LTFM","LGAV"),
  lon  = c(-47.92,-46.47,-46.66,-47.13,-43.17,-43.25,-43.97,-38.33,-51.17,-49.17,-34.92,-60.05,
           4.76,  2.55, -0.46,  8.57, 11.79, -3.57, -9.13,  2.07, -0.19,  8.55, 28.75, 23.94),
  lat  = c(-15.87,-23.43,-23.63,-23.01,-22.91,-22.81,-19.62,-12.91,-29.99,-25.53, -8.13, -3.04,
           52.31, 49.01, 51.48, 50.03, 48.35, 40.47, 38.77, 41.30, 51.15, 47.46, 41.27, 37.94)
)

# --- Build airport dataframes ---
airports_brazil <- data.frame(icao = icao_brazil, name = name_brazil) |>
  dplyr::left_join(coords, by = "icao")

airports_europe <- data.frame(icao = icao_europe, name = name_europe) |>
  dplyr::left_join(coords, by = "icao")

# --- Base maps ---
world      <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
brazil_map <- world |> dplyr::filter(admin == "Brazil")
europe_map <- world |> dplyr::filter(continent == "Europe" | admin == "Turkey")

# --- Reusable plot function ---
make_airport_map <- function(base_map, airports_df, xlim, ylim, title) {
  
  ggplot2::ggplot() +
    
    ggplot2::geom_sf(
      data      = base_map,
      fill      = "#E8E8E8",
      color     = "#AAAAAA",
      linewidth = 0.3
    ) +
    
    ggplot2::geom_point(
      data = airports_df,
      ggplot2::aes(x = lon, y = lat),
      color = "#2E6B3E", size = 1.5
    ) +
    
    ggrepel::geom_label_repel(
      data = airports_df,
      ggplot2::aes(x = lon, y = lat, label = paste0(icao, "\n", name)),
      size          = 2.5,
      color         = "#1A1A1A",
      fill          = "white",
      label.size    = 0.25,
      label.r       = grid::unit(0.15, "lines"),
      box.padding   = 0.4,
      point.padding = 0.3,
      max.overlaps  = 20,
      lineheight    = 0.85,
      segment.color     = "#555555",
      segment.size      = 0.3,
      min.segment.length = 0
    ) +
    
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    
    ggplot2::theme_void(base_size = 10) +
    
    ggplot2::theme(
      plot.title       = ggplot2::element_text(size = 11, hjust = 0.05,
                                               margin = ggplot2::margin(b = 4)),
      panel.background = ggplot2::element_rect(fill = "#F5F8FA", color = NA),
      plot.background  = ggplot2::element_rect(fill = "white",   color = NA),
      plot.margin      = ggplot2::margin(5, 5, 5, 5)
    )
}

# --- Build panels ---
map_brazil <- make_airport_map(
  base_map    = brazil_map,
  airports_df = airports_brazil,
  xlim        = c(-74, -34),
  ylim        = c(-34,   6),
  title       = "Brazil"
)

map_europe <- make_airport_map(
  base_map    = europe_map,
  airports_df = airports_europe,
  xlim        = c(-12, 38),
  ylim        = c( 35, 55),
  title       = "Europe"
)

# --- Combine ---
patchwork::wrap_plots(map_brazil, map_europe, ncol = 2) +
  patchwork::plot_annotation(
    caption = "Charts for both regions not in scale",
    theme   = ggplot2::theme(
      plot.caption = ggplot2::element_text(size = 8, hjust = 1, color = "#555555")
    )
  )