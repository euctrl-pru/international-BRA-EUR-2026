
world      <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
brazil_map <- world |> dplyr::filter(admin == "Brazil")
europe_map <- world |> dplyr::filter(continent == "Europe" | admin == "Turkey")

airports_brazil <- data.frame(
  icao    = c("SBBR","SBGR","SBSP","SBKP","SBRJ","SBGL","SBCF","SBSV","SBPA","SBCT","SBRF","SBEG"),
  name    = c("Brasília","Guarulhos","Congonhas","Campinas","Santos Dumont","Galeão","Belo Horizonte","Salvador","Porto Alegre","Curitiba","Recife","Eduardo Gomes"),
  lon     = c(-47.92,-46.47,-46.66,-47.13,-43.17,-43.35,-43.97,-38.33,-51.17,-49.17,-34.92,-60.05),
  lat     = c(-15.87,-23.43,-23.63,-23.01,-22.91,-22.75,-19.62,-12.91,-29.99,-25.53, -8.13, -3.04),
  lbl_lon = c(-58.0,-57.0,-43.0,-57.0,-36.0,-35.0,-38.0,-45.0,-43.0,-62.0,-40.0,-60.0),
  lbl_lat = c(-13.0,-24.0,-27.5,-19.0,-21.0,-26.0,-16.0,-10.0,-32.0,-29.0, -3.0,  0.0)
)

airports_europe <- data.frame(
  icao    = c("EHAM","LFPG","EGLL","EDDF","EDDM","LEMD","LPPT","LEBL","EGKK","LSZH","LTFM","LGAV"),
  name    = c("Amsterdam","Paris","Heathrow","Frankfurt","Munich","Madrid","Lisbon","Barcelona","Gatwick","Zurich","Istanbul","Athens"),
  lon     = c(  4.76,  2.55, -0.46,  8.57, 11.79, -3.57, -9.13,  2.07, -0.19,  8.55, 28.75, 23.94),
  lat     = c( 52.31, 49.01, 51.48, 50.03, 48.35, 40.47, 38.77, 41.30, 51.15, 47.46, 41.27, 37.94),
  lbl_lon = c(  6.0, -3.0, -8.0, 15.0, 17.0, -2.0, -11.0, 9.0,  -7.0,  14.0,  33.0,  30.0),
  lbl_lat = c( 57.5, 45.0, 53.5, 54.5, 50.5,  38.0,  43.5, 39.0,  49.5,  45.5,  43.5,  37.5)
)

map_brazil <- ggplot() +
  geom_sf(data = brazil_map, fill = "gray90", color = "white") +
  coord_sf(xlim = c(-74, -30), ylim = c(-34, 5), expand = FALSE) +
  geom_segment(data = airports_brazil,
               aes(x = lbl_lon, y = lbl_lat, xend = lon, yend = lat),
               color = "gray50", linewidth = 0.3) +
  geom_point(data = airports_brazil,
             aes(x = lon, y = lat), color = "#2E6B3E", size = 4) +
  geom_label(data = airports_brazil,
             aes(x = lbl_lon, y = lbl_lat, label = paste0(icao, "\n", name)),
             size = 5.7, lineheight = 0.8, fill = "white",
             label.padding = unit(0.35, "lines"), label.size = 1) +
  labs(subtitle = "Brazil") +
  theme_void() +
  theme(
    plot.subtitle    = element_text(size = 24, hjust = 0.5 , margin = margin(b = 4)),
    plot.margin      = margin(10, 10, 10, 10),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

map_europe <- ggplot() +
  geom_sf(data = europe_map, fill = "gray90", color = "white") +
  coord_sf(xlim = c(-15, 37), ylim = c(35, 67), expand = FALSE) +
  geom_segment(data = airports_europe,
               aes(x = lbl_lon, y = lbl_lat, xend = lon, yend = lat),
               color = "gray50", linewidth = 0.3) +
  geom_point(data = airports_europe,
             aes(x = lon, y = lat), color = "#4472C4", size = 4) +
  geom_label(data = airports_europe,
             aes(x = lbl_lon, y = lbl_lat, label = paste0(icao, "\n", name)),
             size = 5.7, lineheight = 0.8, fill = "white",
             label.padding = unit(0.35, "lines"), label.size = 1) +
  labs(subtitle = "Europe") +
  theme_void() +
  theme(
    plot.subtitle    = element_text(size = 24, hjust = 0.5 , margin = margin(b = 4)),
    plot.margin      = margin(10, 10, 10, 10),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

combined_plot <- map_brazil + map_europe +
  patchwork::plot_layout(widths = c(5, 5)) +
  patchwork::plot_annotation(
    caption = "Charts for both regions not in scale",
    theme   = theme(plot.caption = element_text(size = 20, hjust = 1, color = "#555555"))
  )

combined_plot

ggplot2::ggsave(
  filename = here::here("figures", "scope-airports-2025.png"),
  plot     = combined_plot,
  width    = 16, height = 8, dpi = 100,
  units    = "in", bg = "white"
)