airports_brazil <- data.frame(
  icao    = c("SBBR","SBGR","SBSP","SBKP","SBRJ","SBGL","SBCF","SBSV","SBPA","SBCT","SBRF","SBEG"),
  name    = c("Brasília","Guarulhos","Congonhas","Campinas","Santos Dumont","Galeão","Belo Horizonte","Salvador","Porto Alegre","Curitiba","Recife","Eduardo Gomes"),
  lon     = c(-47.92,-46.47,-46.66,-47.13,-43.17,-43.35,-43.97,-38.33,-51.17,-49.17,-34.92,-60.05),
  lat     = c(-15.87,-23.43,-23.63,-23.01,-22.91,-22.75,-19.62,-12.91,-29.99,-25.53, -8.13, -3.04),
  lbl_lon = c(-58.0,-57.0,-40.0,-57.0,-37.0,-35.0,-38.0,-45.0,-65.0,-62.0,-40.0,-70.0),
  lbl_lat = c(-13.0,-29.0,-29.5,-19.0,-19.0,-26.0,-15.0,-10.0,-31.0,-29.0, -6.0,  0.0)
)

airports_europe <- data.frame(
  icao    = c("EHAM","LFPG","EGLL","EDDF","EDDM","LEMD","LPPT","LEBL","EGKK","LSZH","LTFM","LGAV"),
  name    = c("Amsterdam","Paris","Heathrow","Frankfurt","Munich","Madrid","Lisbon","Barcelona","Gatwick","Zurich","Istanbul","Athens"),
  lon     = c(  4.76,  2.55, -0.46,  8.57, 11.79, -3.57, -9.13,  2.07, -0.19,  8.55, 28.75, 23.94),
  lat     = c( 52.31, 49.01, 51.48, 50.03, 48.35, 40.47, 38.77, 41.30, 51.15, 47.46, 41.27, 37.94),
  lbl_lon = c(  9.0, -5.0, -8.0, 15.0, 17.0, -11.0, -13.0, -5.0,  -7.0,  14.0,  33.0,  19.0),
  lbl_lat = c( 54.5, 47.0, 53.5, 52.5, 50.5,  38.0,  36.5, 39.0,  49.5,  45.5,  43.5,  35.5)
)

map_brazil <- ggplot() +
  geom_sf(data = brazil_map, fill = "gray90", color = "white") +
  coord_sf(xlim = c(-74, -28), ylim = c(-34, 6), expand = FALSE) +
  geom_segment(data = airports_brazil,
               aes(x = lbl_lon, y = lbl_lat, xend = lon, yend = lat),
               color = "gray50", linewidth = 0.3) +
  geom_point(data = airports_brazil,
             aes(x = lon, y = lat), color = "#2E6B3E", size = 2) +
  geom_label(data = airports_brazil,
             aes(x = lbl_lon, y = lbl_lat, label = paste0(icao, "\n", name)),
             size = 4, lineheight = 1.2, fill = "white",
             label.padding = unit(0.35, "lines"), label.size = 0.25) +
  labs(subtitle = "Brazil") +
  theme_void() +
  theme(
    plot.margin      = margin(10, 10, 10, 10),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

map_europe <- ggplot() +
  geom_sf(data = europe_map, fill = "gray90", color = "white") +
  coord_sf(xlim = c(-15, 38), ylim = c(34, 60), expand = FALSE) +
  geom_segment(data = airports_europe,
               aes(x = lbl_lon, y = lbl_lat, xend = lon, yend = lat),
               color = "gray50", linewidth = 0.3) +
  geom_point(data = airports_europe,
             aes(x = lon, y = lat), color = "#4472C4", size = 2) +
  geom_label(data = airports_europe,
             aes(x = lbl_lon, y = lbl_lat, label = paste0(icao, "\n", name)),
             size = 4, lineheight = 1.2, fill = "white",
             label.padding = unit(0.35, "lines"), label.size = 0.25) +
  labs(subtitle = "Europe") +
  theme_void() +
  theme(
    plot.margin      = margin(10, 10, 10, 10),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

combined_plot <- map_brazil + map_europe +
  patchwork::plot_layout(widths = c(1.5, 2)) +
  patchwork::plot_annotation(
    caption = "Charts for both regions not in scale",
    theme   = theme(plot.caption = element_text(size = 8, hjust = 1, color = "#555555"))
  )

combined_plot

ggplot2::ggsave(
  filename = here::here("figures", "Rplot1.png"),
  plot     = combined_plot,
  width    = 16, height = 7, dpi = 100,
  units    = "in", bg = "white"
)