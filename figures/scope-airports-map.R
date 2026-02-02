#' script to plot scope map
#' 
#' 

# load airport
source("_chapter-setup.R")

# get airport LAT/LON --------------------------------------------------------
# our_airports <- readr::read_csv(
#   "https://davidmegginson.github.io/ourairports-data/airports.csv"
#   , show_col_types = FALSE)
# 
# this_airports <- our_airports |> 
#   dplyr::filter(ident %in% c(bra_apts, eur_apts)) |> 
#   dplyr::select(ICAO = ident, LAT = latitude_deg, LON = longitude_deg) |> 
#   dplyr::inner_join( dplyr::bind_rows(bra_apts_names, eur_apts_names)
#                     ,dplyr::join_by(ICAO))
# 
# readr::write_csv(this_airports, "./data/airport-LAT-LON-NAME.csv")

# read-in look-up
this_airports <- readr::read_csv(here::here("data", "airport-LAT-LON-NAME.csv"), show_col_types = FALSE)
this_airports <- this_airports |> dplyr::filter(ICAO %in% c(bra_apts, eur_apts))

worldmap <- ggplot2::borders("world2", colour="lightblue", fill="lightblue")
ggplot2::ggplot() + worldmap + theme_void()

library(ggplot2)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(ggrepel)

world   <- ne_countries(scale = "medium", returnclass = "sf")
bra_map <- world |> dplyr::filter(admin == "Brazil")
#eur_map <- world |> dplyr::filter(admin %in% c("United Kingdom","Netherlands","Germany","Italy","Spain"))
eur_map <- ne_countries(
  country = c("Spain","Portugal","France"
              ,"United Kingdom"
              ,"Germany","Belgium","Netherlands","Luxembourg"
              ,"Austria","Switzerland", "Italy"), scale = "medium")

bra_apts_coord <- this_airports |> 
  filter(grepl(pattern = "^SB", x = ICAO)) |> 
  mutate(NUDGE_X = case_when(
    ICAO %in% c("SBSV") ~ -20
    ,.default = -10)
  )

bra_chart <- ggplot2::ggplot() +
  geom_sf(data = bra_map) +
  geom_point(data = this_airports |> filter(grepl(pattern = "^SB", x = ICAO))
             , aes(x = LON, y = LAT), col = bra_col) +
  #geom_label_repel(data = this_airports |> filter(grepl(pattern = "^SB", x = ICAO))
  geom_label_repel(data = this_airports |> filter(grepl(pattern = "^SB", x = ICAO))
                 , aes(x = LON, y = LAT
                         , label = stringr::str_wrap(paste(ICAO, NAME), 8)
                   )
                   
                   ,position = ggpp::position_nudge_center(x = -2, y = 2,
                                                    center_x = 0, center_y = 0),
                  # label.size = NA,
                   label.padding = 0.2
                   
                   , max.overlaps = Inf
                   # , force = 1
                   # , nudge_x = 5
  ) +
  theme_void()

eur_chart <- ggplot2::ggplot() +
  geom_sf(data = eur_map) + 
  coord_sf( xlim = c(-15, 25)
           ,ylim = c(35, NA)
           , expand = FALSE
           ) +
  geom_label_repel(data = this_airports |> filter(grepl(pattern = "^(E|L)", x = ICAO))
                   , aes(x = LON, y = LAT
                         , label = ICAO # paste(ICAO, NAME)
                   )
                   , max.overlaps = Inf
                   , force = 30
                  #  , nudge_x = -2
  ) +
  geom_point(data = this_airports |> filter(grepl(pattern = "^(E|L)", x = ICAO))
             ,aes(x = LON, y = LAT), color = eur_col) +
  theme_void()

#bra_chart

eur_chart


# handle whitespace around sf-plot
# https://stackoverflow.com/questions/68744892/is-there-a-way-to-remove-the-whitespace-outside-of-a-ggplot
plot_ratio <- tmaptools::get_asp_ratio(eur_map)
my_size <- 5

eur_chart |> 
  ggplot2::ggsave(filename = here::here("figures","_map-eur.png")
                  , dpi = 320, bg = "white"
                  , width = plot_ratio * my_size, height = my_size)


# setting labels by hand ----------------------------------------------
bra_apts_coord <- this_airports |> 
  filter(grepl(pattern = "^SB", x = ICAO)) |> 
  mutate(LB_LAT = LAT, LB_LON = LON) |> 
  rows_update(
    tribble(
      ~ICAO, ~LB_LAT, ~LB_LON
      ,"SBSV", -7, -45
      ) 
    , by = "ICAO"
  )


bra_chart2 <- ggplot2::ggplot() +
  geom_sf(data = bra_map) +
  geom_point(data = bra_apts_coord
             , aes(x = LON, y = LAT), col = bra_col) +
  geom_label_repel(data = bra_apts_coord
                   , aes(x = LB_LON, y = LB_LAT
                         , label = stringr::str_wrap(paste0(ICAO," \n", NAME), 10)
                   )
                   
                   ,position = ggpp::position_nudge_center(x = -2, y = 2,
                                                           center_x = 0, center_y = 0),
                   # label.size = NA,
                   label.padding = 0.2
                   
                   , max.overlaps = Inf
                   # , force = 1
                   # , nudge_x = 5
                   # control line spacing in label
                   ,lineheight = 0.7
  ) +
#  theme_void() +
  labs(subtitle = "Brazil")


bra_chart2 + eur_chart2

eur_apts_coords <- this_airports |> filter(grepl(pattern = "^(E|L)", x = ICAO)) |> 
  mutate(LB_LAT = LAT, LB_LON = LON)

eur_chart2 <- ggplot2::ggplot() +
  geom_sf(data = eur_map) + 
  coord_sf(xlim = c(-15, 25), ylim = c(35, NA), expand = FALSE) +
  geom_point( data = eur_apts_coords, aes(x = LON, y = LAT), color = eur_col) +
  geom_label_repel(
              data = eur_apts_coords
             ,aes(x = LB_LON, y = LB_LAT
                  , label = , label = stringr::str_wrap(paste(ICAO, NAME), 8)
                  )
             ,max.overlaps = Inf
             ,force = 35
             # control line spacing in label
             ,lineheight = 0.7) +
#  theme_void() +
  labs(subtitle = "Europe")

bra_chart2 + eur_chart2



## ----------------------------------------------------------------------

# Define airport data
airports <- tribble(
  ~ICAO, ~LAT, ~LON, ~NAME,
  "EDDF", 50.0, 8.56, "Frankfurt",
  "EDDM", 48.4, 11.8, "Munich",
  "EGKK", 51.1, -0.192, "Gatwick",
  "EGLL", 51.5, -0.462, "Heathrow",
  "EHAM", 52.3, 4.76, "Amsterdam",
  "LEBL", 41.3, 2.08, "Barcelona",
  "LEMD", 40.5, -3.56, "Madrid",
  "LFPG", 49.0, 2.55, "Paris",
  "LPPT", 38.8, -9.13, "Lisbon",
  "LSZH", 47.5, 8.55, "Zurich",
  "SBBR", -15.9, -47.9, "Brasília",
  "SBCF", -19.6, -44.0, "Belo Horizonte",
  "SBCT", -25.5, -49.2, "Curitiba",
  "SBGL", -22.8, -43.3, "Galeão",
  "SBGR", -23.4, -46.5, "Guarulhos",
  "SBKP", -23.0, -47.1, "Campinas",
  "SBPA", -30.0, -51.2, "Porto Alegre",
  "SBRJ", -22.9, -43.2, "Santos Dumont",
  "SBSP", -23.6, -46.7, "Congonhas",
  "SBSV", -12.9, -38.3, "Salvador"
)

# Split datasets
eur_airports <- airports |> filter(LAT > 0)
bra_airports <- airports |> filter(LAT < 0)

# Load world map (you can use a better map if you have)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Define plotting function
plot_airports <- function(
      map_data
    , apt_data
    , xlim = NULL
    , ylim = NULL
  #  , title
    , label_force = 1
    , apt_dot_color = "darkblue"
    ) {
  region_plot <- ggplot() +
    geom_sf(data = map_data, fill = "gray90", color = "white") +
    geom_point(data = apt_data, aes(x = LON, y = LAT), color = apt_dot_color, size = 2) +
    geom_label_repel(
      data = apt_data,
      aes(x = LON, y = LAT, label = str_wrap(paste(ICAO, NAME), 10)),
      force = label_force,
      size = 3,
      lineheight = 0.9,
      max.overlaps = Inf,
      box.padding = 0.3,
      segment.size = 0.3
    ) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
#    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(size = 14, face = "bold"))
}

# Europe plot
plot_eur <- plot_airports(
  world, eur_airports,
  xlim = c(-10, 20), ylim = c(36, 58),
#  title = "Europe",
  label_force = 25,
  apt_dot_color = eur_col
)

# Brazil plot
plot_bra <- plot_airports(
  world |> dplyr::filter(admin == "Brazil")
  , bra_airports,
 # xlim = c(-55, -35), ylim = c(-35, -10),
 # title = "Brazil",
  label_force = 20,
 apt_dot_color = bra_col
)

# Combine plots - contorl spacing between plots rather than stitichin it together
combi_plot_scope <- 
(plot_bra + 
   labs(subtitle = "Brazil") +
   theme(plot.margin = unit(c(0,20,0,0), "pt")) 
 ) + 
(plot_eur + 
   labs(subtitle = "Europe") + 
   theme(plot.margin = unit(c(0,0,0,20), "pt")) 
 ) +
  labs(caption = "Charts for both regions not in scale")

combi_plot_scope

combi_plot_scope |> ggsave(
  filename = here::here("figures","_scope-airports-map.png")
 # filename = here::here("figures","_scope-airports-map.pdf")
  ,dpi = 320
  ,width = 8, height = 5, units = "in"
  ,bg = "white")

