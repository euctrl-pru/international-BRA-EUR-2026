table_bra_eur_filtered <- table_bra_eur |>
  dplyr::select(
    KPA,
    Brazil_2023,
    Brazil_2024,
    Brazil_2025,
    Europe_2023,
    Europe_2024,
    Europe_2025
  )

theme_bra <- "#52854C"
theme_eur <- "#4E84C4"
theme_dark <- "#1F2937"

ft <- flextable::flextable(table_bra_eur_filtered)

# TOP HEADER
ft <- flextable::add_header_row(
  ft,
  values = c("", "BRAZIL", "EUROPE"),
  colwidths = c(1, 3, 3)
)

# COLUMN LABELS
ft <- flextable::set_header_labels(
  ft,
  KPA = "KPA",
  Brazil_2023 = "2023",
  Brazil_2024 = "2024",
  Brazil_2025 = "2025",
  Europe_2023 = "2023",
  Europe_2024 = "2024",
  Europe_2025 = "2025"
)

# MERGE HEADER CELLS
ft <- flextable::merge_h(ft, part = "header")

# BASE THEME
ft <- flextable::theme_vanilla(ft)

# HEADER COLORS
ft <- flextable::bg(
  ft,
  i = 1,
  j = 2:4,
  bg = theme_bra,
  part = "header"
)

ft <- flextable::bg(
  ft,
  i = 1,
  j = 5:7,
  bg = theme_eur,
  part = "header"
)

ft <- flextable::bg(
  ft,
  i = 2,
  bg = theme_dark,
  part = "header"
)

# HEADER TEXT
ft <- flextable::color(
  ft,
  color = "white",
  part = "header"
)

ft <- flextable::bold(
  ft,
  bold = TRUE,
  part = "header"
)

# KPA COLUMN
ft <- flextable::bold(
  ft,
  j = 1,
  bold = TRUE,
  part = "body"
)

# ALIGNMENT
ft <- flextable::align(
  ft,
  align = "center",
  part = "all"
)

ft <- flextable::align(
  ft,
  j = 1,
  align = "left",
  part = "body"
)

ft <- flextable::valign(
  ft,
  valign = "center",
  part = "all"
)

ft <- flextable::height_all(
  ft,
  height = 0.35
)

# FONT SIZE
ft <- flextable::fontsize(
  ft,
  size = 14,
  part = "body"
)

ft <- flextable::fontsize(
  ft,
  size = 18,
  part = "header"
)

# ZEBRA STRIPES
ft <- flextable::bg(
  ft,
  i = seq(1, nrow(table_bra_eur_filtered), 2),
  bg = "#F7F9FC",
  part = "body"
)

# COLUMN WIDTHS
ft <- flextable::width(ft, j = 1, width = 3.2)
ft <- flextable::width(ft, j = 2:7, width = 0.85)

ft <- flextable::line_spacing(
  ft,
  space = 1.3,
  part = "body"
)

# PADDING
ft <- flextable::padding(
  ft,
  padding = 10
)

# REMOVE DEFAULT BORDERS
ft <- flextable::border_remove(ft)

# HORIZONTAL LINES
ft <- flextable::hline(
  ft,
  border = officer::fp_border(
    color = "#D9DEE5",
    width = 1
  ),
  part = "body"
)



flextable::save_as_image(
  ft,
  path = here::here("figures", "table_bra_eur.png"),
  zoom = 6
)
