################## SETUP ######################################################
# Quarto renders each chapter in a separate session.
# To save loading the same libraries in every chapter, we define defaults here.
# This script/definitions are sourced() at the beginning of every chapter.
###############################################################################

# load required libraries for each chapter ====================================
library(tidyverse)
library(lubridate)
library(ggrepel)
library(patchwork)
library(ggforce)
#-------- supporting packages
library(flextable)
library(zoo)
library(magrittr)
library(purrr)
library(glue)
library(pdftools)
library(devtools)
# library(readr)     # included in tidyverse
# library(tinytex)
library(arrow)

# ============== DEFAULTS and DEFINITIONS =====================================
# study year
this_year <- 2024

# max_date
max_date <- lubridate::ymd("2024-12-31")

# set ggplot2 default theme
ggplot2::theme_set(theme_minimal())

#============== flextable stuff ===============================================
# set flextable font to surpress warning about used Latex engine
flextable::set_flextable_defaults(
  fonts_ignore = TRUE    # ignore waring of Latex engine
  , font.size = 10         # set some default size and family
  , font.family = "Helvetica")

# set flextable border properties
ft_border = flextable::fp_border_default(width = 0.5)

# study airports and names ====================================================
bra_apts <- c("SBGR","SBGL","SBRJ","SBCF","SBBR","SBSV","SBKP","SBSP","SBCT","SBPA")
eur_apts <- c("EGLL","EGKK","EHAM","EDDF","EDDM","LSZH","LFPG","LEMD","LEBL","LPPT") # ,"LIRF"

bra_apts_names <- tibble::tribble(
  ~ICAO  , ~NAME
  ,"SBGR", "Guarulhos"
  ,"SBGL", "Galeão"
  ,"SBRJ", "Rio Dumont"
  ,"SBCF", "Belo Horizonte"
  ,"SBBR", "Brasília"
  ,"SBSV", "Salvador"
  ,"SBKP", "Campinas"
  ,"SBSP", "Congonhas"
  ,"SBCT", "Curitiba"
  ,"SBPA", "Porto Alegre"
)

eur_apts_names <- tibble::tribble(
  ~ICAO  , ~NAME
  ,"EGLL", "Heathrow"
  ,"EGKK", "Gatwick"
  ,"EHAM", "Amsterdam"
  ,"EDDF", "Frankfurt"
  ,"EDDM", "Munich"
  ,"LSZH", "Zurich"
  ,"LIRF", "Rome"
  ,"LFPG", "Paris"
  ,"LEMD", "Madrid"
  ,"LEBL", "Barcelona"
  ,"LPPT", "Lisbon"
)



# define standard theme aspects for Brazil and Europe =========================
bra_eur_colours <- c(BRA = "#52854C",EUR = "#4E84C4")
bra_col         <- getElement(bra_eur_colours, "BRA")
eur_col         <- getElement(bra_eur_colours, "EUR")

# theme setting - tbd or replaced
bra_eur_theme_minimal <- theme_minimal() + theme(axis.title = element_text(size = 9))
bra_eur_theme_bw      <- theme_bw() + theme(axis.title = element_text(size = 9))

