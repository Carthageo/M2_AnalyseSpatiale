library(tidyverse)
library(sf)
library(automap)
library(potential)

paris <- st_read("../DATA/Maillages_Limitrophes_Paris.gpkg", layer = "Communes", as_tibble = TRUE, quiet = TRUE) %>%
  filter(INSEE_COM != "75056", INSEE_DEP == "75")
bars <- st_read("../DATA/MGB_Paris_L93.gpkg", as_tibble = TRUE, quiet = TRUE) %>%
  filter(!is.na(regularPrice))
grille <- st_make_grid(paris, cellsize = 100, what = "centers") %>%
  st_sf()

variogramme_bars <- autofitVariogram(regularPrice ~ 1, input_data = bars)
variogramme_bars
