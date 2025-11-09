library(tidyverse)
library(sf)
library(automap)
library(potential)

paris <- st_read("../DATA/Maillages_Limitrophes_Paris.gpkg", layer = "Communes", as_tibble = TRUE) %>%
  filter(INSEE_COM != "75056", INSEE_DEP == "75")
bars <- st_read("../DATA/MGB_Paris_L93.gpkg", as_tibble = TRUE) %>%
  filter(!is.na(regularPrice))
grille <- st_make_grid(paris, cellsize = 100, what = "centers") %>%
  st_sf()

variogramme_bars <- autofitVariogram(regularPrice ~ 1, input_data = bars)
variogramme
plot(variogramme)

krigeage_bars <- autoKrige(formula = regularPrice ~ 1, input_data = bars, new_data = grille)
write_rds(krigeage_bars, file = "../DATA/krigeage_bars.rds")
krigeage_bars
plot(krigeage_bars)

equipotential(x = krigeage_bars$krige_output, var = "var1.pred", nclass = 8, mask = paris) %>%
  mutate(center = fct_rev(as.factor(round(center, 2)))) %>%
  ggplot() +
  geom_sf(aes(fill = center)) +
  geom_sf(data = paris, fill = NA, colour = "white") +
  scale_fill_viridis_d(name = "Prix de la pinte [€]", direction = -1)
