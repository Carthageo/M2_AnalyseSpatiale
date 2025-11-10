library(tidyverse)
library(sf)
library(automap)
library(potential)
library(gstat)

# 0 - Données

bars <- st_read("../DATA/MGB_Paris_L93.gpkg", as_tibble = TRUE) %>%
  filter(!is.na(regularPrice))
bars

paris <- st_read("../DATA/Maillages_Limitrophes_Paris.gpkg", layer = "Communes", as_tibble = TRUE) %>%
  filter(INSEE_DEP == "75", INSEE_COM != "75056")

grille_bars <- st_make_grid(bars, cellsize = 100, what = "centers")

# 1- Variogramme

variogramme_bars <- variogram(object = regularPrice ~ 1,
                              data = bars)
plot(variogramme_bars$dist, variogramme_bars$gamma)
summary(lm(gamma ~ dist , data = variogramme_bars))

plot(variogramme_bars)

variogramme_bars_oriente <- variogram(object = regularPrice ~ 1,
                                      data = bars,
                                      alpha=c(0,45,90,135))
plot(variogramme_bars_oriente)

# 2 - Autofit

autofit_bars <- autofitVariogram(formula = regularPrice ~ 1,
                                 input_data = bars
                                 )
plot(autofit_bars)


# 3 - Krigeage

krigeage_bars <- autoKrige(formula = regularPrice ~ 1,
                           input_data = bars,
                           verbose = TRUE)

krigeage_bars_grille <- autoKrige(formula = regularPrice ~ 1,
                                  input_data = bars,
                                  new_data = grille_bars,
                                  verbose = TRUE)
  
  
plot(krigeage_bars_grille)

plot(krigeage_bars_grille$krige_output)

ggplot(krigeage_bars_grille$krige_output) +
  geom_sf(aes(colour = var1.pred)) +
  geom_sf(data = paris, fill = NA, lwd = .75, colour = "white")

equipot_krigeage_bars <- potential::equipotential(x = krigeage_bars_grille$krige_output,
                         var = "var1.pred",
                         nclass = 15,
                         mask = paris)  
plot_krigeage <- ggplot(equipot_krigeage_bars %>% mutate(center = round(center, 2))) +
  geom_sf(aes(fill = factor(center))) +
  geom_sf(data = paris, fill = NA, lwd = .75, colour = "white") +
  scale_fill_viridis_d(name = "Prix de la pinte [€]", guide = guide_legend(nrow = 1),
                       direction = -1) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Potentiels
grille_pot <- potential::create_grid(bars, res = 150)
matrice_distance <- potential::create_matrix(x = bars, y = grille_pot)
mes_potentiels <- potential(x = bars, y = grille_pot,
                            d = matrice_distance, var = "regularPrice",
                            fun = "p", span = 375, beta = 2)
mes_potentiels_ratio <- potential(x = bars %>% mutate(NB = 1), y = grille_pot,
                            d = matrice_distance, var = "NB",
                            fun = "p", span = 375, beta = 2)
grille_pot$mes_potentiels <- mes_potentiels / mes_potentiels_ratio
equipot_potentiels_bars <- potential::equipotential(x = grille_pot,
                                                  var = "mes_potentiels",
                                                  nclass = 15,
                                                  mask = paris)  
plot_potentiel <- ggplot(equipot_potentiels_bars %>% mutate(center = round(center, 2))) +
  geom_sf(aes(fill = factor(center))) +
  geom_sf(data = paris, fill = NA, lwd = .75, colour = "white") +
  scale_fill_viridis_d(name = "Prix de la pinte [€]", guide = guide_legend(nrow = 1),
                       direction = -1) +
  theme_minimal() +
  theme(legend.position = "bottom")


library(patchwork)
wrap_plots(plot_potentiel, plot_krigeage)


