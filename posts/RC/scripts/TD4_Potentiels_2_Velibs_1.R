library(tidyverse)
library(sf)
library(potential)

velibs <- st_read("../DATA/Velibs_Paris_20241013_17h30.gpkg", as_tibble = TRUE)
communes <- st_read("../DATA/Maillages_Limitrophes_Paris.gpkg", layer = "Communes")
grille <- potential::create_grid(x = communes, res = 100)

matrice_distance <- potential::create_matrix(velibs, grille)
potentiel_capacite <- potential(x = velibs, y = grille, d = matrice_distance,
                                var = "numbikesavailable", fun = "p", span = 200, beta = 2)

grille$potentiel_capacite <- potentiel_capacite
plot(grille["potentiel_capacite"])


