###################################
##### CHARGEMENT DES PACKAGES #####
###################################
library(tidyverse)
library(readxl)
library(sf)
library(potential)
library(patchwork)

##############################
##### IMPORT DES DONNEES #####
##############################

communes_paris <- st_read("../data/Communes_LimitrophesParis_L93.gpkg", as_tibble = TRUE) %>%
  st_transform(2154) %>%
  filter(INSEE_DEP == "75") %>%
  filter(INSEE_COM != "75056")

contour_paris <- communes_paris %>% summarise() %>% st_buffer(500)

bureauxVote <- st_read("../data/BureauxVote_Paris_L93.gpkg", as_tibble = TRUE) %>%
  st_transform(2154)
resultatsVote <- read_xlsx("../data/BureauxVote_Paris_Resultats_Presi2022_T1.xlsx") %>%
  select(-c(1:6))

resultats_bureaux <- bureauxVote %>%
  left_join(resultatsVote, by = c("CodeBVote" = "Code du b.vote")) %>%
  mutate(Inscrits = as.numeric(Inscrits))


#################################
##### MESURE DES  DISTANCES #####
#################################

############ 1 - Distance au plus proche voisin ############

library(nngeo)
bureaux_distincts <- bureaux_distincts %>% select() %>% distinct()
distance_bureaux <- st_nn(x = bureaux_distincts, y = bureaux_distincts, k = 2, returnDist = TRUE)
distance_bureaux_reelle <- distance_bureaux$dist %>%
  as.matrix() %>%
  as_tibble() %>%
  rename(distance = V1) %>%
  mutate(ID = row_number()) %>%
  unnest(cols = distance) %>%
  group_by(ID) %>%
  slice_max(distance, n = 1) %>%
  ungroup()

ggplot(distance_bureaux_reelle) +
  aes(distance) +
  geom_histogram(aes(y = after_stat(density)), fill = "#67c9ff", colour = "white") +
  geom_density()

distance_bureaux_reelle %>%
  arrange(distance) %>%
  mutate(dummy = 1/nrow(distance_bureaux_reelle)) %>%
  mutate(cumfreq = cumsum(dummy)) %>%
  ggplot() +
  aes(distance, cumfreq) +
  geom_line() +
  scale_y_continuous(labels = scales::percent)

############ 2 - Distances aux n plus proche voisins ############

bureaux_distincts <- bureaux_distincts %>% select() %>% distinct()
distance_bureaux_10 <- st_nn(x = bureaux_distincts, y = bureaux_distincts, k = 11, returnDist = TRUE)
distance_bureaux_10_reelle <- distance_bureaux_10$dist %>%
  as.matrix() %>%
  as_tibble() %>%
  rename(distance = V1) %>%
  mutate(ID = row_number()) %>%
  unnest(cols = distance) %>%
  group_by(ID) %>%
  mutate(voisinage = row_number() - 1) %>%
  ungroup() %>%
  filter(voisinage != 0) %>%
  mutate(voisinage = as.factor(voisinage))

library(ggridges)

ggplot(distance_bureaux_10_reelle) +
  geom_density_ridges(aes(x = distance, y = voisinage))
ggplot(distance_bureaux_10_reelle) +
  geom_density_ridges(aes(x = distance, y = voisinage), stat = "binline", scale = 0.9)

############ 3 - Distances moyennes ############

matrice_distances <- resultats_bureaux %>%
  st_distance(by_element = FALSE) %>%
  as_tibble() %>%
  mutate(ID1 = as.character(row_number())) %>%
  pivot_longer(-ID1) %>%
  rename(ID2 = name, distance = value) %>%
  filter(ID1 != ID2) %>%
  mutate(distance = as.numeric(distance))

ggplot(matrice_distances) +
  geom_histogram(aes(distance, after_stat(density)))


############ 4 - Distances et différences ############

resultats_macron <- resultats_bureaux %>%
  mutate(ID = as.character(row_number())) %>%
  mutate(pctMacron = MACRON / Inscrits) %>%
  select(ID, pctMacron) %>%
  st_drop_geometry() %>%
  as_tibble()

distances_differences <- matrice_distances %>%
  left_join(resultats_macron, by = c("ID1" = "ID")) %>%
  rename(pctMacron_ID1 = pctMacron) %>%
  left_join(resultats_macron, by = c("ID2" = "ID")) %>%
  rename(pctMacron_ID2 = pctMacron) %>%
  mutate(differencePct = pctMacron_ID1 - pctMacron_ID2)

ggplot(distances_differences) +
  aes(x = distance, y = differencePct) +
  geom_point()

distances_differences <- distances_differences %>%
  filter(ID1 < ID2) %>%
  mutate(AbsDifference = abs(differencePct))

ggplot(distances_differences) +
  aes(x = distance, y = AbsDifference) +
  geom_point()


##### On discrétise par bornes de 100m

distances_differences_discretes <- distances_differences %>%
  mutate(bande = cut(distance, breaks = c(0:120) * 100, labels = FALSE)) %>%
  mutate(bande = bande * 100)

meanDiff <- distances_differences_discretes %>%
  group_by(bande) %>%
  summarise(diffmoyenne = mean(AbsDifference), nb = n())

ggplot(meanDiff) +
  aes(x = bande, y = diffmoyenne, size = nb) +
  geom_point() +
  scale_y_continuous(labels = scales::percent)

######## SemiVariogramme :

semivarDiff <- distances_differences_discretes %>%
  group_by(bande) %>%
  summarise(semivar = 0.5 * mean(AbsDifference^2), nb = n())

ggplot(semivarDiff) +
  aes(x = bande, y = semivar, size = nb) +
  geom_point() +
  scale_y_continuous()

#################### Dirigé

resultats_macron_geom <- resultats_bureaux %>%
  mutate(ID = as.character(row_number())) %>%
  mutate(pctMacron = MACRON / Inscrits) %>%
  select(ID, pctMacron)

pts_origine <- distances_differences_discretes %>%
  select(ID1) %>%
  left_join(resultats_macron_geom, by = c("ID1" = "ID")) %>%
  st_sf()

pts_destination <- distances_differences_discretes %>%
  select(ID2) %>%
  left_join(resultats_macron_geom, by = c("ID2" = "ID")) %>%
  st_sf()

differences_azimuthales <- st_azimuth(pts_origine, pts_destination)  

distances_differences_discretes_azimuthales <- distances_differences_discretes %>%
  mutate(azimuth = differences_azimuthales) %>%
  mutate(direction = case_when(
    azimuth >= 337.5 | azimuth <= 22.5 ~ "Nord",
    between(azimuth, 22.5, 67.5) ~ "Nord-Est",
    between(azimuth, 67.5, 112.5) ~ "Est",
    between(azimuth, 112.5, 157.5) ~ "Sud-Est",
    between(azimuth, 157.5, 202.5) ~ "Sud",
    between(azimuth, 202.5, 247.5) ~ "Sud-Ouest",
    between(azimuth, 247.5, 292.5) ~ "Ouest",
    between(azimuth, 292.5, 337.5) ~ "Nord-Ouest"
  )) %>%
  mutate(directionSym = case_when(
    direction %in% c("Nord", "Sud") ~ "NS",
    direction %in% c("Est", "Ouest") ~ "EW",
    direction %in% c("Nord-Est", "Sud-Ouest") ~ "NE-SW",
    direction %in% c("Nord-Ouest", "Sud-Est") ~ "NW-SE",
  )) %>%
  filter(!is.na(directionSym)) %>%
  group_by(bande, directionSym) %>%
  summarise(
    nb = n(),
    semivar = 0.5 * mean(AbsDifference^2)
  ) %>%
  ungroup()

ggplot(distances_differences_discretes_azimuthales) +
  aes(x = bande, y = semivar, size = nb) +
  geom_point() +
  facet_wrap(~directionSym, ncol = 1)


library(ggpubr)

ggplot(distances_differences_discretes_azimuthales) +
  aes(x = bande, y = semivar, size = nb) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm", lwd = 1) +
  stat_cor(label.y = 0.015, label.x = 1000) +
  stat_regline_equation(label.y = 0.01, label.x = 1000) +
  facet_wrap(~directionSym, ncol = 1)


ggplot(distances_differences_discretes_azimuthales) +
  aes(x = bande, y = semivar * 100) +
  geom_point(aes(size = nb)) +
  geom_smooth(se = FALSE, method = "lm", lwd = 1) +
  stat_cor(label.y = 1.5, label.x = 1000) +
  stat_regline_equation(label.y = 1.25, label.x = 1000) +
  facet_wrap(~directionSym, ncol = 1)


gstat::show.vgms(nugget = 0.25, sill = 1.5)



########## Réalisation du krigage en automatique ################

library(automap)

sp_resultats_macron_geom <- as_Spatial(resultats_macron_geom)
sp_grille <- contour_paris %>%
  st_make_grid(cellsize = 100, what = "centers") %>%
  as_Spatial()

kriging_interpolation_pctMacron <- autoKrige(
  formula = pctMacron ~ 1,
  input_data = sp_resultats_macron_geom,
  new_data = sp_grille, 
  verbose = TRUE
)

sf_kriging_interpolation_pctMacron <- st_as_sf(kriging_interpolation_pctMacron$krige_output)

ggplot(sf_kriging_interpolation_pctMacron) +
  geom_sf(aes(colour = var1.pred))
ggplot(sf_kriging_interpolation_pctMacron) +
  geom_sf(aes(colour = var1.var))
ggplot(sf_kriging_interpolation_pctMacron) +
  geom_sf(aes(colour = var1.stdev))

equipotentiels_pctMacron <- equipotential(sf_kriging_interpolation_pctMacron, var = "var1.pred", nclass = 10) %>%
  st_intersection(communes_paris)

carte_prediction <- ggplot(equipotentiels_pctMacron) +
  geom_sf(aes(fill = center)) +
  geom_sf(data = communes_paris, fill = NA, colour = "grey80", lwd = 0.2) +
  scale_fill_steps(n.breaks = 7, low = "white", high = "#ffeb00")

equipotentiels_pctMacron_variance <- equipotential(sf_kriging_interpolation_pctMacron, var = "var1.var", nclass = 10) %>%
  st_intersection(communes_paris)

carte_variance <- ggplot(equipotentiels_pctMacron_variance) +
  geom_sf(aes(fill = center)) +
  geom_sf(data = communes_paris, fill = NA, colour = "grey80", lwd = 0.2) +
  scale_fill_viridis_c()

equipotentiels_pctMacron_stdev <- equipotential(sf_kriging_interpolation_pctMacron, var = "var1.stdev", nclass = 10) %>%
  st_intersection(communes_paris)

carte_stdev <- ggplot(equipotentiels_pctMacron_stdev) +
  geom_sf(aes(fill = center)) +
  geom_sf(data = communes_paris, fill = NA, colour = "grey80", lwd = 0.2) +
  scale_fill_viridis_c()

patchwork::wrap_plots(carte_prediction, carte_variance, carte_stdev, nrow = 1)
