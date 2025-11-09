library(sf)
library(tidyverse)
library(spdep)
library(sfdep)
library(glue)

setwd("../DATA/")

communes_paris <- st_read("Maillages_Limitrophes_Paris.gpkg", layer = "Communes", as_tibble = TRUE) %>%
  st_transform(2154) %>%
  filter(INSEE_DEP == "75") %>%
  filter(INSEE_COM != "75056")

contour_paris <- communes_paris %>% summarise() %>% st_buffer(500)

votes <- st_read("BureauxVote_Paris_L93.gpkg")
resultats <- readxl::read_xlsx("BureauxVote_Paris_Resultats_Presi2022_T1.xlsx") %>%
  select(CodeBVote = 7, 8:25) %>%
  mutate(Inscrits = as.numeric(Inscrits)) %>%
  janitor::clean_names()

bureaux_avec_resultats <- votes %>%
  left_join(resultats, by = c("CodeBVote" = "code_b_vote"))

bureaux_distincts <- bureaux_avec_resultats %>%
  group_by(geom) %>%
  summarise(
    across(where(is.character), ~first(.x)),
    across(where(is.numeric), ~sum(.x)),
    .groups = "drop"
  ) %>%
  ungroup()

resultats_votes <- bureaux_distincts %>%
  filter(exprimes > 0) %>%
  mutate(across(arthaud:dupont_aignan, ~.x/exprimes, .names = "pct_{.col}"))


voisinage_NN <- sfdep::st_knn(geometry = resultats_votes, k = 5)
voisinage_DIST <- sfdep::st_dist_band(geometry = resultats_votes, upper = 300)

poids_KNN <- sfdep::st_weights(nb = voisinage_NN, style = "W")
poids_DIST_GAUSS <- sfdep::st_kernel_weights(nb = voisinage_DIST, geometry = resultats_votes, kernel = "uniform")
poids_DIST_IDW <- sfdep::st_inverse_distance(nb = voisinage_DIST, geometry = resultats_votes, alpha = 2)

sfdep::global_moran(x = resultats_votes$pct_macron, nb = voisinage_NN, wt = poids_KNN)$I
sfdep::global_moran(x = resultats_votes$pct_macron, nb = voisinage_DIST, wt = poids_DIST_GAUSS)$I
sfdep::global_moran(x = resultats_votes$pct_macron, nb = voisinage_DIST, wt = poids_DIST_IDW)$I


moran_nn <- tibble(NN = c(), I = c())
for (nn in 1:100){
  voisinage_NN <- sfdep::st_knn(geometry = resultats_votes, k = nn)
  poids_KNN <- sfdep::st_weights(nb = voisinage_NN, style = "W")
  foo <- sfdep::global_moran(x = resultats_votes$pct_macron, nb = voisinage_NN, wt = poids_KNN)$I
  moran_nn <- moran_nn %>% bind_rows(tibble(NN = nn, I = foo))
}
moran_nn
ggplot(moran_nn) +
  aes(NN, I) %>%
  geom_point()


voisinage_DIST <- sfdep::st_dist_band(geometry = resultats_votes)
moran_kernel <- tibble(kernel = c(), I = c())
for (kernel in c("uniform", "gaussian", "triangular", "epanechnikov", "quartic")){
  poids_DIST <- sfdep::st_kernel_weights(nb = voisinage_DIST, geometry = resultats_votes, kernel = kernel)
  foo <- sfdep::global_moran(x = resultats_votes$pct_macron, nb = voisinage_DIST, wt = poids_DIST)$I
  moran_kernel <- moran_kernel %>% bind_rows(tibble(kernel = kernel, I = foo))
}
moran_kernel
ggplot(moran_kernel) +
  aes(factor(kernel), I) %>%
  geom_col()

# MORAN LOCAL


voisinage_DIST <- sfdep::st_dist_band(geometry = resultats_votes)
poids_DIST <- sfdep::st_kernel_weights(nb = voisinage_DIST, geometry = resultats_votes, kernel = "quartic")



data_lag <- resultats_votes %>%
  mutate(wt_macron = poids_DIST %>% map(~sum(.x)) %>% unlist()) %>%
  mutate(lag_macron = st_lag(pct_macron, nb = voisinage_DIST, wt = poids_DIST) / wt_macron) %>%
  mutate(mean_macron = mean(pct_macron, na.rm = TRUE),
         mean_lag = mean(lag_macron, na.rm = TRUE)) %>%
  mutate(quadrant = case_when(
    pct_macron > mean_macron & lag_macron > mean_lag ~ "High-High",
    pct_macron < mean_macron & lag_macron < mean_lag ~ "Low-Low",
    pct_macron >= mean_macron & lag_macron <= mean_lag ~ "High-Low",
    pct_macron <= mean_macron & lag_macron >= mean_lag ~ "Low-High"
  )) %>%
  filter(!is.na(quadrant))

moran_plot <- ggplot(data_lag) +
  aes(x = pct_macron, y = lag_macron) +
  geom_point() +
  labs(x = "Variable", y = "Variable dans le voisinage")

moran_plot

moran_plot +
  geom_hline(yintercept = mean(data_lag$lag_macron, na.rm = TRUE)) +
  geom_vline(xintercept = mean(data_lag$pct_macron, na.rm = TRUE))

moran_plot +
  aes(colour = quadrant) +
  geom_hline(yintercept = mean(data_lag$lag_macron, na.rm = TRUE)) +
  geom_vline(xintercept = mean(data_lag$pct_macron, na.rm = TRUE))

moran_plot +
  aes(colour = quadrant, alpha = quadrant) +
  geom_hline(yintercept = mean(data_lag$lag_macron, na.rm = TRUE)) +
  geom_vline(xintercept = mean(data_lag$pct_macron, na.rm = TRUE)) +
  scale_color_discrete(guide = NULL) + 
  scale_alpha_manual(values = c("High-High" = 1, "High-Low" = 0.2, "Low-High" = 0.2, "Low-Low" = 1), guide = NULL) +
  coord_equal()

candidat <- "pct_macron"

foo <- local_moran(x = resultats_votes[[candidat]], nb = voisinage_DIST, wt = poids_DIST)
plot(foo)  


resultats_locaux <- resultats_votes %>% slice(0) %>% select() %>% mutate(candidat = NA_character_, size = NA_integer_, quadrant = NA_character_)
for (candidat in colnames(resultats_votes %>% st_drop_geometry() %>% select(24:34))){
  moran_global <- global_moran(x = resultats_votes[[candidat]], nb = voisinage_DIST, wt = poids_DIST)$I %>% round(., digits = 2)
  geary_global <- sfdep::global_c(x = resultats_votes[[candidat]], nb = voisinage_DIST, wt = poids_DIST)$C %>% round(., digits = 1)
  candidat_title <- candidat %>% str_remove("pct_") %>% str_to_upper()
  resultats_moran <- local_moran(x = resultats_votes[[candidat]], nb = voisinage_DIST, wt = poids_DIST) %>%
    select(mean) %>%
    mutate(size = case_when(
      mean == "High-High" ~ 1,
      mean == "Low-Low" ~ 1,
      .default = 0.5)) %>%
    mutate(candidat = glue("{candidat_title} \nMoran's I = {moran_global}\nGeary's C = {geary_global}")) %>%
    select(candidat, size, quadrant = mean)
  resultats_moran_geo <- resultats_votes %>% select() %>% bind_cols(resultats_moran)
  resultats_locaux <- resultats_locaux %>% bind_rows(resultats_moran_geo)
}

resultats_locaux %>%
  ggplot() +
  geom_sf(aes(colour = quadrant, size = size)) +
  facet_wrap(~candidat) +
  scale_size_identity() +
  theme_void()



voro_per_geom <- function(sfc, contour = contour_paris){
  sf <- sfc %>% st_sf() %>% mutate(sf, id = row_number())
  new_geom <- st_combine(sf) %>% st_voronoi(envelope = st_as_sfc(contour_paris)) %>% st_collection_extract()
  new_geom %>% st_sf() %>% st_join(sf) %>% arrange(id) %>% st_geometry()
}

resultats_locaux %>%
  group_split(candidat) %>%
  map(., function(x){st_set_geometry(x, voro_per_geom(x$geom))}) %>%
  bind_rows() %>%
  st_intersection(contour_paris) %>%
  ggplot() +
  geom_sf(aes(fill = quadrant, alpha = size)) +
  facet_wrap(~candidat) +
  scale_alpha(range = c(0.2,1))
