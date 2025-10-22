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

communes_paris <- st_read("../data/Maillages_Limitrophes_Paris.gpkg", layer = "Communes", as_tibble = TRUE) %>%
  st_transform(2154) %>%
  filter(INSEE_DEP == "75") %>%
  filter(INSEE_COM != "75056")

contour_paris <- communes_paris %>% summarise() %>% st_buffer(500)

bureauxVote <- st_read("../data/BureauxVote_Paris_L93.gpkg", as_tibble = TRUE) %>%
  st_transform(2154)
resultatsVote <- readxl::read_xlsx("../data/BureauxVote_Paris_Resultats_Presi2022_T1.xlsx") %>%
  select(-c(1:6))

resultats_bureaux <- bureauxVote %>%
  left_join(resultatsVote, by = c("CodeBVote" = "Code du b.vote")) %>%
  mutate(Inscrits = as.numeric(Inscrits))


#################################
##### CREATION DE LA GRILLE #####
#################################

grille_analyse <- create_grid(x = communes_paris, res = 200)
matrice_distance_euclidienne <- create_matrix(x = resultats_bureaux,
                                              y = grille_analyse,
                                              longlat = FALSE,
                                              checksize = FALSE)

#################################
##### CALCUL DES POTENTIELS #####
#################################

candidats <- resultats_bureaux %>%
  st_drop_geometry() %>%
  select(Inscrits, "ARTHAUD":"DUPONT-AIGNAN") %>%
  colnames()

couleurs_candidats <- tribble(
  ~couleur, ~candidat,
  "#bb0000", "Arthaud",
  "#dd0000", "Roussel",
  "#ffeb00", "Macron",
  "#0d378a", "Le Pen",
  "#cc2443", "Mélenchon",
  "#404040", "Zemmour",
  "#0066cc", "Pécresse",
  "#00c000", "Jadot",
  "#26c4ec", "Lassalle",
  "#0082c4", "Dupont-Aignan",
  "#ff8080", "Hidalgo",
  "#bb0000", "Poutou"
) %>%
  mutate(candidat = toupper(candidat))
  

resultat_potentiels <- potential(x = resultats_bureaux,
                                 y = grille_analyse,
                                 d = matrice_distance_euclidienne,
                                 var = candidats,
                                 fun = "e",
                                 span = 500,
                                 beta = 2) %>%
  as_tibble()

grille_resultats <- grille_analyse %>%
  bind_cols(resultat_potentiels)


#######################################
##### CARTOGRAPHIE DES POTENTIELS #####
#######################################

#### CARTOGRAPHIE NAÏVE

ggplot(grille_resultats) +
  geom_sf(aes(colour = ARTHAUD)) +
  geom_sf(data = communes_paris, fill = NA, colour = "grey80", lwd = 0.5)


equipot_macron <- equipotential(x = grille_resultats, var = "MACRON", nclass = 6, mask = communes_paris)

ggplot(equipot_macron) +
  geom_sf(data = communes_paris %>% summarise(), fill = NA, colour = "black", lwd = 2) +
  geom_sf(data = communes_paris, fill = NA, colour = "grey50", lwd = 1) +
  geom_sf(data = equipot_macron, aes(fill = center), alpha = 0.8) +
  scale_fill_steps(n.breaks = 7, low = "white", high = "#ffeb00") +
  theme_minimal()+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "Potentiel de voix pour Macron")


####################################
##### CARTOGRAPHIE AUTOMATIQUE #####
####################################
toutes_cartes <- list()
for (ce_candidat in couleurs_candidats$candidat){
  couleur_candidat <- couleurs_candidats %>% filter(candidat == ce_candidat) %>% pull(couleur)
  equipot <- equipotential(grille_resultats, var = ce_candidat, nclass = 6) %>%
    st_make_valid()
  equipot_cropped <- st_intersection(equipot, contour_paris)
  carte_candidat <- ggplot(equipot_cropped) +
    geom_sf(aes(fill = center)) +
    geom_sf(data = communes_paris, fill = NA, colour = "white", lwd = 0.2) +
    scale_fill_steps(n.breaks = 7, low = "white", high = couleur_candidat) +
    theme_minimal()+
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          legend.position = "bottom") +
    labs(title = ce_candidat)
  toutes_cartes[[ce_candidat]] <- carte_candidat
}

wrap_plots(toutes_cartes)

#########################################
##### CARTOGRAPHIE AUTOMATIQUE EN % #####
#########################################

toutes_cartes_pct <- list()
for (ce_candidat in couleurs_candidats$candidat){
  couleur_candidat <- couleurs_candidats %>% filter(candidat == ce_candidat) %>% pull(couleur)
  grille_resultats2 <- grille_resultats %>%
    select(ID, COORDX, COORDY, Inscrits, all_of(ce_candidat)) %>%
    mutate(across(ce_candidat, ~.x/Inscrits))
  
  equipot <- equipotential(grille_resultats2, var = ce_candidat, nclass = 6) %>%
    st_make_valid()
  equipot_cropped <- st_intersection(equipot, contour_paris)
  
  carte_candidat <- ggplot(equipot_cropped) +
    geom_sf(aes(fill = center)) +
    geom_sf(data = communes_paris, fill = NA, colour = "white", lwd = 0.2) +
    scale_fill_steps(n.breaks = 7, low = "white", high = couleur_candidat) +
    theme_minimal()+
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          legend.position = "bottom") +
    labs(title = ce_candidat)
  toutes_cartes_pct[[ce_candidat]] <- carte_candidat
}

toutes_mes_cartes_de_potentiel <- patchwork::wrap_plots(toutes_cartes_pct)
toutes_mes_cartes_de_potentiel
ggsave(filename = "cartes_potentiels.pdf", plot = toutes_mes_cartes_de_potentiel,
     width = 30, height = 20, units = "cm", scale = 2)

############################################
##### ANALYSE SENSIBILITE CARTO MACRON #####
############################################

beta_var <- c(0, 0.5, 1 , 2, 4)
span_var <- c(100, 250, 500, 750, 1000)

grille_resultats <- grille_analyse

for (this_beta in beta_var){
  for (this_span in span_var){
  resultat_potentiels_sensib <- potential(x = resultats_bureaux, y = grille_analyse, d = matrice_distance_euclidienne,
                                          var = c("Inscrits", "MACRON"),
                                          fun = "e",  beta = this_beta, span = this_span) %>%
    as_tibble() %>%
    mutate(pctMACRON = MACRON / Inscrits)
  
  resultat_potentiels_sensib_renamed <- resultat_potentiels_sensib %>%
    rename_with(~glue::glue("{.x}_{this_beta}_{this_span}"), contains("pctMACRON")) %>%
    select(contains("pctMACRON"))
  grille_resultats <- grille_resultats %>%
    bind_cols(resultat_potentiels_sensib_renamed)
  }
}

colonnes_resultats <- grille_resultats %>% st_drop_geometry() %>% select(contains("pctMACRON")) %>% colnames()

plot_list <- list()
for (this_result in colonnes_resultats){
  equipot_macron <- equipotential(x = grille_resultats, var = this_result, nclass = 6) %>% st_make_valid()
  equipot_macron_cropped <- st_intersection(equipot_macron, contour_paris)
  
  beta <- str_split(this_result, pattern = "_", simplify = TRUE)[2]
  span <- str_split(this_result, pattern = "_", simplify = TRUE)[3]
  this_plot <- ggplot(equipot_macron_cropped) +
    geom_sf(aes(fill = center)) +
    geom_sf(data = communes_paris, fill = NA, colour = "grey80", lwd = 0.2) +
    scale_fill_steps(n.breaks = 7, low = "white", high = "#ffeb00") +
    theme_minimal()+
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank()) +
    labs(subtitle = glue::glue("Span : {span} / Beta : {beta}"))
  
  plot_list[[this_result]] <- this_plot
}

patchwork::wrap_plots(plot_list, ncol = 5)
ggsave(plot = last_plot(), filename = "../TD4/carte_sensib_potentiels_macron.pdf", width = 30, height = 20, units = "cm", scale = 2)
