library(ggplot)

ggplot(iris) +
  aes(Sepal.Length, Sepal.Width)  +
  geom_point() +
  facet_wrap(~Species)



library(tidyverse)
library(sf)
library(automap)


# 0 - Charge packages et données
library(sf)
library(tidyverse)
library(potential)
library(automap)

limites_paris <- st_read("../data/Communes_LimitrophesParis_L93.gpkg") %>%
  filter(INSEE_DEP == "75")
bureaux_vote <- st_read("../data/BureauxVote_Paris_L93.gpkg")
resultats_vote <- readxl::read_xlsx("../data/BureauxVote_Paris_Resultats_Presi2022_T1.xlsx")
bureaux_avec_resultats <- bureaux_vote %>%
  left_join(resultats_vote, by = c("CodeBVote" = "Code du b.vote"))

grille_paris <- limites_paris %>% st_make_grid(cellsize = 200,
                                               what = "centers")

resultats_votes <- bureaux_avec_resultats %>%
  janitor::clean_names() %>%
  mutate(inscrits = as.numeric(inscrits)) %>%
  filter(inscrits > 0) %>%
  mutate(tauxPecresse = pecresse / inscrits)

resultat_kriging <- automap::autoKrige(
    formula = tauxPecresse ~ 1,
    input_data = resultats_votes,
    new_data = grille_paris,
    verbose = TRUE
)

resultat_kriging_sf <- resultat_kriging$krige_output %>% st_as_sf()

plot(resultat_kriging_sf)

mes_equipotentiels <- equipotential(resultat_kriging_sf, var = "var1.pred", nclass = 5)
plot(mes_equipotentiels)

ggplot() +
  geom_sf(data = mes_equipotentiels, aes(fill = center)) +
  geom_sf(data = limites_paris, fill = NA, colour = 'white') +
  scale_fill_gradient2(low = "white", high = "#0066cc", labels = scales::percent)


resultats_votes <- bureaux_avec_resultats %>%
  janitor::clean_names() %>%
  filter(exprimes > 0) %>%
  mutate(across(arthaud:dupont_aignan, ~.x/exprimes, .names = "pct_{.col}"))

pct_candidats <- resultats_votes %>% st_drop_geometry() %>% select(starts_with("pct_")) %>% colnames()

tous_resultats_krigeage <- list()
for (this_candidat in pct_candidats){
  resultat_krigeage <- autoKrige(
    formula = eval(as.name(paste(this_candidat))) ~ 1,
    #formula = pct_lassalle ~ 1,
    input_data = resultats_votes,
    new_data = grille_paris,
    verbose = TRUE
  )
  
  tous_resultats_krigeage[[this_candidat]] <- resultat_krigeage
}


tous_resultats_krigeage

