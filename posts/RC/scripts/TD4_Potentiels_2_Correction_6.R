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
    theme(axis.ticks = element_blank(), axis.text = element_blank(),
          panel.grid = element_blank(), legend.position = "bottom") +
    labs(title = ce_candidat)
  toutes_cartes_pct[[ce_candidat]] <- carte_candidat
}
wrap_plots(toutes_cartes_pct)