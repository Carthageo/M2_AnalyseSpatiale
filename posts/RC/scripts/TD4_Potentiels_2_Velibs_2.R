# Discretisation
potentiels_discrets <- equipotential(x = grille, var = "potentiel_capacite", nclass = 8, mask = communes)
ggplot(potentiels_discrets) +
  geom_sf(aes(fill = center)) +
  geom_sf(data = communes, fill = NA, lwd = 0.5, colour  = "white") +
  scale_fill_viridis_c()