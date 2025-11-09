ggplot(grille_resultats) +
  geom_sf(aes(colour = ARTHAUD)) +
  geom_sf(data = communes_paris, fill = NA, colour = "grey80", lwd = 0.5)

equipot_macron <- equipotential(x = grille_resultats, var = "MACRON",
                                nclass = 6, mask = communes_paris)

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