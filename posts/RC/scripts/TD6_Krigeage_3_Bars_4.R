equipotential(x = krigeage_bars$krige_output, var = "var1.pred", nclass = 8, mask = paris) %>%
  mutate(center = fct_rev(as.factor(round(center, 2)))) %>%
  ggplot() +
  geom_sf(aes(fill = center)) +
  geom_sf(data = paris, fill = NA, colour = "white") +
  scale_fill_viridis_d(name = "Prix de la pinte [€]", direction = -1)