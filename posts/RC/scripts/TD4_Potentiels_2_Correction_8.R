colonnes_resultats <- grille_resultats %>% st_drop_geometry() %>%
  select(contains("pctMACRON")) %>% colnames()
plot_list <- list()
for (this_result in colonnes_resultats){
  equipot_macron <- equipotential(x = grille_resultats, var = this_result, nclass = 6) %>%
    st_make_valid()
  equipot_macron_cropped <- st_intersection(equipot_macron, contour_paris)
  beta <- str_split(this_result, pattern = "_", simplify = TRUE)[2]
  span <- str_split(this_result, pattern = "_", simplify = TRUE)[3]
  this_plot <- ggplot(equipot_macron_cropped) +
    geom_sf(aes(fill = center)) +
    geom_sf(data = communes_paris, fill = NA, colour = "grey80", lwd = 0.2) +
    scale_fill_steps(n.breaks = 7, low = "white", high = "#ffeb00", guide = NULL) +
    theme_minimal()+
    theme(axis.ticks = element_blank(), axis.text = element_blank(), panel.grid = element_blank()) +
    labs(subtitle = glue::glue("Span : {span} / Beta : {beta}"))
  plot_list[[this_result]] <- this_plot
}
patchwork::wrap_plots(plot_list, ncol = 5)
