candidats <- resultats_bureaux %>% st_drop_geometry() %>%
  select(Inscrits, "ARTHAUD":"DUPONT-AIGNAN") %>% colnames()

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


