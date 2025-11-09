beta_var <- c(0, 0.5, 1 , 2, 4)
span_var <- c(100, 250, 500, 750, 1000)
grille_resultats <- grille_analyse

for (this_beta in beta_var){
  for (this_span in span_var){
    resultat_potentiels_sensib <- potential(
      x = resultats_bureaux, y = grille_analyse, d = matrice_distance_euclidienne,
      var = c("Inscrits", "MACRON"), fun = "e",  beta = this_beta, span = this_span
      ) %>%
      as_tibble() %>% mutate(pctMACRON = MACRON / Inscrits)
    
    resultat_potentiels_sensib_renamed <- resultat_potentiels_sensib %>%
      rename_with(~glue::glue("{.x}_{this_beta}_{this_span}"), contains("pctMACRON")) %>%
      select(contains("pctMACRON"))
    grille_resultats <- grille_resultats %>%
      bind_cols(resultat_potentiels_sensib_renamed)
  }
}