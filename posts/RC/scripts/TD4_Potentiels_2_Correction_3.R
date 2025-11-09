grille_analyse <- create_grid(x = communes_paris, res = 200)
matrice_distance_euclidienne <- create_matrix(x = resultats_bureaux, y = grille_analyse)

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