library(tidyverse)
library(readxl)
library(googlesheets4)

data <- read_csv("https://docs.google.com/spreadsheets/d/1PrKtU3OmTIrrzZVYfLwrBKFkPVmJ43Q01dc72Kzq4-M/export?format=csv") %>%
  mutate(across(c(Nb_Mailles:CV_Compte), ~str_replace(.x, ",", ".") %>% as.numeric(.)))

data %>%
  select(Type_Maille, Moyenne_Compte, Superficie_Moy_Maille_m2, StDev_Compte, `Décalage`) %>%
  filter(Type_Maille %in% c("Carrée", "Hexagonale", "Losange")) %>%
  filter(Superficie_Moy_Maille_m2 < 5E6) %>%
  filter(Moyenne_Compte < 100) %>%
  ggplot() +
  aes(Superficie_Moy_Maille_m2, Moyenne_Compte, group = `Décalage`, shape = `Décalage`, colour = Type_Maille) +
  geom_point() +
  #geom_smooth(se = FALSE, method = "lm" ) + 
#  facet_wrap(~Type_Maille, scales = "free") +
  NULL

data %>%
  pivot_longer(cols = c(Nb_Mailles, Moyenne_Compte, Mediane_Compte, StDev_Compte, CV_Compte)) %>%
  filter(name == "Moyenne_Compte") %>%
  filter(!Type_Maille %in% c("IRIS", "Voronoi")) %>%
  #filter(Type_Maille %in% c("Carrée")) %>%
  filter(Superficie_Moy_Maille_m2 < 5E6) %>%
  filter(Superficie_Moy_Maille_m2 < 500E3) %>%
  ggplot() +
  aes(Superficie_Moy_Maille_m2, value, colour = Décalage, group = Décalage) +
  geom_point(aes(colour = Décalage)) +
  geom_line(aes(group = Décalage)) +
  facet_grid(~Type_Maille ~ name) +
  #geom_smooth(se = FALSE) + 
  theme(strip.text = element_text(size = 12))
