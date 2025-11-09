library(tidyverse)
library(readxl)
library(sf)
library(potential)
library(patchwork)

communes_paris <- st_read("../data/Maillages_Limitrophes_Paris.gpkg",
                          layer = "Communes", as_tibble = TRUE) %>%
  st_transform(2154) %>%
  filter(INSEE_DEP == "75", INSEE_COM != "75056")
contour_paris <- communes_paris %>% summarise() %>% st_buffer(500)
bureauxVote <- st_read("../data/BureauxVote_Paris_L93.gpkg", as_tibble = TRUE) %>%
  st_transform(2154)
resultatsVote <- readxl::read_xlsx("../data/BureauxVote_Paris_Resultats_Presi2022_T1.xlsx") %>%
  select(-c(1:6))

resultats_bureaux <- bureauxVote %>%
  left_join(resultatsVote, by = c("CodeBVote" = "Code du b.vote")) %>%
  mutate(Inscrits = as.numeric(Inscrits))
