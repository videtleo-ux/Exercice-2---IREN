
library(lubridate)

# 1. CRÉATION DES DICTIONNAIRES 
profs_niv1_lisse <- f_complete %>%
  group_by(Mois_Annee, Structure, ET2, PROJ, DEE) %>%
  filter(n_distinct(MAT2, na.rm = TRUE) == 1) %>% 
  summarise(
    B1_1 = mean(as.numeric(B1), na.rm = TRUE),
    B2_1 = mean(as.numeric(B2), na.rm = TRUE),
    MAT_1 = first(MAT2), 
    .groups = "drop"
  )

profs_niv3_lisse <- f_complete %>%
  mutate(Date_H = as.Date(Horodateur)) %>%
  filter(Date_H >= as.Date("2020-06-17") & Date_H <= as.Date("2022-12-06")) %>%
  group_by(Structure, ET2, PROJ, DEE) %>%
  filter(n_distinct(MAT2, na.rm = TRUE) == 1) %>%
  summarise(
    B1_3 = mean(as.numeric(B1), na.rm = TRUE),
    B2_3 = mean(as.numeric(B2), na.rm = TRUE),
    MAT_3 = first(MAT2), 
    .groups = "drop"
  )

# 2. MATCHING ET FILTRAGE CIBLÉ
base_finale_lissement <- e_final %>%
  left_join(profs_niv1_lisse, by = c("Mois_Annee", "Structure", "ET2", "PROJ", "DEE")) %>%
  left_join(profs_niv3_lisse, by = c("Structure", "ET2", "PROJ", "DEE")) %>%
  
  mutate(
    Prof_B1_num = case_when(
      !is.na(B1_1) ~ B1_1,
      is.na(Horodateur) & !is.na(B1_3) ~ B1_3,
      TRUE ~ NA_real_
    ),
    Prof_B2_num = case_when(
      !is.na(B2_1) ~ B2_1,
      is.na(Horodateur) & !is.na(B2_3) ~ B2_3,
      TRUE ~ NA_real_
    ),
    Prof_MAT = case_when(
      !is.na(MAT_1) ~ as.character(MAT_1),
      is.na(Horodateur) & !is.na(MAT_3) ~ as.character(MAT_3),
      TRUE ~ NA_character_
    )
  ) %>%
  
  # LE FILTRE EXACT : On exige un professeur (MAT2 existant), on tolère les NA sur B1/B2
  filter(!is.na(Prof_MAT)) %>%
  
  dplyr::select(
    Horodateur, Mois_Annee, Structure, SEXE, Age, ET2, DEE, PROJ, 
    A2, A4, A7, A9, A10, A11, 
    Prof_B1_num, Prof_B2_num, Prof_MAT
  ) %>%
  
  mutate(
    across(c(Structure, SEXE, ET2, DEE, PROJ, Prof_MAT), as.factor),
    across(c(A2, A4, A7, A9, A10, A11), ~ factor(.x, levels = 1:5, ordered = TRUE)),
    Prof_B1 = factor(round(Prof_B1_num), levels = 1:5, ordered = TRUE),
    Prof_B2 = factor(round(Prof_B2_num), levels = 1:5, ordered = TRUE)
  )

# 3. DIAGNOSTIC

cat("\n--- RÉSULTAT DU MATCHING (FILTRE APPLIQUÉ) ---\n")
cat("Élèves conservés (Match réussi) :", nrow(base_finale_lissement), "\n")
cat("Dont élèves avec Prof_B1 manquant (NA tolérés) :", sum(is.na(base_finale_lissement$Prof_B1)), "\n")
cat("Dont élèves avec Prof_B2 manquant (NA tolérés) :", sum(is.na(base_finale_lissement$Prof_B2)), "\n")


str(base_finale_lissement)


#Duplication de la bdd
r<-base_finale_lissement
#SAuvegarde en format R
save(r, file = "BaseReg.RData")


r$ET2[r$ET2 %in% c("BTS", "Autres") ]<-"Autres"
r$ET2[r$ET2 %in% c("Collège", "Lycée technologique", "Lycée_Général") ]<-"Secondaire"
r$ET2 <- droplevels(r$ET2)
table(r$ET2)






r <- r %>%
  mutate(ET3 = case_when(
    ET2 %in% c("BTS", "Autres") ~ "Autres",
    ET2 %in% c("Collège", "Lycée technologique", "Lycée_Général") ~ "Secondaire",
    TRUE ~ as.character(ET2) # Sécurité pour garder les valeurs imprevues
  )) %>%
  # On transforme le résultat en facteur
  mutate(ET3 = as.factor(ET3))

# Vérification immédiate
table(r$ET3)






table(e$Projet_f_r, e$Etude_f_r)