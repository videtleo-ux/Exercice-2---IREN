library(dplyr)

# 1. UNIFORMISATION DE LA BASE ÉLÈVES (e_final)
e_final <- e_final %>%
  mutate(
    # On passe tout en texte et on coupe les espaces (avec trimws)
    Structure = trimws(as.character(Structure)),
    PROJ      = trimws(as.character(PROJ)),
    DEE       = trimws(as.character(DEE)),
    ET2       = trimws(as.character(ET2)),
    
    # On corrige les différences de texte pour l'établissement
    ET2 = case_when(
      ET2 == "Autre" ~ "Autres",
      ET2 == "Lycée général" ~ "Lycée_Général",
      TRUE ~ ET2 # On laisse le reste intact
    )
  )

# 2. UNIFORMISATION DE LA BASE PROFS (f_complete)
f_complete <- f_complete %>%
  mutate(
    # On fait le même nettoyage de texte côté profs
    Structure = trimws(as.character(Structure)),
    PROJ      = trimws(as.character(PROJ)),
    DEE       = trimws(as.character(DEE)),
    ET2       = trimws(as.character(ET2))
  )

# Le Crash-Test !
crash_test <- anti_join(e_final, f_complete, by = c("Structure", "ET2", "PROJ", "DEE"))
cat("Nombre d'élèves sans prof :", nrow(crash_test), "\n")

library(dplyr)

# On résume les profils des 77 élèves qui ont raté le test
profils_orphelins <- crash_test %>%
  count(Structure, ET2, PROJ, DEE) %>%
  arrange(desc(n)) # On trie du plus gros groupe au plus petit

# On affiche le résultat
print(profils_orphelins)




# ==========================================
# 2. LE MATCHING EN CASCADE (CORRIGÉ)
# ==========================================

# A. Profils des profs AVEC date (On arrondit pour garder l'entier !)
profs_avec_date <- f_complete %>%
  group_by(Mois_Annee, Structure, ET2, PROJ, DEE) %>%
  summarise(
    Prof_B1 = round(mean(as.numeric(B1), na.rm = TRUE)),
    Prof_B2 = round(mean(as.numeric(B2), na.rm = TRUE)),
    Prof_MAT = first(MAT2),
    .groups = "drop"
  )

# B. Profils des profs SANS date (Dictionnaire de secours)
profs_sans_date <- profs_avec_date %>%
  group_by(Structure, ET2, PROJ, DEE) %>%
  summarise(
    Prof_B1 = round(mean(Prof_B1, na.rm = TRUE)),
    Prof_B2 = round(mean(Prof_B2, na.rm = TRUE)),
    Prof_MAT = first(Prof_MAT),
    .groups = "drop"
  )

# C. CASCADE 1 : On tente le matching strict (Avec le mois)
etape1 <- left_join(e_final, profs_avec_date, by = c("Mois_Annee", "Structure", "ET2", "PROJ", "DEE"))

# D. On trie : ceux qui ont réussi, et ceux qui ont raté
eleves_sauves_strict <- etape1 %>% filter(!is.na(Prof_B1))
eleves_a_repecher <- etape1 %>% 
  filter(is.na(Prof_B1)) %>%
  select(-Prof_B1, -Prof_B2, -Prof_MAT) # On enlève les colonnes vides

# E. CASCADE 2 : On repêche avec le dictionnaire de secours
etape2 <- left_join(eleves_a_repecher, profs_sans_date, by = c("Structure", "ET2", "PROJ", "DEE"))


# ==========================================
# 3. CRÉATION DE LA BASE FINALE (AVEC B1 EN ORDINAL)
# ==========================================

base_regression_finale <- bind_rows(eleves_sauves_strict, etape2) %>%
  filter(!is.na(Prof_B1)) %>% 
  mutate(
    ET2 = as.factor(ET2),
    DEE = as.factor(DEE),
    SEXE = as.factor(SEXE),
    Prof_MAT = as.factor(Prof_MAT),
    
    # On remet les variables de l'élève en ordinal
    A10 = factor(A10, levels = 1:5, ordered = TRUE),
    
    # LA CORRECTION : On remet les variables du prof en facteurs ordonnés !
    Prof_B1 = factor(Prof_B1, levels = 1:5, ordered = TRUE),
    Prof_B2 = factor(Prof_B2, levels = 1:5, ordered = TRUE)
  )

# On vérifie la structure finale de tes variables B
str(base_regression_finale$Prof_B1)















