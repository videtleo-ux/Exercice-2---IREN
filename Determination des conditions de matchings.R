# On remplace les NA de la colonne Structure par "Agoranov"
f_complete <- f_complete %>%
  mutate(
    Structure = ifelse(is.na(Structure), "Agoranov", Structure)
  )

# Petite vérification pour être sûr qu'il n'y a plus de NA :
table(f_complete$Structure, useNA = "ifany")

str(e_final)
str(f_complete)
f_complete$B1[99:107] <- NA
f_complete$B2[99:107] <- NA


colnames(e_final)
colnames(f_complete)

colnames(base_startups_ecole_finale)
colnames(Données)


library(dplyr)

# 1. Calcul du nombre de professeurs par combinaison de clés
verification_cles <- f_complete %>%
  group_by(Mois_Annee, Structure, ET2, PROJ, DEE) %>%
  summarise(nombre_de_profs = n(), .groups = "drop")

# 2. Affichage de la distribution globale
cat("--- RÉPARTITION DU NOMBRE DE PROFESSEURS PAR PROJET ---\n")
print(table(verification_cles$nombre_de_profs))

# 3. Isolement et affichage des combinaisons problématiques (doublons)
cas_multiples <- verification_cles %>%
  filter(nombre_de_profs > 1) %>%
  arrange(desc(nombre_de_profs))

cat("\n--- DÉTAIL DES COMBINAISONS AVEC PLUSIEURS PROFESSEURS ---\n")
print(cas_multiples)



library(dplyr)

# On reprend nos cas multiples et on regarde ce qui se passe à l'intérieur
analyse_conflits <- f_complete %>%
  # On ne garde que les clés qui posent problème (les 16 groupes)
  inner_join(cas_multiples, by = c("Mois_Annee", "Structure", "ET2", "PROJ", "DEE")) %>%
  group_by(Mois_Annee, Structure, ET2, PROJ, DEE) %>%
  summarise(
    nombre_de_profs = first(nombre_de_profs),
    # On compte combien de matières différentes il y a dans le groupe
    nb_matieres_differentes = n_distinct(MAT2),
    # On liste les matières pour voir exactement le mélange
    liste_matieres = paste(unique(MAT2), collapse = " + "),
    .groups = "drop"
  ) %>%
  arrange(desc(nb_matieres_differentes), desc(nombre_de_profs))

print(analyse_conflits)











library(dplyr)
library(lubridate)

# 1. On isole les profs qui sont dans la bonne période
profs_periode_cible <- f_complete %>%
  mutate(Date_H = as.Date(Horodateur)) %>%
  filter(Date_H >= as.Date("2020-06-17") & Date_H <= as.Date("2022-12-06"))

# 2. On vérifie la pureté disciplinaire dans cette période
diagnostic_secours <- profs_periode_cible %>%
  group_by(Structure, ET2, PROJ, DEE) %>%
  summarise(
    nb_profs = n(),
    nb_matieres = n_distinct(MAT2, na.rm = TRUE),
    matieres_detectees = paste(unique(MAT2), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    Eligible_Secours = ifelse(nb_matieres == 1, "OUI", "NON (Conflit Matière)")
  )

# 3. Affichage du bilan
cat("--- BILAN DES GROUPES PROFS ÉLIGIBLES AU SECOURS (2020-2022) ---\n")
print(table(diagnostic_secours$Eligible_Secours))

cat("\n--- DÉTAIL DES GROUPES ÉLIGIBLES ---\n")
print(diagnostic_secours %>% filter(Eligible_Secours == "OUI"))













library(dplyr)

verification_doublons <- f_complete %>%
  group_by(Mois_Annee, Structure, ET2, PROJ, DEE) %>%
  summarise(nombre_de_profs = n(), .groups = "drop") %>%
  filter(nombre_de_profs > 1) %>%
  arrange(desc(nombre_de_profs))

print(verification_doublons)


