library(gtsummary)



# On inverse l'échelle pour que 5 soit le meilleur score (positif)
r <- r %>%
  mutate(across(c(A2, A4, A7, A9, A10, A11, Prof_B1, Prof_B2), ~ factor(8 - as.numeric(.x), levels = 1:5, ordered = TRUE)))

# Relance juste la régression A4 pour voir :
m4_test <- polr(A4 ~ DEE + SEXE + ET3 + Prof_B1_num + Prof_B2_num + Prof_MAT, data = r, Hess = TRUE)
summary(m4_test)




# Tableau descriptif général
base_finale_lissement %>%
  dplyr::select(SEXE, Age, ET2, DEE, Prof_MAT) %>%
  tbl_summary(missing = "no") %>%
  bold_labels()




# Répartition selon le type de projet (DEE)
base_finale_lissement %>%
  dplyr::select(DEE, A4, A7, A10, A11) %>%
  tbl_summary(by = DEE, percent = "column") %>%
  add_p()










table2<-table (base_finale_lissement$DEE,base_finale_lissement$ET2)
lprop(table2)


base_finale_lissement %>% tbl_summary(include = c("ET2","DEE"))


base_finale_lissement %>%
  tbl_summary(
    by = DEE,                
    include = ET2,   
    percent = "column"              
  ) %>%
  add_p()   








#  cbind() pour les grouper
test_global2 <- manova(cbind(A4, A7, A10, A11) ~ DEE, data = r)

summary(test_global2)


# 1.  MANOVA en un tableau de donnée propre
tableau_manova2 <- tidy(test_global2)

datasummary_df(tableau_manova2, 
               title = "Résultats du test MANOVA (Deeptech vs Classique)",
               output = "default") 





library(MASS)
library(modelsummary)

# 1. Définition des références (le "niveau de la mer")
r$DEE <- relevel(r$DEE, ref = "Classique")
r$ET3 <- relevel(r$ET3, ref = "Secondaire")

# 2. MODÈLES DE BASE (Effet Projet seul)
m4_base <- polr(A4 ~ DEE + SEXE + ET3, data = r, Hess = TRUE)
m7_base <- polr(A7 ~ DEE + SEXE + ET3, data = r, Hess = TRUE)
m10_base <- polr(A10 ~ DEE + SEXE + ET3, data = r, Hess = TRUE)
m11_base <- polr(A11 ~ DEE + SEXE + ET3, data = r, Hess = TRUE)


# --- TABLEAU 1 : LES MODÈLES DE BASE ---
list_base <- list(
  "Confiance (A4)"  = m4_base, 
  "Analyse (A7)"    = m7_base, 
  "Savoirs (A10)"   = m10_base, 
  "Monde Pro (A11)" = m11_base
)

modelsummary(list_base, 
             exponentiate = TRUE, 
             stars = TRUE,
             title = "Tableau 1 : Modèles de base (Impact brut du type de projet)")


# 3. MODÈLES COMPLETS (Effet Projet + Effet Professeurs)
m4_prof <- polr(A4 ~ DEE + SEXE + ET3 + Prof_B1_num + Prof_B2_num + Prof_MAT, data = r, Hess = TRUE)
m7_prof <- polr(A7 ~ DEE + SEXE + ET3 + Prof_B1_num + Prof_B2_num + Prof_MAT, data = r, Hess = TRUE)
m10_prof <- polr(A10 ~ DEE + SEXE + ET3 + Prof_B1_num + Prof_B2_num + Prof_MAT, data = r, Hess = TRUE)
m11_prof <- polr(A11 ~ DEE + SEXE + ET3 + Prof_B1_num + Prof_B2_num + Prof_MAT, data = r, Hess = TRUE)



# TABLEAU 2 : MODÈLES COMPLETS 
list_prof <- list(
  "Confiance (A4)"  = m4_prof, 
  "Analyse (A7)"    = m7_prof, 
  "Savoirs (A10)"   = m10_prof, 
  "Monde Pro (A11)" = m11_prof
)

modelsummary(list_prof, 
             exponentiate = TRUE, 
             stars = TRUE,
             title = "Tableau 2 : Modèles complets (Intégration des caractéristiques professeurs)")



# 4. TABLEAU COMPARATIF
mes_modeles_comparatifs <- list(
  "Conf (Base)" = m4_base, "Conf (+Prof)" = m4_prof,
  "Ana (Base)"  = m7_base, "Ana (+Prof)"  = m7_prof,
  "Sav (Base)"  = m10_base,"Sav (+Prof)"  = m10_prof,
  "Pro (Base)"  = m11_base,"Pro (+Prof)"  = m11_prof
)

modelsummary(mes_modeles_comparatifs, 
             exponentiate = TRUE, 
             stars = TRUE,
             title = "Régression Logistique Ordinale : Effet Projet vs Effet Professeur")










library(gtsummary)
library(dplyr)

# 1. Tableau pour la Confiance (A4)
tab_A4 <- r %>%
  select(DEE, A4) %>%
  tbl_summary(by = DEE, percent = "row", label = list(A4 ~ "Confiance en soi")) %>%
  add_p()

# 2. Tableau pour l'Analyse (A7)
tab_A7 <- r %>%
  select(DEE, A7) %>%
  tbl_summary(by = DEE, percent = "row", label = list(A7 ~ "Capacité d'analyse")) %>%
  add_p()

# 3. Tableau pour les Savoirs (A10)
tab_A10 <- r %>%
  select(DEE, A10) %>%
  tbl_summary(by = DEE, percent = "row", label = list(A10 ~ "Acquisition de savoirs")) %>%
  add_p()

# 4. Tableau pour le Monde Pro (A11)
tab_A11 <- r %>%
  select(DEE, A11) %>%
  tbl_summary(by = DEE, percent = "row", label = list(A11 ~ "Monde Professionnel")) %>%
  add_p()

# Affichage des résultats
tab_A4
tab_A7
tab_A10
tab_A11


str(e)
str(r)
