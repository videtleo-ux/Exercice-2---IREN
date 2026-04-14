#Duplication de la bdd
z<-PROFESSEURS_2023_2024
#SAuvegarde en format R
save(p, file = "Exercice2Rajoute.RData")

#r
str(z)

#factor

z$Age<-factor(z$`Quel âge avez-vous ?`)
var_label(z$Age)<-"Age"

z$SEXE<-factor(z$`Vous êtes :`)
var_label(z$SEXE)<-"Sexe"


z$MAT<-factor(z$`Quelle matière enseignez-vous ?`)
var_label(z$MAT)<-"Matière"

z$ET<-factor(z$`Les élèves qui ont participé au programme sont issus de quel établissement ?`)
var_label(z$ET)<-"Etablissement"

describe(z$`Quelle matière enseignez-vous ?`)

# Création de la nouvelle classification
z$MAT2 <- fct_collapse(z$`MAT`,
                       "STEM"                = c("Technologie"),
                       "Business_SES"        = c("Marketing, gestion et droit/economie", "Management", 
                                                 "Ressources humaines","DEVELOPPEMENT COMMERCIAL INTERNATIONAL"),
                       "Autres"              = c("Langues vivantes", "Communication")
)

describe(z$`Les élèves qui ont participé au programme sont issus de quel établissement ?`)


#proj et deeptech 
z$PROJ<-factor(z$`A quel type de projet avez-vous participé ?`)
var_label(z$PROJ)<-"Type de Projet"

z$DEE <- fct_collapse(z$PROJ,
                      "Deeptech" = c("Projet de développement technologique, scientifique ou artistique"),
                      "Classique" = c("Projet de création de startup","Projet de développement sur des aspects business"),
)

#ordinal 


# On définit les colonnes par leurs numéros
cols_to_fix2 <- c(9, 10)

# On transforme avec mutate et across
z <- z %>%
  mutate(across(all_of(cols_to_fix2), ~ factor(.x, levels = c(1, 2, 3, 4, 5), ordered = TRUE)))


# 1. Renommage et Labellisation en un seul bloc
z <- z %>%
  rename(
    B1  = `Diriez-vous qu'avant de participer au programme Startups à l'Ecole vous connaissiez bien le monde de l'entrepreneuriat ?`,
    B2  = `Avez-vous eu le sentiment d'être bien accompagné.e pendant l'organisation en amont du projet ?`,
  ) %>%
  set_variable_labels(
    B1  = "Connaissance entreprise avant",
    B2  = "Sentiment accompagnement", )

str(z)


# Création de la nouvelle base z_final à partir de z
z_final <- z %>%
  mutate(Structure = NA_character_) %>% # On crée la colonne vide
  dplyr::select(
    Horodateur, 
    Age, 
    SEXE, 
    MAT2, 
    ET2, 
    PROJ, 
    DEE, 
    Structure, # Elle est maintenant disponible ici
    B1, 
    B2
  )

# Vérification de la nouvelle base
str(z_final)


library(dplyr)

# On empile z_final en dessous de f
f_complete <- bind_rows(f, z_final)

# Vérification : le nombre de lignes doit être la somme des deux bases
nrow(f_complete) 

# Pour voir si les données des élèves sont bien à la fin
tail(f_complete)

str(f_complete)

# On crée e_final en combinant les colonnes de d et de e
# On utilise cbind si les deux bases ont exactement le même nombre de lignes (ce qui est ton cas)
e_final <- cbind(
  d %>% dplyr::select(Horodateur, 
                      Structure = `Quel est le nom de la structure qui a coordonné le programme?`),
  e
)
e_final$PROJ <- d$Projet_f


e_final <- e_final %>%
  rename(
    Age = Age_f,
    ET2 = Etude_f_r,
    DEE = Projet_f_r
  )

str(f_complete)
str(e_final)



# On transforme le texte en vraie date (mdy_hms = month, day, year, hour, minute, second)
e_final$Horodateur <- mdy_hms(e_final$Horodateur)

# Petite vérification pour être sûr que ça a marché :
str(e_final$Horodateur)



# 1. On crée la clé dans la base élèves
e_final <- e_final %>%
  mutate(Mois_Annee = format(Horodateur, "%Y-%m"))

# 2. On fait exactement la même chose dans la base profs 
# (utilise 'f' ou 'f_complete' selon le nom exact de ton objet actuel)
f_complete <- f_complete %>%
  mutate(Mois_Annee = format(Horodateur, "%Y-%m"))

# Petite vérification rapide
table(e_final$Mois_Annee)
table(f_complete$Mois_Annee)



