#Duplication de la bdd
p<-base_startups_ecole_finale
#SAuvegarde en format R
save(p, file = "Exercice2Prof.RData")

#r
str(p)

library(readr)      
library(dplyr)      # Pour le pipe (%>%), select, et manipulation générale
library(labelled)   # Pour la fonction var_label
library(Hmisc)      # Pour la fonction describe
library(questionr)  # Pour la fonction lprop (tableaux croisés % ligne/colonne)
library(gtsummary)  # Pour la fonction tbl_summary
library(modelsummary) # Pour la fonction modelsummary (résultats de régression)
library(MASS)       # Pour la fonction polr (régression logistique ordinale)
library(broom)      ## 
library(car)

library(lubridate)


##Age d’un vecteur character à un facteur

p$Age<-factor(p$`Quel âge avez-vous?`)
var_label(p$Age)<-"Age"

p$SEXE<-factor(p$`Vous êtes :`)
var_label(p$SEXE)<-"Sexe"


p$MAT<-factor(p$`Quelle matière enseignez-vous?`)
var_label(p$MAT)<-"Matière"

p$ET<-factor(p$`Les élèves qui ont participé au programme sont issus de quel établissement?`)
var_label(p$ET)<-"Etablissement"

table(p$Age)
table(p$SEXE)
table(p$MAT)

# Création de la nouvelle classification
p$MAT2 <- fct_collapse(p$`MAT`,
                                "STEM"                = c("Technologie", "SVT", "Système d'Information", "Mathématiques"),
                                "Business_SES"        = c("Marketing", "Management", "SES", 
                                                          "Ressources humaines", "Ressources Humaines", 
                                                          "Développement Commercial International"),
                                "Autres"              = c("Autre", "Français", "Langues vivantes", "Responsable de l'école")
)

# Vérification rapide des effectifs
table(p$MAT2)

p$ET2 <- NULL


table(p$ET)


# 1. On crée la nouvelle colonne ET2 à partir de votre colonne actuelle
# (Remplacez 'NOM_COLONNE' par le nom réel de votre colonne, ex: p$ET)
p$ET2 <- fct_collapse(p$ET,
                      "Autres" = c("Autre", "Ecole de commerce", "Lycée professionnel", "Université"),
                      "BTS" = "BTS",
                      "Collège" = "Collège",
                      "Lycée_Général" = "Lycée général",
                      "Lycée technologique" = "Lycée technologique"
)

# 2. Vérification du résultat
table(p$ET2)




p$PROJ<-factor(p$`A quel(s) type(s) de projet avez-vous participé?`)
var_label(p$PROJ)<-"Type de Projet"

table(p$PROJ)

p$DEE <- NULL

p$DEE <- fct_collapse(p$PROJ,
                      "Deeptech" = c("Projet de développement technologique, scientifique ou artistique","Projet de développement technologique, scientifique ou artistique, Projet de création de podcast","Projet de développement technologique, scientifique ou artistique, Projet de développement sur des aspects business"),
                      "Classique" = c('Projet de création de podcast',"Projet de création de startup","Projet de développement axé marketing et stratégie","Projet de développement sur des aspects business","Projet de développement sur des aspects business, Projet de création de podcast","Projet de développement sur des aspects business, Projet de création de startup","Projet de recherche en petit groupe: TPE ou TIPE","Projet de recherche en petit groupe: TPE ou TIPE","TPE ou TIPE"),
)
table(p$DEE)



##Faire en sorte que les variables passent de numérique à catégorielle ordinale

library(dplyr)

# On définit les colonnes par leurs numéros
cols_to_fix <- c(8, 10, 12, 13, 15:29)

# On transforme avec mutate et across
p <- p %>%
  mutate(across(all_of(cols_to_fix), ~ factor(.x, levels = c(1, 2, 3, 4, 5), ordered = TRUE)))

# VERIFICATION : on regarde juste la colonne 8 et 15
class(p[[8]])
class(p[[15]])


table(p$`Combien d'heures avez-vous passées à préparer le projet (si vous avez participé à plusieurs projets, donnez nous une moyenne par projet)?`)


library(dplyr)
library(labelled)

# 1. Renommage et Labellisation en un seul bloc
p <- p %>%
  rename(
    B1  = `Diriez-vous qu'avant de participer aux Startups à l'Ecole vous connaissiez bien le monde de l'entreprise?`,
    B2  = `Avez-vous eu le sentiment d'être bien accompagné pendant l'organisation amont et le déroulement du (des) projet(s)?`,
    B3  = `Il vous a été facile d'aménager le temps consacré au(x) projet(s) au sein de vos cours.`,
    B4  = `Vous avez eu des difficultés à obtenir l'accord de l'administration de votre établissement pour organiser les sorties réalisées dans le cadre du programme.`,
    B5  = `L'expérience a changé positivement votre perception sur l'entrepreneuriat et sur le monde professionnel.`,
    B6  = `Grâce au programme vous avez l'impression de pouvoir mieux aider vos élèves à s'insérer dans le monde professionnel.`,
    B7  = `L'expérience vous a donné envie de travailler différemment avec vos élèves.`,
    B8  = `Lors des interventions de la (des) startup(s) et du médiateur, vous avez trouvé que les notions techniques ou scientifiques étaient clairement expliquées.`,
    B9  = `Vous avez trouvé que les activités proposées étaient pédagogiques et interactives.`,
    B10 = `Vous avez trouvé que le médiateur était à l'écoute des besoins de vos élèves et savait s'adapter.`,
    B11 = `Grâce au programme, vos élèves ont plus confiance en leurs capacités.`,
    B12 = `Vos élèves ont développé leur capacité à travailler en équipe.`,
    B13 = `Vos élèves sont plus à l'aise pour exprimer leurs idées.`,
    B14 = `Vos élèves ont développé leur capacité à analyser une problématique et à rechercher des informations pour y répondre.`,
    B15 = `Vos élèves ont développé leur créativité.`,
    B16 = `Vos élèves ont eu l'occasion de mettre en pratique des connaissances et compétences travaillées en classe.`,
    B17 = `Grâce au programme, vos élèves ont acquis des connaissances`,
    B18 = `Vos élèves connaissent mieux le monde professionnel et en particulier celui des startups.`,
    B19 = `Vous avez le sentiment que le programme a renforcé la dynamique de travail et la motivation de la classe.`
  ) %>%
  set_variable_labels(
    B1  = "Connaissance entreprise avant",
    B2  = "Sentiment accompagnement",
    B3  = "Facilité aménagement temps",
    B4  = "Difficulté accord administration",
    B5  = "Changement perception entrepreneuriat",
    B6  = "Aide insertion élèves",
    B7  = "Envie travailler différemment",
    B8  = "Clarté notions techniques",
    B9  = "Activités pédagogiques",
    B10 = "Médiateur à l'écoute",
    B11 = "Confiance élèves",
    B12 = "Travail en équipe élèves",
    B13 = "Expression idées élèves",
    B14 = "Capacité analyse élèves",
    B15 = "Créativité élèves",
    B16 = "Mise en pratique connaissances",
    B17 = "Acquisition connaissances",
    B18 = "Connaissance monde startups",
    B19 = "Dynamique et motivation classe"
  )

# 2. Vérification
look_for(p, "B") # Affiche les noms courts et les labels associés


library(dplyr)

library(dplyr)

library(dplyr)

# Re-création de la base f avec B1 et B2 uniquement
f <- p %>%
  dplyr::select(
    Horodateur,
    Age,
    SEXE,
    MAT2,
    ET2,
    PROJ,
    DEE,
    Structure = `Quel est le nom de la structure qui a coordonné le programme?`,
    B1, 
    B2
  )

# Vérification du résultat
str(f)

str(p)

















