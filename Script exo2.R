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

#Uploead de la bdd
library(readr)
Données <- read_delim("C:\\Users\\videt\\OneDrive\\Bureau\\Fac\\M2\\Exercice 1\\Données\\Données.csv", 
                      delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                      trim_ws = TRUE)

#Duplication de la bdd
d<-Données
#SAuvegarde en format R
save(d, file = "Exercice2.RData")

#r
str(d)

## Sexe d’un vecteur character à un facteur

d$SEXE <- factor(d$`Tu es de sexe :`)

var_label(d$SEXE)<-"Sexe"
d$SEXE[d$SEXE=="Autres"]<-"Autre"
d$SEXE <- droplevels(d$SEXE)

describe (d$SEXE)

table(d$SEXE)

tbl_summary

##Age d’un vecteur character à un facteur

d$Age_f<-factor(d$`Quel âge as-tu?`)
var_label(d$Age_f)<-"Age"
table(d$Age_f)
describe (d$Age_f)
d$Age_f[d$Age_f=="Entrer 25 et 29 ans"]<-"Entre 25 et 29 ans"

## Lieu d'étude d’un vecteur character à un facteur
d$Etude_f<-factor(d$`Dans quel établissement es-tu scolarisé?`)
var_label(d$Etude_f)<-"Etude"
describe(d$Etude_f)

table(d$Etude_f)

##Recodage de Lycée - filière technologique avec Lycée technologique
d$Etude_f_r<-d$Etude_f
d$Etude_f_r[d$Etude_f=="Lycée - filière technologique"]<-"Lycée technologique"
d$Etude_f_r <- droplevels(d$Etude_f_r)
table(d$Etude_f_r)

##Recodage de Lycée général avec Lycée - filière générale 
d$Etude_f_r[d$Etude_f=="Lycée - filière générale"]<-"Lycée général"
d$Etude_f_r <- droplevels(d$Etude_f_r)
table(d$Etude_f_r)

var_label(d$Etude_f_r)<-"Etablissement"

#Transmormer Projets en variable binomiale 
d$Projet_f<-factor(d$`A quel(s) type(s) de projet as-tu participé?`)
var_label(d$Projet_f)<-"Prjt"
table(d$Projet_f)
## Regroupement des Projets ayant une dimension Deeptech en 1 seul modalité (1)
d$Projet_f_r[d$Projet_f %in% c("Projet de développement technologique, scientifique ou artistique","Projet de développement technologique, scientifique ou artistique, Projet de création de podcast","Projet de développement technologique, scientifique ou artistique, Projet de création de startup", "Projet de développement technologique, scientifique ou artistique, Projet de développement sur des aspects business","Projet de développement technologique, scientifique ou artistique, Projet de développement sur des aspects business, Projet de création de startup","Projet de développement technologique, scientifique ou artistique, Projet de développement sur des aspects business, Projet de recherche en petit groupe: TPE ou TIPE, Projet de création de startup","Projet de développement technologique, scientifique ou artistique, Projet de recherche en petit groupe: TPE ou TIPE"   )]<-1
## Regroupement des Projets n'ayant pas une dimension Deeptech en 1 seul modalité (2)
d$Projet_f_r[d$Projet_f %in% c("Projet de recherche en petit groupe: TPE ou TIPE","TPE ou TIPE", "Projet de création de podcast", "Projet de création de startup", "Projet de création de startup, Projet de création de podcast", "Projet de développement axé marketing et stratégie", "Projet de développement axé marketing et stratégie, Projet de création de startup", "Projet de développement axé marketing et stratégie, TPE ou TIPE, Projet de création de startup", "Projet de développement sur des aspects business", "Projet de développement sur des aspects business, Projet de création de podcast", "Projet de développement sur des aspects business, Projet de création de startup")]<-2
var_label(d$Projet_f_r)<-"Deeptech"

d$Projet_f_r <- factor(d$Projet_f_r,
                       levels = c(1, 2),
                       labels = c("Deeptech", "Classique"))
var_label(d$Projet_f_r)


describe(d$Age_f)


str(d)

##Faire en sorte que les variables passent de numérique à catégorielle ordinale

# 1. Définir la plage des colonnes

mes_colonnes <- 8:18  

# 2. Appliquer la transformation sur toutes ces colonnes en une fois
d[, mes_colonnes] <- lapply(d[, mes_colonnes], function(x) {
  factor(x,
         levels = c(1, 2, 3, 4, 5),
         ordered = TRUE)
})
# 3. Vérifier que ça a marché
str(d)
describe(d$`Tu as eu l'occasion de mettre en pratique des connaissances et compétences travaillées en classe.`)
describe(d$Age_f)

d$Etude_f<-NULL
d$`Suite au programme, tu es plus familier avec la notion d'innovation.`<-NULL

# Création du vecteur de noms : "A1", "A2", ..., "A11"
nouveaux_noms <- paste0("A", 1:11)

# Application aux colonnes 8 à 18
colnames(d)[8:18] <- nouveaux_noms

describe(d$A1)
describe(d$A2)

var_label(d$A1)<-"Explication des notions scientifiques"
var_label(d$A2)<-"Activité pédagogiques"
var_label(d$A3)<-"Médiateur à l'écoute"
var_label(d$A4)<-"Confiance"
var_label(d$A5)<-"travaille en équipe"
var_label(d$A6)<-"à l'aise"
var_label(d$A7)<-"analyse et recherche"
var_label(d$A8)<-"créativité"
var_label(d$A9)<-"mise en pratique de cours"
var_label(d$A10)<-"Connaissances"
var_label(d$A11)<-"Compréhension du monde professionnel"

##Création de la base de donnée propre
e<-select(d,SEXE,Age_f,Etude_f_r,Projet_f_r,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)

e$SEXE[e$SEXE=="Autres"]<-"Autre"
e$SEXE <- droplevels(e$SEXE)
table(e$SEXE)


#Analyse univariée 

e %>% tbl_summary(include = c("SEXE","Age_f"))
e %>% tbl_summary(include = c("Etude_f_r","Projet_f_r"))
e %>% tbl_summary(include = c("Etude_f_r","Projet_f_r"))

e %>% tbl_summary(include = c(A1:A11))

#Analyse Bivariée 

table1<-table (e$Projet_f_r,e$Etude_f_r)
lprop(table1)


e %>%
  tbl_summary(
    by = Projet_f_r,                
    include = SEXE,   
    percent = "column"              
  ) %>%
  add_p()                           #  Chi-2







#regression 

m1 <- polr(A10 ~ Projet_f_r + SEXE + Age_f + Etude_f_r, data = e, Hess = TRUE)

##Erreur le plan semble pas de rang plein

##On regroupe ecole de commerce et ecole primaire et lycée pro dans autre
e$Etude_f_r[e$Etude_f_r %in% c("École de commerce", "Ecole primaire","Lycée professionnel","Université") ]<-"Autre"
e$Etude_f_r <- droplevels(e$Etude_f_r)
table(e$Etude_f_r)

table(e$Projet_f_r, e$Etude_f_r)




#  cbind() pour les grouper
test_global <- manova(cbind(A4, A5, A6, A7, A8, A10, A11) ~ Projet_f_r, data = e)

summary(test_global)



# 1.  MANOVA en un tableau de donnée propre
tableau_manova <- tidy(test_global)

datasummary_df(tableau_manova, 
               title = "Résultats du test MANOVA (Deeptech vs Classique)",
               output = "default") 



# On définit "Lycée général" comme la nouvelle base de comparaison
e$Etude_f_r <- relevel(e$Etude_f_r, ref = "Lycée général")

# On définit "classique" comme la nouvelle base de comparaison
e$Projet_f_r <- relevel(as.factor(e$Projet_f_r), ref = "Classique")

e_clean$SEXE <- relevel(e_clean$SEXE, ref = "Féminin")

# On lance une régression pour chaque variable à expliquer afin d'etudier celles qui sont le plus significative
##A4, A5, A6, A7, A8, A10, A11
m4 <- polr(A4 ~ Projet_f_r + SEXE + Etude_f_r, data = e_clean, Hess = TRUE)
m5 <- polr(A5 ~ Projet_f_r + SEXE + Etude_f_r, data = e_clean, Hess = TRUE)
m6 <- polr(A6 ~ Projet_f_r + SEXE + Etude_f_r, data = e_clean, Hess = TRUE)
m7 <- polr(A7 ~ Projet_f_r + SEXE + Etude_f_r, data = e_clean, Hess = TRUE)
m8 <- polr(A8 ~ Projet_f_r + SEXE + Etude_f_r, data = e_clean, Hess = TRUE)
m10 <- polr(A10 ~ Projet_f_r + SEXE + Etude_f_r, data = e_clean, Hess = TRUE)
m11 <- polr(A11 ~ Projet_f_r + SEXE + Etude_f_r, data = e_clean, Hess = TRUE)


#2. On crée une liste nommée pour que le tableau ait de jolis titres
mes_modeles <- list(
  "Confiance (A4)" = m4,
  "travaille en équipe (A5)" = m5,
  "Expression (A6)"= m6,
  "Analyse (A7)"   = m7,
  "Créativité (A8)"  = m8,
  "Savoirs (A10)"  = m10,
  "Monde Pro (A11)"= m11
)

# 3. On génère le tableau récapitulatif
# exponentiate = TRUE : Affiche les Odds Ratios (plus facile à lire : >1 = positif, <1 = négatif)
# stars = TRUE : Ajoute les étoiles de significativité (* p<0.05, ** p<0.01, *** p<0.001)

modelsummary(mes_modeles, 
             exponentiate = TRUE, 
             stars = TRUE,
             title = "Regression logistique ordinale")


levels(e$Projet_f_r)


## il y a bien une différence quand le projet est deeptech pour 4 7 10 11, regardons mtn qu'est ce qui fait 
  ## des projets deeptechs des projets différents 

e %>%
  tbl_summary(
    by = Projet_f_r,
    include = c(A1, A2, A3, A9), 
    percent = "column"
  ) %>%
  add_p()

describe(e$A1)


##on garde ducoup A2 et A9 qui sont significatifs et on regarde leurs effet quand on les incorpores dans les regressions

m4_2 <- polr(A4 ~ Projet_f_r + SEXE + Etude_f_r + A2, data = e, Hess = TRUE)
m7_2 <- polr(A7 ~ Projet_f_r + SEXE + Etude_f_r + A2, data = e, Hess = TRUE)
m10_2 <- polr(A10 ~ Projet_f_r + SEXE + Etude_f_r + A2, data = e, Hess = TRUE)
m11_2 <- polr(A11 ~ Projet_f_r + SEXE + Etude_f_r + A2, data = e, Hess = TRUE)


mes_modeles_2 <- list(
  "Confiance (A4)" = m4_2, "Analyse (A7)" = m7_2,"Savoirs (A10)" = m10_2,
  "Monde Pro (A11)"= m11_2
)

modelsummary(mes_modeles_2, 
             exponentiate = TRUE, 
             stars = TRUE,
             title = "Regression logistique ordinale")



m4_3 <- polr(A4 ~ Projet_f_r + SEXE + Etude_f_r + A9, data = e, Hess = TRUE)
m7_3 <- polr(A7 ~ Projet_f_r + SEXE + Etude_f_r + A9, data = e, Hess = TRUE)
m10_3 <- polr(A10 ~ Projet_f_r + SEXE + Etude_f_r + A9, data = e, Hess = TRUE)
m11_3 <- polr(A11 ~ Projet_f_r + SEXE + Etude_f_r + A9, data = e, Hess = TRUE)

mes_modeles_3 <- list(
  "Confiance (A4)" = m4_3, "Analyse (A7)" = m7_3,"Savoirs (A10)" = m10_3,
  "Monde Pro (A11)"= m11_3
)

modelsummary(mes_modeles_3, 
             exponentiate = TRUE, 
             stars = TRUE,
             title = "Regression logistique ordinale")


str(e)
str(z_final)
str(r)
str(f_complete)
