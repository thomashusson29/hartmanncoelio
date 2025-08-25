# Script pour créer un love plot modifié avec une seule catégorie d'âge
# Nécessite les packages cobalt et MatchIt

library(cobalt)
library(MatchIt)

# Créer une copie du modèle d'appariement pour modification
# Extraire les données de balance de l'objet matchit
bal_data <- bal.tab(m.out, un = TRUE)

# Créer une copie de la table de balance pour modification
bal_data_modified <- bal_data

# Identifier les lignes à conserver (tout sauf les autres catégories d'âge)
# On supprime Age_cat_50-60, Age_cat_60-70, et Age_cat_>70
rows_to_keep <- !rownames(bal_data_modified$Balance) %in% c("Age_cat_50-60", "Age_cat_60-70", "Age_cat_>70")

# Filtrer les données pour ne garder que les lignes souhaitées
bal_data_modified$Balance <- bal_data_modified$Balance[rows_to_keep, ]

# Renommer Age_cat_<50 en Age pour simplifier l'affichage
rownames(bal_data_modified$Balance)[rownames(bal_data_modified$Balance) == "Age_cat_<50"] <- "Age"

# Créer le love plot modifié
loveplot <- love.plot(bal_data_modified,
          var.order = "alphabetical",      # Ordre des variables alphabétique
          threshold = 0.1,                 # Ajouter une ligne de seuil à 0.1
          stat = "mean.diffs",            # Type de statistique à afficher (différences moyennes standardisées)
          grid = TRUE,                     # Afficher la grille
          line = TRUE,                     # Afficher les lignes entre les points
          stars = "raw",                   # Afficher les astérisques pour les variables binaires
          abs = TRUE,                      # Afficher les valeurs absolues des différences moyennes standardisées
          title = "Love Plot - Balance des covariables",
          sample.names = c("Non apparié", "Apparié"),
          colors = c("#db1d88", "#4b6bd6"))# Couleurs pour les groupes 
+theme(legend.text = element_text(size = 16))

loveplot
# Note: Ce script suppose que l'objet m.out (résultat de matchit) est déjà présent dans votre environnement R








loveplot <- love.plot(bal_data_modified,
                      var.order     = "alphabetical",
                      threshold     = 0.1,
                      stat          = "mean.diffs",
                      grid          = TRUE,
                      line          = TRUE,
                      stars         = "raw",
                      abs           = TRUE,
                      title         = "Love Plot - Balance des covariables",
                      sample.names  = c("Non apparié", "Apparié"),
                      colors        = c("#db1d88", "#4b6bd6")
) + 
  theme(
    axis.text.y = element_text(size = 14)  # ajustez la taille à votre convenance
  )

loveplot
ggsave("loveplot.svg", plot = loveplot, width = 10, height = 6, dpi = 1000)



# Script pour créer un love plot modifié avec une seule catégorie d'âge et des noms en français
# Nécessite les packages cobalt et MatchIt

library(cobalt)
library(MatchIt)

# Créer une copie du modèle d'appariement pour modification
# Extraire les données de balance de l'objet matchit
bal_data <- bal.tab(m.out, un = TRUE)

# Créer une copie de la table de balance pour modification
bal_data_modified <- bal_data

# Identifier les lignes à conserver (tout sauf les autres catégories d'âge)
# On supprime Age_cat_50-60, Age_cat_60-70, et Age_cat_>70
rows_to_keep <- !rownames(bal_data_modified$Balance) %in% c("Age_cat_50-60", "Age_cat_60-70", "Age_cat_>70")

# Filtrer les données pour ne garder que les lignes souhaitées
bal_data_modified$Balance <- bal_data_modified$Balance[rows_to_keep, ]

# Créer un vecteur avec les noms actuels
new_names <- rownames(bal_data_modified$Balance)

# Renommer toutes les variables en français
new_names[new_names == "Age_cat_<50"] <- "Âge"
new_names[new_names == "Sexe_Male"] <- "Sexe"
new_names[new_names == "BMI"] <- "IMC"
new_names[new_names == "A_ASA_sup2"] <- "Score ASA"
new_names[new_names == "A_tabac"] <- "Tabac"
new_names[new_names == "A_immunosuppression"] <- "Immunodépression"
new_names[new_names == "I_arret_ATC"] <- "Anticoagulation curative"
new_names[new_names == "A_ATCD_laparo_mediane"] <- "ATCD de laparotomie"
new_names[new_names == "I_scoreHinchey_4"] <- "Stade de Hinchey"
# La variable "distance" reste inchangée

# Appliquer les nouveaux noms
rownames(bal_data_modified$Balance) <- new_names

# Créer le love plot final avec tous les noms modifiés
loveplot2 <- love.plot(bal_data_modified,
          var.order = "alphabetical",      # Ordre des variables alphabétique
          threshold = 0.1,                 # Ajouter une ligne de seuil à 0.1
          stat = "mean.diffs",            # Type de statistique à afficher (différences moyennes standardisées)
          grid = TRUE,                     # Afficher la grille
          line = TRUE,                     # Afficher les lignes entre les points
          stars = "raw",                   # Afficher les astérisques pour les variables binaires
          abs = TRUE,                      # Afficher les valeurs absolues des différences moyennes standardisées
          title = "Love Plot - Balance des covariables",
          sample.names = c("Non apparié", "Apparié"),
          colors = c("#db1d88", "#4b6bd6"),  # Vos couleurs personnalisées + 
  theme(
    axis.text.y = element_text(size = 14)  # ajustez la taille à votre convenance
  ))     # Taille de la légende augmentée

# Note: Ce script suppose que l'objet m.out (résultat de matchit) est déjà présent dans votre environnement R
