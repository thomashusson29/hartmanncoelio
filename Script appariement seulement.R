#tout supprimer
rm(list=ls())

##----- Chargement des librairies-----
# Installation des packages necessaires
install.packages(c(
  "cardx", "dplyr", "readxl", "openxlsx", "tidyverse", "gtsummary", 
  "magrittr", "ggplot2", "lubridate", "ggpubr", "survival", 
  "survminer", "summarytools", "MatchIt", "optmatch", 
  "officer", "flextable", "gt", "mice", "googlesheets4", 
  "RItools", "epiR", "tableone", "cobalt", "broom", "forcats", "dlstats", "pkgsearch", "pROC", "stats", "ResourceSelection", "forestplot"))

##----- Chargement des librairies-----
library(forcats)
library(cardx)
library(tidyverse)
library(openxlsx)
library(dplyr)
library(gtsummary)
library(magrittr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(survival)
library(survminer)
library(summarytools)
library(MatchIt)
library(optmatch)
library(flextable)
library(officer)
library(gt)
library(mice)
library(googlesheets4)
library(RItools)
library(epiR)
library(tableone)
library(cobalt)
library(broom)
library(dlstats)    # for package download stats
library(pkgsearch)  # for searching packages
library(pROC)
library(stats)
library(ResourceSelection)
library(forestplot)

##----- Analyses-----

# etape 1 : Importer toute la base
gs4_deauth()

df <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1PrWmbpaXgLFLBJ8eGCTdcPI0qEJhmxr6mXe4wylyQtY/edit?gid=531779098#gid=531779098",
  sheet = "base_filtrée_péritonites",
)

# Filtrer les patients avec une valeur renseignee pour 'I_geste_comparaison_1HartmannCoelio'
# et uniquement ceux operes en urgence (I_urgence == 1)
df_HartmannCoelio <- df[!is.na(df$I_geste_comparaison_1HartmannCoelio) & df$I_urgence == 1, ]

cols_to_include_in_table1 <- c(
  "Age",
  "Sexe_Male",
  "BMI",
  "A_ASA",
  "A_ASA_sup2",
  "A_ATCD_neuro",
  "A_ATCD_Cardio",
  "A_ATCD_pneumo",
  "A_diabete",
  "A_tabac",
  "A_immunosuppression",
  "A_anticoagulants",
  "A_antiaggregants",
  "A_ATCD_laparo_mediane",
  "H_ATCD_diverticulite",
  "H_n_pousse",
  "H_n_pousse_sup_equal_2",
  "H_ATCD_Divert_complique",
  "H_perforation",
  "H_abces",
  "H_taille_abces",
  "H_drainage_YN",
  "H_drainage_TDM",
  "H_drainage_endoscopie",
  "H_lavage_celio",
  "H_occlusion",
  "H_hemorragie",
  "H_fistule",
  "H_tt_medical",
  "H_antibio_YN",
  "H_antibio_IV",
  "H_antibio_PO",
  "H_antibio_IV_PO",
  "H_duree_antibio",
  "H_colo",
  "I_urgence",
  "I_voie_coelio",
  "I_conversion",
  "I_voie_laparo",
  "I_voie_robot",
  "I_indication_poussee_compliquee",
  "I_indication_immunodepression",
  "I_indication_hemorragie",
  "I_delai_jours_J0",
  "I_delai_jours_24h",
  "I_delai_jours_sup24h",
  "I_localisation",
  "I_localisation_colondroit",
  "I_localisation_sigmoide",
  "I_peritonite",
  "I_scoreHinchey",
  "I_scoreHinchey_3",
  "I_scoreHinchey_4",
  "I_occlusion",
  "I_hemorragie",
  "I_arret_ATC",
  "I_fistule"
)

cols_to_include_in_table2 <- c(
  "I_voie_coelio",
  "I_conversion",
  "I_voie_laparo",
  "I_voie_robot",
  "I_abaissement",
  "I_section_origine_AMI",
  "I_anastomose",
  "I_anastomose_terminoterminale",
  "I_anastomose_laterolaterale",
  "I_stomie",
  "I_drainage",
  "I_duree",
  "I_transfusion",
  "I_conversion",
  "I_culots"
)

# Definition des colonnes à inclure dans le tableau 3
cols_to_include_in_table3 <- c(
  "S_mortalite90d",
  "S_morbidite90d",
  "S_dindo",
  "S_dindo_sup2",
  "S_fistuleanasto",
  "S_abscesprof",
  "S_drainage",
  "S_complication_hemorragique",
  "S_transfusion",
  "S_Culots",
  "S_reoperation",
  "S_date_Reop_delai",
  "S_voie_Reop",
  "S_voie_Reop_coelio",
  "S_voie_Reop_coelio_conversion",
  "S_voie_Reop_laparo",
  "S_voie_Reop_robot",
  "S_geste_Reop",
  "S_geste_Reop_Hartmann",
  "S_geste_Reop_stomie_drainage",
  "S_geste_Reop_drainage",
  "S_geste_Reop_autres",
  "S_rea",
  "S_duree_hospit_postop",
  "S_complicationtardive"
)

# Definition des colonnes à inclure dans le tableau 4
cols_to_include_in_table4 <- c(
  "S_complicationtardive",
  "S_rehospitalisation",
  "S_stenoseanastomotique",
  "S_eventration",
  "S_occlusionpostop",
  "S_recidivediverti",
  "S_fermeturestomie",
  "S_duree_avec_stomie",
  "S_etatDDN_vivant"
)

##************Appariement selon score de propension*****
#analyse uni- puis multivariée pour identifier facteurs importants

#creer un nouveau df pour appariemment selon score de propension
dfps_HartmannCoelio <- df_HartmannCoelio

# 1️⃣ Conversion de la variable de traitement en binaire (0 = HartmannCoelio, 1 = RAlaparoProtegee)
dfps_HartmannCoelio <- dfps_HartmannCoelio %>%
  mutate(I_geste_comparaison_1HartmannCoelio = ifelse(I_geste_comparaison_1HartmannCoelio == "Hartmann Coelio", 0, 1))

# 2️⃣ Verification des valeurs uniques après transformation
unique(dfps_HartmannCoelio$I_geste_comparaison_1HartmannCoelio)  # Doit retourner uniquement 0 et 1

#Ca ne va pas marcher à cette etape là sauf si aucune donnee manquante
#permet d'afficher les "non finite values", qu'il faudra imputer avec la fonction mice (imputation multiple)
m.out <- matchit(
  formula = I_geste_comparaison_1HartmannCoelio ~ Age + Sexe_Male + BMI + A_ASA_sup2 + A_tabac + A_immunosuppression + I_arret_ATC + A_ATCD_laparo_mediane + I_scoreHinchey_4,
  method = "optimal",
  data = dfps_HartmannCoelio,
  na.rm = TRUE
)


#non finite values sont
"Age",
"BMI",
"A_tabac",
"A_immunosuppression",
"I_arret_ATC"


# Selection uniquement des colonnes à imputer et les utiliser avec la fonction mice()
cols_to_impute <- c(
  "Age",
  "BMI",
  "A_tabac",
  "A_immunosuppression",
  "I_arret_ATC",
  "A_ATCD_laparo_mediane"
)

# Imputation multiple des donnees manquantes avec mice()
imputed_data <- mice(dfps_HartmannCoelio[, cols_to_impute], m = 5, method = "pmm", seed = 123)

# Completer les donnees imputees
completed_data <- complete(imputed_data)

# Remplacer les colonnes imputees dans dfps_imputed
dfps_HartmannCoelio_imputed <- dfps_HartmannCoelio
dfps_HartmannCoelio_imputed[cols_to_impute] <- completed_data

dfps_HartmannCoelio_imputed <- dfps_HartmannCoelio_imputed %>%
  mutate(Age_cat = cut(Age, breaks = c(-Inf, 50, 60, 70, Inf), labels = c("<50", "50-60", "60-70", ">70")))

m.out <- matchit(
  I_geste_comparaison_1HartmannCoelio ~ Age_cat + Sexe_Male + BMI + A_ASA_sup2 + A_tabac + A_immunosuppression + I_arret_ATC + A_ATCD_laparo_mediane + I_scoreHinchey_4,
  method = "nearest",
  caliper = 0.20,
  data = dfps_HartmannCoelio_imputed,  # Appariement exact sur les categories d'âge
  na.rm = TRUE
)


#Affichage de l'appariement du score de propension
m.out

# Tous les individus apparies
m.final <- match.data(m.out)
m.final

#renommer la variable RC
m.final <- m.final %>%
  mutate(
    I_geste_comparaison_1HartmannCoelio = recode(I_geste_comparaison_2HartmannRA, `0` = "HartmannCoelio", `1` = "RAlaparoProtegee"))

# Structure
str(m.final)


#**************SCRIPT PUBMED DE TEST DU SCORE DE PROPENSION**********#
# Après l'appariement base sur le score de propension, un nouveau jeu de donnees est genere.
# Cependant, il n'y a aucune garantie d'equilibre des donnees.
# Dans cette section, nous verifions l'equilibre des distributions des covariables entre les groupes de traitement et de contrôle.
# Nous introduisons et comparons differentes methodes de diagnostic d'equilibre

#comparer les SMD et les VR
#SMD : evalue les differences de covariables sans influence par les unites de mesure
#variable equilibree si SMD < 0,25 (Stuart EA 2010) voire si < 0,1 (Austin PC 2011)
#VR : rapport de la variance entre les grpes traitement et contrôle.
#VR proche de 1.0 = equilibre dans la covariable, tandis qu'un VR <0.5 ou >2.0 est considere comme "trop extrême" (57).

#1ère option : "summary" avec standardize = true (se passe des unites en utilisant le SMD où la difference moyenne est divisee par l'ecart-type dans le groupe traite initialement.
summary(m.out, standardize = TRUE)
# Le resultat de la commande summary comprend cinq parties :
# (I) le call initial (la formule matchit)
# (II) Un df montrant :
####la moyenne de la distance (PS) et des covariables dans les groupes AVANT APPARIEMMENT
####l'ecart-type dans le groupe de contrôle,
####la difference moyenne (standardisee) entre les deux groupes,
####ainsi que la mediane, la moyenne et les differences maximales dans les fonctions de distribution cumulative (CDF) pour chaque covariable ;
# (III) Un df avec la même chose mais pour LES DONNeES APPARIeES
# (IV) L'amelioration du pourcentage de l'equilibre avant et après l'appariement ;
# (V) Un data frame qui compte les echantillons dans tous les ensembles de donnees (originaux) qui sont apparies, non apparies ou rejetes.

# Comparer les covariables appariees avec les covariables initiales pour evaluer l'equilibre
# Utiliser love.plot pour visualiser l'equilibre des covariables
love.plot(bal.tab(m.out),
          var.order = "alphabetical", # Ordre des variables alphabetique (peut être "adjusted" ou "unadjusted")
          threshold = 0.1, # Ajouter une ligne de seuil à 0.1
          stat = "mean.diffs", # Type de statistique à afficher (differences moyennes standardisees)
          grid = TRUE,
          line = TRUE,
          stars = "raw",
          abs = TRUE) # Afficher les valeurs absolues des differences moyennes standardisees

bal.tab(m.out, m.threshold = 0.1, un = TRUE)

#******************2ème test du score de propension*****************#
#**TEST DE LA DISCRIMINATION = AUC**#
# Convertir RC en facteur avec niveaux 0 et 1 explicitement definis
dfps_HartmannCoelio_imputed$I_geste_comparaison_1HartmannCoelio <- factor(dfps_HartmannCoelio_imputed$I_geste_comparaison_1HartmannCoelio, levels = c(0, 1))

# Convertir RC en variable binaire numerique
dfps_HartmannCoelio_imputed$I_geste_comparaison_1HartmannCoelio <- as.numeric(dfps_HartmannCoelio_imputed$I_geste_comparaison_1HartmannCoelio) - 1  # Convertir 0/1 en 0/1 numerique

# Calculer la courbe ROC avec les scores de propension et la variable binaire
roc_curve <- roc(dfps_HartmannCoelio_imputed$I_geste_comparaison_1HartmannCoelio, m.out$distance)

# Tracer la courbe ROC
plot(roc_curve)

# Afficher l'AUC
auc(roc_curve)

#**TEST DE LA CALIBRATION = HOSMER LEMESHOW**#

# Effectuer le test de Hosmer-Lemeshow
hosmer_result <- hoslem.test(dfps_HartmannCoelio_imputed$I_geste_comparaison_1HartmannCoelio, m.out$distance)

# Afficher les resultats du test
print(hosmer_result)

# Interpretation des resultats :
# La sortie du test est un objet avec plusieurs informations, dont la p-value

# Exemple d'interpretation base sur la p-value :
if (hosmer_result$p.value > 0.05) {
  cat("Le modèle est bien calibre. La p-value est", round(hosmer_result$p.value, 3), "superieure à 0.05.\n")
  cat("Cela signifie que les scores de propension correspondent bien aux proportions observees de traitement.\n")
} else {
  cat("Le modèle n'est pas bien calibre. La p-value est", round(hosmer_result$p.value, 3), "inferieure à 0.05.\n")
  cat("Cela suggère que les scores de propension ne correspondent pas bien aux proportions observees de traitement.\n")
}


#********APPARIE : tableaux********#
#Tests statistiques après score de propension
#1 verification de la colonne "subclass" de m.final
#ça doit retourner le nombre de paires avec des 2 en dessous. 
#chaque colonne subclass correspond bien aux paires appariees generees par matchit(). Chaque valeur de subclass est attribuee à deux individus (ce qui est attendu pour un appariement 1:1).
table(m.final$subclass)

# Creer une colonne identifiant les paires
m.final <- m.final %>%
  mutate(Pair_ID = as.factor(subclass))  


#verifier la modification : 
head(m.final$Pair_ID)
#doit retourner qqch comme : 
#1  3  5  6  7  8 
#1 92  2 78  3  4 
#98 Levels: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 ... 98


library(gtsummary)
library(dplyr)

table1adjusted <- m.final %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table1),
    missing = "no",
    by = I_geste_comparaison_1HartmannCoelio,  # Comparaison entre groupes
    percent = "column",
    type = list(
      H_taille_abces ~ "continuous"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{median} ({p25} - {p75})"
    )
  ) %>%
  #add_difference() %>% 
  add_p(
    test = list(
      all_continuous() ~ "paired.wilcox.test",  # Test de Wilcoxon pour les donnees continues
      all_categorical() ~ "mcnemar.test"  # Test de McNemar pour les donnees categoriques
    ),
    group = Pair_ID  # Utilisation de l'ID de paire
  )

# Affichage du tableau final
table1adjusted


# Convertir le tableau en objet gt
table1adjusted_gt <- as_gt(table1adjusted)

# Enregistrer le tableau au format Word
gtsave(table1adjusted_gt , file = "table1adjusted.docx")




table2adjusted <- m.final %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table2),
    missing = "ifany",
    by = I_geste_comparaison_1HartmannCoelio,  # Comparaison entre groupes
    percent = "column",
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{median} ({p25} - {p75})"
    )
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "paired.wilcox.test",  # Test de Wilcoxon pour les donnees continues
      all_categorical() ~ "mcnemar.test"  # Test de McNemar pour les donnees categoriques
    ),
    group = Pair_ID  # Utilisation de l'ID de paire
  )

# Affichage du tableau final
table2adjusted


# Convertir le tableau en objet gt
table2adjusted_gt <- as_gt(table2adjusted)

# Enregistrer le tableau au format Word
gtsave(table2adjusted_gt , file = "table2adjusted.docx")







table3adjusted <- m.final %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table3),
    missing = "ifany",
    by = I_geste_comparaison_1HartmannCoelio,  # Comparaison entre groupes
    percent = "column",
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{median} ({p25} - {p75})"
    )
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "paired.wilcox.test",  # Test de Wilcoxon pour les donnees continues
      all_categorical() ~ "mcnemar.test"  # Test de McNemar pour les donnees categoriques
    ),
    group = Pair_ID  # Utilisation de l'ID de paire
  )

# Affichage du tableau final
table3adjusted


# Convertir le tableau en objet gt
table3adjusted_gt <- as_gt(table3adjusted)

# Enregistrer le tableau au format Word
gtsave(table3adjusted_gt , file = "table3adjusted.docx")






table4adjusted <- m.final %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table4),
    missing = "ifany",
    by = I_geste_comparaison_1HartmannCoelio,  # Comparaison entre groupes
    percent = "column",
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{median} ({p25} - {p75})"
    )
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "paired.wilcox.test",  # Test de Wilcoxon pour les donnees continues
      all_categorical() ~ "mcnemar.test"  # Test de McNemar pour les donnees categoriques
    ),
    group = Pair_ID  # Utilisation de l'ID de paire
  )

# Affichage du tableau final
table4adjusted


# Convertir le tableau en objet gt
table4adjusted_gt <- as_gt(table4adjusted)

# Enregistrer le tableau au format Word
gtsave(table4adjusted_gt , file = "table4adjusted.docx")



#********APPARIE : courbe de survie sans stomie******#

library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Filtrer les patients avec stomie
df_survie_apparie <- m.final %>%
  filter(I_stomie == 1) %>%
  mutate(
    S_duree_avec_stomie = as.numeric(S_duree_avec_stomie),
    fermeture_stomie = ifelse(S_fermeturestomie == 1, 1, 0),
    S_duree_avec_stomie = ifelse(S_fermeturestomie == 1, S_duree_avec_stomie, 730)
  )

# Objet de survie
surv_object <- Surv(df_survie_apparie$S_duree_avec_stomie, df_survie_apparie$fermeture_stomie)
km_fit <- survfit(surv_object ~ I_geste_comparaison_1HartmannCoelio, data = df_survie_apparie)

# p-value log-rank
test_logrank <- survdiff(surv_object ~ I_geste_comparaison_1HartmannCoelio, data = df_survie_apparie)
raw_p_value <- 1 - pchisq(test_logrank$chisq, df = length(test_logrank$n) - 1)
p_value <- ifelse(raw_p_value < 0.001, formatC(raw_p_value, format = "e", digits = 2),
                  formatC(raw_p_value, format = "f", digits = 3))

# Pourcentage de fermeture
taux_final_fermeture <- df_survie_apparie %>%
  group_by(I_geste_comparaison_1HartmannCoelio) %>%
  summarise(pourcentage_fermeture = mean(S_fermeturestomie) * 100, .groups = "drop")

# Medianes donnees
mediane_fermeture <- data.frame(
  I_geste_comparaison_1HartmannCoelio = c(0, 1),
  mediane_jours = c(143.5, 115),
  y_label = c(-1, -4)
)

# Recuperer les couleurs automatiques
p_temp <- ggsurvplot(km_fit, data = df_survie_apparie)
colors_used <- ggplot_build(p_temp$plot)$data[[1]] %>%
  distinct(group, colour) %>%
  arrange(group) %>%
  pull(colour)

# Recuperer la valeur de survie au jour de mediane dans km_fit
get_surv_at_time <- function(km_fit, groupe, temps) {
  surv_data <- summary(km_fit, times = temps)$surv
  surv_percent <- 100 * (1 - surv_data)  # car on affiche 100*(1-survie)
  return(surv_percent[groupe + 1])  # groupe 0 ou 1
}

mediane_fermeture$y_courbe <- sapply(1:nrow(mediane_fermeture), function(i) {
  get_surv_at_time(km_fit, mediane_fermeture$I_geste_comparaison_1HartmannCoelio[i],
                   mediane_fermeture$mediane_jours[i])
})

# Tracer la courbe finale
p <- ggsurvplot(
  km_fit,
  data = df_survie_apparie,
  pval = TRUE,
  risk.table = TRUE,
  break.time.by = 100,
  conf.int = TRUE,
  xlim = c(0, 730),
  xlab = "Temps (jours)",
  ylab = "Taux de retablissement de la continuite (%)",
  legend.title = "Technique chirurgicale",
  legend.labs = c("Hartmann", "RA Protegee"),
  fun = function(x) 100 * (1 - x)
)

# Ajouter les elements graphiques
p$plot <- p$plot +
  guides(fill = "none", linetype = "none",
         color = guide_legend(override.aes = list(linetype = 1))) +
  
  # Lignes horizontales et verticales qui s'arrêtent à la vraie courbe
  geom_segment(data = mediane_fermeture,
               aes(x = 0, xend = mediane_jours,
                   y = y_courbe, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  
  geom_segment(data = mediane_fermeture,
               aes(x = mediane_jours, xend = mediane_jours,
                   y = 0, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  
  # Texte sous l'axe des abscisses
  geom_text(data = mediane_fermeture,
            aes(x = mediane_jours, y = y_label,
                label = paste0(mediane_jours, " j")),
            color = colors_used, size = 5, fontface = "bold",
            hjust = 0.5, show.legend = FALSE) +
  
  # Taux final
  geom_hline(aes(yintercept = pourcentage_fermeture),
             data = taux_final_fermeture, linetype = "dashed", size = 1,
             color = "black", show.legend = FALSE) +
  
  geom_text(data = taux_final_fermeture,
            aes(x = 730, y = pourcentage_fermeture,
                label = paste0(round(pourcentage_fermeture, 1), "%")),
            vjust = -0.5, size = 5, fontface = "bold", color = "black") +
  
  annotate("text", x = 600, y = 10,
           label = paste0("Log-rank p = ", p_value),
           size = 5, hjust = 0)

# Affichage
print(p)


#********APPARIE : courbe de survie sans stomie******#
#********MATCHED: stoma-free survival curve******#

library(survival)
library(survminer)
library(patchwork)
library(dplyr)

# 1) Préparation & km_fit … (inchangé)
df_survie_apparie <- m.final %>%
  filter(I_stomie == 1) %>%
  mutate(
    S_duree_avec_stomie = as.numeric(S_duree_avec_stomie),
    fermeture_stomie     = ifelse(S_fermeturestomie == 1, 1, 0),
    S_duree_avec_stomie = ifelse(S_fermeturestomie == 1,
                                 S_duree_avec_stomie, 730)
  )

surv_object <- Surv(df_survie_apparie$S_duree_avec_stomie,
                    df_survie_apparie$fermeture_stomie)
km_fit <- survfit(surv_object ~ I_geste_comparaison_1HartmannCoelio,
                  data = df_survie_apparie)

# 2) calculs préliminaires (p-value, taux finaux) … (inchangé)
test_logrank <- survdiff(surv_object ~ I_geste_comparaison_1HartmannCoelio,
                         data = df_survie_apparie)
raw_p_value <- 1 - pchisq(test_logrank$chisq,
                          df = length(test_logrank$n) - 1)
p_value <- ifelse(
  raw_p_value < 0.001,
  "< 0.001",
  paste0(formatC(raw_p_value, format = "f", digits = 3))
)


taux_final_fermeture <- df_survie_apparie %>%
  group_by(I_geste_comparaison_1HartmannCoelio) %>%
  summarise(pourcentage_fermeture = mean(S_fermeturestomie)*100,
            .groups = "drop")

# 3) médianes + y_courbe (LIGNE AJOUTÉE)
mediane_fermeture <- data.frame(
  I_geste_comparaison_1HartmannCoelio = c(0, 1),
  mediane_jours = c(143.5, 115),
  y_label      = c(-1, -4)
)

get_surv_at_time <- function(km_fit, groupe, temps) {
  surv_data   <- summary(km_fit, times = temps)$surv
  surv_percent<- 100 * (1 - surv_data)
  surv_percent[groupe + 1]
}

mediane_fermeture$y_courbe <- sapply(
  seq_len(nrow(mediane_fermeture)),
  function(i) {
    get_surv_at_time(
      km_fit,
      mediane_fermeture$I_geste_comparaison_1HartmannCoelio[i],
      mediane_fermeture$mediane_jours[i]
    )
  }
)

# 4) récupère les couleurs auto (inchangé)
p_temp      <- ggsurvplot(km_fit, data = df_survie_apparie)
colors_used <- ggplot_build(p_temp$plot)$data[[1]] %>%
  distinct(group, colour) %>%
  arrange(group) %>%
  pull(colour)

# 5) build du ggsurvplot de base (inchangé, pval=FALSE) …
p <- ggsurvplot(
  km_fit, data = df_survie_apparie,
  fun                  = function(x) 100*(1 - x),
  pval                 = FALSE,
  conf.int             = TRUE,
  xlim                 = c(0, 730),
  break.time.by        = 100,
  risk.table           = TRUE,
  risk.table.height    = 0.25,
  risk.table.y.text.col= TRUE,
  risk.table.breaks    = seq(0, 730, 100),
  risk.table.xlim      = c(0, 730),
  xlab                 = "Time (days)",
  ylab                 = "Stoma reversal rate (%)",
  legend.title         = "Surgical technique",
  legend.labs          = c("Laparoscopic Hartmann",
                           "Open Diverted anastomosis"),
  ggtheme              = theme_classic()
)

# 6) on customise p$plot avec vos segments, textes, annotate() …
p$plot <- p$plot +
  guides(
    fill     = "none",
    linetype = "none",
    color    = guide_legend(override.aes = list(linetype = 1))
  ) +
  # texte du pourcentage final au bout du plot
  geom_text(
    data    = taux_final_fermeture,
    aes(x = 730, y = pourcentage_fermeture,
        label = paste0(round(pourcentage_fermeture, 1), "%")),
    vjust     = -0.5,
    size      = 5,
    fontface  = "bold",
    color     = "black"
  ) +
  # annotation du p-value
  annotate(
    "text",
    x     = 400,
    y     = 10,
    label = paste0("p ", p_value),
    size  = 5,
    hjust = 0
  )

# 7) re-assemblement patchwork
combined_plot <- p$plot / p$table +
  plot_layout(heights = c(8, 2))

print(combined_plot)


colors        = c("#db1d88", "#4b6bd6")



#********APPARIE : courbe de survie sans stomie******#
#********MATCHED: stoma-free survival curve******#

library(survival)
library(survminer)
library(patchwork)
library(dplyr)

# 1) Préparation & km_fit … (inchangé)
df_survie_apparie <- m.final %>%
  filter(I_stomie == 1) %>%
  mutate(
    S_duree_avec_stomie = as.numeric(S_duree_avec_stomie),
    fermeture_stomie     = ifelse(S_fermeturestomie == 1, 1, 0),
    S_duree_avec_stomie = ifelse(S_fermeturestomie == 1,
                                 S_duree_avec_stomie, 730)
  )

surv_object <- Surv(df_survie_apparie$S_duree_avec_stomie,
                    df_survie_apparie$fermeture_stomie)
km_fit <- survfit(surv_object ~ I_geste_comparaison_1HartmannCoelio,
                  data = df_survie_apparie)

# 2) calculs préliminaires (p-value, taux finaux) … (inchangé)
test_logrank <- survdiff(surv_object ~ I_geste_comparaison_1HartmannCoelio,
                         data = df_survie_apparie)
raw_p_value <- 1 - pchisq(test_logrank$chisq,
                          df = length(test_logrank$n) - 1)
p_value <- ifelse(
  raw_p_value < 0.001,
  "< 0.001",
  paste0(formatC(raw_p_value, format = "f", digits = 3))
)


taux_final_fermeture <- df_survie_apparie %>%
  group_by(I_geste_comparaison_1HartmannCoelio) %>%
  summarise(pourcentage_fermeture = mean(S_fermeturestomie)*100,
            .groups = "drop")

# 3) médianes + y_courbe (LIGNE AJOUTÉE)
mediane_fermeture <- data.frame(
  I_geste_comparaison_1HartmannCoelio = c(0, 1),
  mediane_jours = c(143.5, 115),
  y_label      = c(-1, -4)
)

get_surv_at_time <- function(km_fit, groupe, temps) {
  surv_data   <- summary(km_fit, times = temps)$surv
  surv_percent<- 100 * (1 - surv_data)
  surv_percent[groupe + 1]
}

mediane_fermeture$y_courbe <- sapply(
  seq_len(nrow(mediane_fermeture)),
  function(i) {
    get_surv_at_time(
      km_fit,
      mediane_fermeture$I_geste_comparaison_1HartmannCoelio[i],
      mediane_fermeture$mediane_jours[i]
    )
  }
)

# 4) récupère les couleurs auto (inchangé)
p_temp      <- ggsurvplot(km_fit, data = df_survie_apparie)
colors_used <- ggplot_build(p_temp$plot)$data[[1]] %>%
  distinct(group, colour) %>%
  arrange(group) %>%
  pull(colour)

# 5) build du ggsurvplot de base (inchangé, pval=FALSE) …
p <- ggsurvplot(
  km_fit, data = df_survie_apparie,
  fun                  = function(x) 100*(1 - x),
  pval                 = FALSE,
  palette = colors,
  conf.int             = TRUE,
  xlim                 = c(0, 730),
  break.time.by        = 100,
  risk.table           = TRUE,
  risk.table.height    = 0.25,
  risk.table.y.text.col= TRUE,
  risk.table.breaks    = seq(0, 730, 100),
  risk.table.xlim      = c(0, 730),
  xlab                 = "Time (days)",
  ylab                 = "Stoma reversal rate (%)",
  legend.title         = "Surgical technique",
  legend.labs          = c("Laparoscopic Hartmann",
                           "Open Diverted anastomosis"),
  ggtheme              = theme_classic()
)

# 6) on customise p$plot avec vos segments, textes, annotate() …
p$plot <- p$plot +
  guides(
    fill     = "none",
    linetype = "none",
    color    = guide_legend(override.aes = list(linetype = 1))
  ) +
  # texte du pourcentage final au bout du plot
  geom_text(
    data    = taux_final_fermeture,
    aes(x = 730, y = pourcentage_fermeture,
        label = paste0(round(pourcentage_fermeture, 1), "%")),
    vjust     = -0.5,
    size      = 5,
    fontface  = "bold",
    color     = "black"
  ) +
  # annotation du p-value
  annotate(
    "text",
    x     = 400,
    y     = 10,
    label = paste0("p ", p_value),
    size  = 5,
    hjust = 0
  )

# 7) re-assemblement patchwork
combined_plot <- p$plot / p$table +
  plot_layout(heights = c(8, 2))

print(combined_plot)





# Extraire le taux de fermeture de stomie à 2 ans (730 jours) dans chaque groupe
surv_2_ans <- summary(km_fit, times = 730)$surv
taux_fermeture_730 <- 100 * (1 - surv_2_ans)  # en pourcentage
names(taux_fermeture_730) <- c("RA", "Hartmann")  # selon l'ordre des groupes dans km_fit
print(taux_fermeture_730)



# Modèle de Cox marginal avec robustesse intra-paire
cox_robuste <- coxph(Surv(S_duree_avec_stomie, fermeture_stomie) ~ I_geste_comparaison_1HartmannCoelio + cluster(Pair_ID),
                     data = df_survie_apparie)

summary(cox_robuste)




# Charger les packages necessaires
library(survival)
library(forestplot)
library(dplyr)

# Modèle de Cox marginal (dejà calcule)
cox_robuste <- coxph(Surv(S_duree_avec_stomie, fermeture_stomie) ~ I_geste_comparaison_1HartmannCoelio + cluster(Pair_ID),
                     data = df_survie_apparie)

# Extraire les infos
hr <- exp(coef(cox_robuste))
ci <- exp(confint(cox_robuste))
labels <- c("RA laparo protegee vs Hartmann coelio")

# Preparer le tableau pour le forestplot
table_forest <- data.frame(
  Variable = labels,
  HR = round(hr, 2),
  CI_low = round(ci[, 1], 2),
  CI_high = round(ci[, 2], 2)
)

# Creer un objet texte avec HR et IC95%
tabletext <- cbind(
  c("Technique", table_forest$Variable),
  c("HR [IC95%]", paste0(table_forest$HR, " [", table_forest$CI_low, " – ", table_forest$CI_high, "]"))
)

# Generer le forestplot
forestplot(
  labeltext = tabletext,
  mean = c(NA, table_forest$HR),
  lower = c(NA, table_forest$CI_low),
  upper = c(NA, table_forest$CI_high),
  zero = 1,
  boxsize = 0.2,
  line.margin = 0.2,
  col = fpColors(box = "black", line = "black", summary = "black"),
  xlog = TRUE,
  lwd.zero = 2,
  lwd.ci = 2,
  ci.vertices = TRUE,
  xlab = "Hazard Ratio (log-echelle)",
  title = "Modèle de Cox ajuste à l’appariement"
)



# Packages
library(survival)
library(forestplot)
library(dplyr)

# Modèle Cox robuste
cox_robuste <- coxph(Surv(S_duree_avec_stomie, fermeture_stomie) ~ I_geste_comparaison_1HartmannCoelio + cluster(Pair_ID),
                     data = df_survie_apparie)

# Resume HR + IC
hr <- exp(coef(cox_robuste))
ci <- exp(confint(cox_robuste))
labels <- c("RA laparo protegee vs Hartmann coelio")

# Preparer donnees pour forestplot
table_forest <- data.frame(
  Variable = labels,
  HR = round(hr, 2),
  CI_low = round(ci[, 1], 2),
  CI_high = round(ci[, 2], 2)
)

tabletext <- cbind(
  c("Technique", table_forest$Variable),
  c("HR [IC95%]", paste0(table_forest$HR, " [", table_forest$CI_low, " – ", table_forest$CI_high, "]"))
)







# Chargement des packages
library(survival)
library(dplyr)
library(broom)
library(ggplot2)
install.packages("lmtest")
library(patchwork)
library(sandwich)
library(lmtest)

# Modèle de Cox robuste (avec cluster)
cox_robuste <- coxph(Surv(S_duree_avec_stomie, fermeture_stomie) ~ I_geste_comparaison_1HartmannCoelio + cluster(Pair_ID),
                     data = df_survie_apparie)

# Extraire les coefficients robustes
# On utilise coeftest() avec estimateur sandwich (robust)
robust_test <- coeftest(cox_robuste)

# Extraire HR, IC 95% et p-value
hr <- exp(robust_test[,"Estimate"])
ci_low <- exp(robust_test[,"Estimate"] - 1.96 * robust_test[,"Std. Error"])
ci_high <- exp(robust_test[,"Estimate"] + 1.96 * robust_test[,"Std. Error"])
pval <- robust_test[,"Pr(>|z|)"]

# Creer dataframe pour ggplot
df_forest <- data.frame(
  variable = "RA laparo protegee vs Hartmann coelio",
  HR = hr,
  CI_low = ci_low,
  CI_high = ci_high,
  p_value = pval
)

# Forestplot ggplot
forest_plot <- ggplot(df_forest, aes(x = HR, y = variable)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_log10(limits = c(0.4, 3), breaks = c(0.5, 1, 2, 3)) +
  xlab("Hazard Ratio (echelle log)") +
  ylab("") +
  ggtitle("Modèle de Cox avec estimateur robuste (cluster sur les paires)") +
  theme_minimal(base_size = 14)

# Flèches directionnelles (en bas du plot)
flèche_gg <- ggplot() +
  annotate("text", x = 0, y = 0, label = "← en faveur du Hartmann coelio", hjust = 0, size = 5) +
  annotate("text", x = 1, y = 0, label = "→ en faveur de la RA laparo protegee", hjust = 1, size = 5) +
  theme_void() +
  xlim(0, 1)

# Combiner les deux avec patchwork
forest_plot / flèche_gg + plot_layout(heights = c(10, 1))



library(survival)

# Modèle de Cox stratifie sur les paires
cox_stratifie <- coxph(Surv(S_duree_avec_stomie, fermeture_stomie) ~ I_geste_comparaison_2HartmannRA + strata(Pair_ID),
                       data = df_survie_apparie)

# Afficher le resume
summary(cox_stratifie)



# Charger les bibliothèques nécessaires
library(survival)
library(ggplot2)
library(dplyr)
library(forestplot)

# Créer un data.frame résumant les deux modèles
df_forest <- tibble::tibble(
  Modèle = c("Cox marginal robuste", "Cox stratifié par paire"),
  HR = c(1.84, 2.28),
  IC_inf = c(1.39, 1.47),
  IC_sup = c(2.44, 3.52),
  p = c(2.34e-05, 0.000223)
)

# Créer une colonne pour affichage textuel de l'IC
df_forest <- df_forest %>%
  mutate(
    HR_IC = sprintf("%.2f [%.2f–%.2f]", HR, IC_inf, IC_sup),
    p_text = ifelse(p < 0.001, "< 0.001", formatC(p, format = "f", digits = 3))
  )

# Préparer les lignes pour forestplot
tabletext <- cbind(
  c("Modèle", df_forest$Modèle),
  c("HR [IC 95%]", df_forest$HR_IC),
  c("p", df_forest$p_text)
)

# Tracer le forest plot
forestplot::forestplot(
  labeltext = tabletext,
  mean = c(NA, df_forest$HR),
  lower = c(NA, df_forest$IC_inf),
  upper = c(NA, df_forest$IC_sup),
  zero = 1,
  xlog = TRUE,  # Échelle logarithmique
  col = forestplot::fpColors(box = "black", lines = "black", zero = "gray50"),
  boxsize = 0.2,
  lineheight = unit(1.5, "cm"),
  lwd.ci = 2,
  ci.vertices = TRUE,
  ci.vertices.height = 0.1,
  xticks = c(0.5, 1, 2, 4),
  xlab = "<--- Faveur Hartmann                  Faveur RA protégée --->"
)


#***************ANALYSE UNIVARIEE FDR DE CONVERSION*****************

df_conversion <- df_conversion %>%
  rename(
    H_antibio_IV_PO = `H_antibio_IV_+_PO`,
    I_indication_poussee_compliquee = `I_indication_poussee_compliquee`,
    I_indication_immunodepression = `I_indication_immunodepression`,
    I_indication_hemorragie = `I_indication_hemorragie`
  )


# 🎯 Recode de la variable cible : conversion (dans df_HartmannCoelio)
df_conversion <- df_HartmannCoelio %>% 
  filter(I_voie_coelio == 1)  # uniquement ceux chez qui une coelioscopie a ete tentee

df_conversion$conversion <- factor(df_conversion$I_conversion, levels = c(0,1), labels = c("Non converti", "Converti"))

# 🔠 Variables explicatives = celles du tableau 1
vars <- cols_to_include_in_table1

# 🧼 Convertir les binaires numeriques en facteurs simples
df_conversion[vars] <- lapply(df_conversion[vars], function(x) {
  if (is.numeric(x) && length(unique(x)) <= 2) factor(x, ordered = FALSE) else x
})

# 🔍 Detection des variables avec moins de 2 modalites dans df_conversion
vars_uniques <- sapply(df_conversion[vars], function(x) length(unique(na.omit(x))))
vars_problematiques <- names(vars_uniques[vars_uniques < 2])
vars_problematiques

# ✅ Exclusion des variables avec < 2 modalites
vars_clean <- setdiff(vars, vars_problematiques)

# ⚙️ Regressions univariees logistiques sur la conversion
univ_models <- lapply(vars_clean, function(var) {
  model <- glm(as.formula(paste("conversion ~", var)), data = df_conversion, family = binomial)
  result <- tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(variable = var)
  
  
  # Niveau de reference
  if (is.factor(df_conversion[[var]])) {
    ref_level <- levels(df_conversion[[var]])[1]
    ref_row <- tibble(
      variable = var,
      term = ref_level,
      OR = 1,
      IC_bas = NA,
      IC_haut = NA,
      `p-value` = NA
    )
    
    result_clean <- result %>%
      filter(term != "(Intercept)") %>%
      select(term, estimate, conf.low, conf.high, p.value) %>%
      rename(OR = estimate, IC_bas = conf.low, IC_haut = conf.high, `p-value` = p.value) %>%
      mutate(variable = var)
    
    bind_rows(ref_row, result_clean)
    
  } else {
    result %>%
      filter(term != "(Intercept)") %>%
      select(term, estimate, conf.low, conf.high, p.value) %>%
      rename(OR = estimate, IC_bas = conf.low, IC_haut = conf.high, `p-value` = p.value) %>%
      mutate(variable = var)
  }
})

# 📋 Regroupement dans un seul tableau
univ_table <- bind_rows(univ_models) %>%
  select(variable, term, OR, IC_bas, IC_haut, `p-value`) %>%
  arrange(variable, term)

# ✨ Ajout indentation pour les modalites
univ_table <- univ_table %>%
  mutate(term_affiche = paste0("  ", term))

# 💄 Mise en forme
univ_table <- univ_table %>%
  mutate(
    OR = ifelse(is.na(OR) | OR == 0 | OR == Inf, NA, OR),
    IC_bas = ifelse(is.na(IC_bas) | IC_bas == 0 | IC_bas == Inf, NA, IC_bas),
    IC_haut = ifelse(is.na(IC_haut) | IC_haut == 0 | IC_haut == Inf | IC_haut > 1e5, NA, IC_haut),
    OR = sprintf("%.2f", OR),
    IC_bas = sprintf("%.2f", IC_bas),
    IC_haut = sprintf("%.2f", IC_haut),
    `p-value` = ifelse(is.na(`p-value`), "", 
                       ifelse(`p-value` < 0.001, "< 0.001", sprintf("%.3f", `p-value`)))
  ) %>%
  mutate(
    OR = ifelse(grepl("NA", OR), "—", OR),
    IC_bas = ifelse(grepl("NA", IC_bas), "—", IC_bas),
    IC_haut = ifelse(grepl("NA", IC_haut), "—", IC_haut)
  )

# 📊 Affichage final avec kableExtra
library(kableExtra)

colnames_kable <- c("Modalite", "OR", "IC\u00A095%\u00A0bas", "IC\u00A095%\u00A0haut", "P\u00A0value")

univ_table %>%
  select(term_affiche, OR, IC_bas, IC_haut, `p-value`) %>%
  kable(col.names = colnames_kable,
        align = "lcccc",
        caption = "Analyse univariee : OR et IC95% des facteurs de conversion chez les patients avec Hartmann coelioscopique") %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  pack_rows(index = table(univ_table$variable))









#***************ANALYSE MULTIVARIEE*****************
# Sélection des variables appropriées
vars_multi_ajuste <- c("Sexe_Male","BMI_sup30", "A_ASA_sup2", "A_ATCD_neuro", "A_ATCD_Cardio", 
                       "A_ATCD_pneumo", "A_diabete", "A_tabac", "A_immunosuppression", 
                       "A_anticoagulants", "A_antiaggregants", "A_ATCD_laparo_mediane", 
                       "H_perforation", "H_abces", "I_scoreHinchey_3", "I_scoreHinchey_4", "I_indication_hemorragie", 
                       "I_indication_immunodepression", "I_indication_poussee_compliquee", "I_indication_smoldering",
                       "I_localisation_colondroit", "I_localisation_sigmoide")

vars_multi_ajuste <- c(
  "Sexe_Male", "A_ASA_sup2", "BMI", "A_ATCD_neuro", "A_ATCD_Cardio", 
  "A_ATCD_pneumo", "A_diabete", "A_tabac", "A_immunosuppression", 
  "A_anticoagulants", "A_antiaggregants", "A_ATCD_laparo_mediane", 
  "H_perforation", "H_colo", "I_scoreHinchey_4"
)

df_HartmannCoelio <- df_HartmannCoelio %>%
  mutate(BMI_sup30 = ifelse(BMI >= 30, 1, 0))

df_HartmannCoelio <- df_HartmannCoelio %>%
  mutate(BMI_sub18 = ifelse(BMI <= 18, 1, 0))

# Construction de la formule pour la régression multivariée avec Hinchey 3 et Hinchey 4 séparés
formule_multi_ajuste <- as.formula(
  paste("I_conversion ~", paste(vars_multi_ajuste, collapse = " + "))
)

# Modèle de régression logistique multivariée
model_multi_ajuste <- glm(formule_multi_ajuste, data = df_HartmannCoelio, family = binomial)

# Résumé du modèle
summary(model_multi_ajuste)

# Extraire les coefficients et les p-values
coefs <- summary(model_multi_ajuste)$coefficients[, c("Estimate", "Std. Error", "Pr(>|z|)")]
coefs <- as.data.frame(coefs)

# Calcul des intervalles de confiance (IC à 95%)
coefs$CI_lower <- coefs$Estimate - 1.96 * coefs$`Std. Error`
coefs$CI_upper <- coefs$Estimate + 1.96 * coefs$`Std. Error`

# Formater les p-values (affichage sous forme de <0.001 ou valeurs)
coefs$`Pr(>|z|)` <- ifelse(coefs$`Pr(>|z|)` < 0.001, "< 0.001", 
                           sprintf("%.3f", coefs$`Pr(>|z|)`))

# Formater les coefficients et intervalles
coefs$Estimate <- sprintf("%.2f", coefs$Estimate)
coefs$CI_lower <- sprintf("%.2f", coefs$CI_lower)
coefs$CI_upper <- sprintf("%.2f", coefs$CI_upper)

# Créer un dataframe pour le forest plot avec les résultats de la régression
forest_data <- data.frame(
  Variable = rownames(coefs),
  HR = exp(as.numeric(coefs$Estimate)),  # Calcul du Hazard Ratio
  CI_Lower = exp(as.numeric(coefs$CI_lower)),  # Limite inférieure de l'intervalle de confiance
  CI_Upper = exp(as.numeric(coefs$CI_upper)),  # Limite supérieure de l'intervalle de confiance
  p_value = coefs$`Pr(>|z|)`
)

# Préparer l'objet textuel pour afficher HR et IC
tabletext <- cbind(
  c("Variable", forest_data$Variable),
  c("HR [IC95%]", paste0(round(forest_data$HR, 2), " [", round(forest_data$CI_Lower, 2), " - ", round(forest_data$CI_Upper, 2), "]"))
)


# Créer le forest plot
library(forestplot)

forestplot(
  labeltext = tabletext,
  mean = c(NA, forest_data$HR),
  lower = c(NA, forest_data$CI_Lower),
  upper = c(NA, forest_data$CI_Upper),
  zero = 1,
  boxsize = 0.2,
  line.margin = 0.2,
  col = fpColors(box = "black", line = "black", summary = "black"),
  xlog = TRUE,  # Affichage de l'échelle logarithmique
  lwd.zero = 2,
  lwd.ci = 2,
  ci.vertices = TRUE,
  xlab = "Hazard Ratio (log-échelle)",
  title = "Modèle de régression logistique multivariée pour la conversion"
)












#----------------------------------------
#Analyse des caractéristiques convertis vs non convertis
#----------------------------------------

# Sélectionner les patients du groupe Hartmann Coelio
df_conversion_HartmannCoelio <- df_HartmannCoelio %>%
  filter(I_geste_comparaison_1HartmannCoelio == "Hartmann Coelio")

# Renommer les valeurs de la variable I_conversion
df_conversion_HartmannCoelio$I_conversion <- recode(df_conversion_HartmannCoelio$I_conversion,
                                                    `0` = "Pas de conversion",
                                                    `1` = "Conversion")

# Vérifier le résultat
table(df_conversion_HartmannCoelio$I_conversion)


# Création du tableau 2 non ajusté avec comparaison entre les patients convertis et non convertis
table2unadjusted <- df_conversion_HartmannCoelio %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table2),
    missing = "ifany",
    by = I_conversion,  # Variable de regroupement (conversion oui/non)
    percent = "column",  # Calcule les pourcentages sur l’ensemble de la colonne
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",  # Effectifs et pourcentages pour variables catégorielles
      all_continuous() ~ "{median} ({p25} - {p75})"  # Mediane et IQR pour variables continues
    )  
  ) %>%  
  add_p()  # Ajoute les valeurs p pour les comparaisons


# Affichage du tableau
table2unadjusted

# Convertir le tableau en objet gt
table2unadjusted_gt <- as_gt(table2unadjusted)

# Enregistrer le tableau au format Word
gtsave(table2unadjusted_gt , file = "table2unadjusted.docx")






# Création du tableau 3 non ajusté avec médianes et IQR, comparaison entre les patients convertis et non convertis
table3unadjusted <- df_conversion_HartmannCoelio %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table3),
    missing = "ifany",
    by = I_conversion,  # Variable de regroupement (conversion oui/non)
    percent = "column",  # Calcule les pourcentages sur l’ensemble de la colonne
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",  # Effectif et pourcentage pour variables catégorielles
      all_continuous() ~ "{median} [{p25} - {p75}]"  # Mediane et IQR pour variables continues
    )
  ) %>%
  add_p()  # Ajoute les valeurs p pour les comparaisons

# Affichage du tableau
table3unadjusted

# Convertir le tableau en objet gt
table3unadjusted_gt <- as_gt(table3unadjusted)

# Enregistrer le tableau au format Word
gtsave(table3unadjusted_gt , file = "table3unadjusted.docx")






# Création du tableau 4 non ajusté avec médianes et IQR, comparaison entre les patients convertis et non convertis
table4unadjusted <- df_conversion_HartmannCoelio %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table4),
    missing = "ifany",
    by = I_conversion,  # Variable de regroupement (conversion oui/non)
    percent = "column",  # Calcule les pourcentages sur l’ensemble de la colonne
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",  # Effectif et pourcentage pour variables catégorielles
      all_continuous() ~ "{median} [{p25} - {p75}]"  # Mediane et IQR pour variables continues
    )
  ) %>%
  add_p()  # Ajoute les valeurs p pour les comparaisons

# Affichage du tableau
table4unadjusted

# Convertir le tableau en objet gt
table4unadjusted_gt <- as_gt(table4unadjusted)

# Enregistrer le tableau au format Word
gtsave(table4unadjusted_gt , file = "table4unadjusted.docx")






#**********COURBE HARTMANN COELIO CONVERTIS OU NON RÉGLÉE*********
# Charger les bibliothèques nécessaires
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Préparer les données de survie pour la courbe Kaplan-Meier
df_survie <- df_HartmannCoelio %>% 
  filter(I_geste_comparaison_1HartmannCoelio == "Hartmann Coelio") %>%
  filter(I_stomie == 1) %>%
  mutate(
    S_duree_avec_stomie_orig = as.numeric(S_duree_avec_stomie),
    fermeture_stomie = ifelse(S_fermeturestomie == 1, 1, 0),
    S_duree_avec_stomie = ifelse(S_fermeturestomie == 1, S_duree_avec_stomie_orig, 730)
  )

# Conversion en facteur
df_survie$I_conversion <- factor(df_survie$I_conversion, levels = c(0, 1), labels = c("Non Converti", "Converti"))

# Objet de survie
surv_object <- Surv(time = df_survie$S_duree_avec_stomie, event = df_survie$fermeture_stomie)
km_fit <- survfit(surv_object ~ I_conversion, data = df_survie)

# Test log-rank
test_logrank <- survdiff(surv_object ~ I_conversion, data = df_survie)
raw_p_value <- 1 - pchisq(test_logrank$chisq, df = length(test_logrank$n) - 1)
p_value <- ifelse(raw_p_value < 0.001, formatC(raw_p_value, format = "e", digits = 2),
                  formatC(raw_p_value, format = "f", digits = 3))

# Pourcentages de fermeture
taux_final_fermeture <- df_survie %>%
  group_by(I_conversion) %>%
  summarise(pourcentage_fermeture = mean(S_fermeturestomie) * 100, .groups = "drop")

# Médianes comme dans tbl_summary (calculées sans censures, sur la base brute)
mediane_fermeture <- df_conversion_HartmannCoelio %>%
  group_by(I_conversion) %>%
  summarise(
    mediane_jours = median(S_duree_avec_stomie, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    I_conversion = recode(I_conversion,
                          "Pas de conversion" = "Non Converti",
                          "Conversion" = "Converti"),
    y_label = case_when(
      I_conversion == "Non Converti" ~ -1,
      I_conversion == "Converti" ~ -5
    ),
    label_median = paste0(mediane_jours, "j")
  )

# Fonction pour estimer la position verticale (survie transformée)
get_surv_at_time <- function(km_fit, groupe, temps) {
  surv_data <- summary(km_fit, times = temps)$surv
  surv_percent <- 100 * (1 - surv_data)
  return(surv_percent[groupe])
}

# Ajouter la position verticale sur la courbe pour les annotations
mediane_fermeture$y_courbe <- sapply(1:nrow(mediane_fermeture), function(i) {
  get_surv_at_time(km_fit, i, mediane_fermeture$mediane_jours[i])
})

# Récupérer les couleurs utilisées automatiquement par ggsurvplot
p_temp <- ggsurvplot(km_fit, data = df_survie)
colors_used <- ggplot_build(p_temp$plot)$data[[1]] %>%
  distinct(group, colour) %>%
  arrange(group) %>%
  pull(colour)

# Générer la courbe principale avec toutes les annotations
p <- ggsurvplot(
  km_fit,
  data = df_survie,
  pval = TRUE,
  risk.table = TRUE,
  conf.int = TRUE,
  break.time.by = 100,
  xlim = c(0, 730),
  xlab = "Temps (jours)",
  ylab = "Taux de rétablissement de la continuité (%)",
  legend.title = "Conversion",
  legend.labs = c("Non Converti", "Converti"),
  fun = function(x) 100 * (1 - x)
)

# Ajouter les éléments sur la courbe
p$plot <- p$plot +
  guides(
    fill = "none",
    linetype = "none",
    color = guide_legend(override.aes = list(linetype = 1))
  ) +
  geom_hline(aes(yintercept = pourcentage_fermeture),
             data = taux_final_fermeture, linetype = "dashed", size = 1,
             color = "black", show.legend = FALSE) +
  geom_text(data = taux_final_fermeture, aes(x = 730, y = pourcentage_fermeture,
                                             label = paste0(round(pourcentage_fermeture, 1), "%")),
            vjust = -0.5, size = 5, fontface = "bold", color = "black") +
  annotate("text", x = 600, y = 10,
           label = paste0("Log-rank p = ", p_value),
           size = 5, hjust = 0) +
  geom_segment(data = mediane_fermeture,
               aes(x = 0, xend = mediane_jours,
                   y = y_courbe, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  geom_segment(data = mediane_fermeture,
               aes(x = mediane_jours, xend = mediane_jours,
                   y = 0, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  geom_text(data = mediane_fermeture,
            aes(x = mediane_jours, y = y_label,
                label = label_median),
            color = colors_used, size = 5, fontface = "bold",
            hjust = 0.5, show.legend = FALSE)

# Afficher la courbe
print(p)
















# Charger les bibliothèques nécessaires
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

unique(m.final$I_geste_comparaison_1HartmannCoelio)

# Créer une variable pour distinguer les trois groupes
df_survie <- m.final %>% 
  filter(I_stomie == 1) %>%  # Tous les patients ont une stomie par définition
  mutate(
    S_duree_avec_stomie = as.numeric(S_duree_avec_stomie),  # Assurez-vous que S_duree_avec_stomie est numérique
    fermeture_stomie = ifelse(S_fermeturestomie == 1, 1, 0),  # Créer la variable fermeture de stomie
    S_duree_avec_stomie = ifelse(S_fermeturestomie == 1, S_duree_avec_stomie, 365),  # 730 jours pour les patients sans fermeture
    # Créer une variable pour distinguer les trois groupes
    groupe = case_when(
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 0 ~ "Hartmann Non Converti",
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 1 ~ "Hartmann Converti",
      I_geste_comparaison_1HartmannCoelio == "Résection anastomose protégée" ~ "RA Protégée",  # Correction du nom
      TRUE ~ NA_character_  # Exclure les autres cas
    )
  )


# Convertir la variable groupe en facteur
df_survie$groupe <- factor(df_survie$groupe, levels = c("Hartmann Non Converti", "Hartmann Converti", "RA Protégée"))

# Définir l'objet de survie
surv_object <- Surv(time = df_survie$S_duree_avec_stomie, event = df_survie$fermeture_stomie)

# Modèle de Kaplan-Meier pour la comparaison entre les trois groupes
km_fit <- survfit(surv_object ~ groupe, data = df_survie)

# Test du log-rank pour comparer les groupes
test_logrank <- survdiff(surv_object ~ groupe, data = df_survie)
raw_p_value <- 1 - pchisq(test_logrank$chisq, df = length(test_logrank$n) - 1)
p_value <- ifelse(raw_p_value < 0.001, formatC(raw_p_value, format = "e", digits = 2),
                  formatC(raw_p_value, format = "f", digits = 3))

# Pourcentage final de fermeture de stomie pour chaque groupe
taux_final_fermeture <- df_survie %>%
  group_by(groupe) %>%
  summarise(pourcentage_fermeture = mean(S_fermeturestomie) * 100, .groups = "drop")

# Médianes fixées manuellement pour chaque groupe
mediane_fermeture <- data.frame(
  groupe = c("Hartmann Non Converti", "Hartmann Converti", "RA Protégée"),
  mediane_jours = c(143, 115, 120),  # Médianes pour chaque groupe, ajustez selon les données
  y_label = c(-1, -8, -15)
)

# Récupérer la survie au temps de médiane dans km_fit
get_surv_at_time <- function(km_fit, groupe, temps) {
  surv_data <- summary(km_fit, times = temps)$surv
  surv_percent <- 100 * (1 - surv_data)
  return(surv_percent[groupe])  # 1 pour "Hartmann Non Converti", 2 pour "Hartmann Converti", 3 pour "RA Protégée"
}

mediane_fermeture$y_courbe <- sapply(1:nrow(mediane_fermeture), function(i) {
  get_surv_at_time(km_fit,
                   i,
                   mediane_fermeture$mediane_jours[i])
})

# Générer la courbe pour récupérer les couleurs utilisées
p_temp <- ggsurvplot(km_fit, data = df_survie)
colors_used <- ggplot_build(p_temp$plot)$data[[1]] %>%
  distinct(group, colour) %>%
  arrange(group) %>%
  pull(colour)

# Tracer la courbe principale
p <- ggsurvplot(
  km_fit,
  data = df_survie,
  pval = TRUE,
  risk.table = TRUE,
  break.time.by = 100,
  conf.int = TRUE,
  xlim = c(0, 365),
  xlab = "Temps (jours)",
  ylab = "Taux de rétablissement de la continuité (%)",
  legend.title = "Groupe",
  legend.labs = c("Hartmann Non Converti", "Hartmann Converti", "RA Protégée"),
  fun = function(x) 100 * (1 - x)
)

# Modifier uniquement les lignes des médianes
p$plot <- p$plot +
  guides(
    fill = "none",
    linetype = "none",
    color = guide_legend(override.aes = list(linetype = 1))
  ) +
  
  geom_hline(aes(yintercept = pourcentage_fermeture),
             data = taux_final_fermeture, linetype = "dashed", size = 1,
             color = "black", show.legend = FALSE) +
  
  geom_text(data = taux_final_fermeture, aes(x = 365, y = pourcentage_fermeture,
                                             label = paste0(round(pourcentage_fermeture, 1), "%")),
            vjust = -0.5, size = 5, fontface = "bold", color = "black") +
  
  annotate("text", x = 600, y = 10,
           label = paste0("Log-rank p = ", p_value),
           size = 5, hjust = 0) +
  
  # Lignes médianes exactement sur la courbe
  geom_segment(data = mediane_fermeture,
               aes(x = 0, xend = mediane_jours,
                   y = y_courbe, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  
  geom_segment(data = mediane_fermeture,
               aes(x = mediane_jours, xend = mediane_jours,
                   y = 0, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  
  geom_text(data = mediane_fermeture,
            aes(x = mediane_jours, y = y_label,
                label = paste0(mediane_jours, " j")),
            color = colors_used, size = 5, fontface = "bold",
            hjust = 0.5, show.legend = FALSE)

# Afficher la courbe
print(p)






























#**************COURBE 3 NIVEAUX KAPLAN**********


# Charger les bibliothèques nécessaires
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

unique(m.final$I_geste_comparaison_1HartmannCoelio)

# Créer une variable pour distinguer les trois groupes
df_survie <- m.final %>% 
  filter(I_stomie == 1) %>%  # Tous les patients ont une stomie par définition
  mutate(
    S_duree_avec_stomie = as.numeric(S_duree_avec_stomie),  # Assurez-vous que S_duree_avec_stomie est numérique
    fermeture_stomie = ifelse(S_fermeturestomie == 1, 1, 0),  # Créer la variable fermeture de stomie
    S_duree_avec_stomie = ifelse(S_fermeturestomie == 1, S_duree_avec_stomie, 365),  # 730 jours pour les patients sans fermeture
    # Créer une variable pour distinguer les trois groupes
    groupe = case_when(
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 0 ~ "Hartmann Non Converti",
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 1 ~ "Hartmann Converti",
      I_geste_comparaison_1HartmannCoelio == "Résection anastomose protégée" ~ "RA Protégée",  # Correction du nom
      TRUE ~ NA_character_  # Exclure les autres cas
    )
  )


# Convertir la variable groupe en facteur
df_survie$groupe <- factor(df_survie$groupe, levels = c("Hartmann Non Converti", "Hartmann Converti", "RA Protégée"))

# Définir l'objet de survie
surv_object <- Surv(time = df_survie$S_duree_avec_stomie, event = df_survie$fermeture_stomie)

# Modèle de Kaplan-Meier pour la comparaison entre les trois groupes
km_fit <- survfit(surv_object ~ groupe, data = df_survie)

# Test du log-rank pour comparer les groupes
test_logrank <- survdiff(surv_object ~ groupe, data = df_survie)
raw_p_value <- 1 - pchisq(test_logrank$chisq, df = length(test_logrank$n) - 1)
p_value <- ifelse(raw_p_value < 0.001, formatC(raw_p_value, format = "e", digits = 2),
                  formatC(raw_p_value, format = "f", digits = 3))

# Pourcentage final de fermeture de stomie pour chaque groupe
taux_final_fermeture <- df_survie %>%
  group_by(groupe) %>%
  summarise(pourcentage_fermeture = mean(S_fermeturestomie) * 100, .groups = "drop")

# Médianes fixées manuellement pour chaque groupe
mediane_fermeture <- data.frame(
  groupe = c("Hartmann Non Converti", "Hartmann Converti", "RA Protégée"),
  mediane_jours = c(143, 115, 120),  # Médianes pour chaque groupe, ajustez selon les données
  y_label = c(-1, -8, -15)
)

# Récupérer la survie au temps de médiane dans km_fit
get_surv_at_time <- function(km_fit, groupe, temps) {
  surv_data <- summary(km_fit, times = temps)$surv
  surv_percent <- 100 * (1 - surv_data)
  return(surv_percent[groupe])  # 1 pour "Hartmann Non Converti", 2 pour "Hartmann Converti", 3 pour "RA Protégée"
}

mediane_fermeture$y_courbe <- sapply(1:nrow(mediane_fermeture), function(i) {
  get_surv_at_time(km_fit,
                   i,
                   mediane_fermeture$mediane_jours[i])
})

# Générer la courbe pour récupérer les couleurs utilisées
p_temp <- ggsurvplot(km_fit, data = df_survie)
colors_used <- ggplot_build(p_temp$plot)$data[[1]] %>%
  distinct(group, colour) %>%
  arrange(group) %>%
  pull(colour)

# Tracer la courbe principale
p <- ggsurvplot(
  km_fit,
  data = df_survie,
  pval = TRUE,
  risk.table = TRUE,
  break.time.by = 100,
  conf.int = TRUE,
  xlim = c(0, 365),
  xlab = "Temps (jours)",
  ylab = "Taux de rétablissement de la continuité (%)",
  legend.title = "Groupe",
  legend.labs = c("Hartmann Non Converti", "Hartmann Converti", "RA Protégée"),
  fun = function(x) 100 * (1 - x)
)

# Modifier uniquement les lignes des médianes
p$plot <- p$plot +
  guides(
    fill = "none",
    linetype = "none",
    color = guide_legend(override.aes = list(linetype = 1))
  ) +
  
  geom_hline(aes(yintercept = pourcentage_fermeture),
             data = taux_final_fermeture, linetype = "dashed", size = 1,
             color = "black", show.legend = FALSE) +
  
  geom_text(data = taux_final_fermeture, aes(x = 365, y = pourcentage_fermeture,
                                             label = paste0(round(pourcentage_fermeture, 1), "%")),
            vjust = -0.5, size = 5, fontface = "bold", color = "black") +
  
  annotate("text", x = 600, y = 10,
           label = paste0("Log-rank p = ", p_value),
           size = 5, hjust = 0) +
  
  # Lignes médianes exactement sur la courbe
  geom_segment(data = mediane_fermeture,
               aes(x = 0, xend = mediane_jours,
                   y = y_courbe, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  
  geom_segment(data = mediane_fermeture,
               aes(x = mediane_jours, xend = mediane_jours,
                   y = 0, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  
  geom_text(data = mediane_fermeture,
            aes(x = mediane_jours, y = y_label,
                label = paste0(mediane_jours, " j")),
            color = colors_used, size = 5, fontface = "bold",
            hjust = 0.5, show.legend = FALSE)

# Afficher la courbe
print(p)













#**************COURBE 3 NIVEAUX QUI FONCTIONNE***********
# Charger les bibliothèques nécessaires
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Créer une variable pour distinguer les trois groupes
df_survie <- m.final %>% 
  filter(I_stomie == 1) %>%
  mutate(
    S_duree_avec_stomie_orig = as.numeric(S_duree_avec_stomie),
    fermeture_stomie = ifelse(S_fermeturestomie == 1, 1, 0),
    S_duree_avec_stomie = ifelse(S_fermeturestomie == 1, S_duree_avec_stomie_orig, 365),
    groupe = case_when(
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 0 ~ "Hartmann Non Converti",
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 1 ~ "Hartmann Converti",
      I_geste_comparaison_1HartmannCoelio == "Résection anastomose protégée" ~ "RA Protégée",
      TRUE ~ NA_character_
    )
  )

df_survie$groupe <- factor(df_survie$groupe, levels = c("Hartmann Non Converti", "Hartmann Converti", "RA Protégée"))

# Objet de survie
surv_object <- Surv(time = df_survie$S_duree_avec_stomie, event = df_survie$fermeture_stomie)
km_fit <- survfit(surv_object ~ groupe, data = df_survie)

# Log-rank
test_logrank <- survdiff(surv_object ~ groupe, data = df_survie)
raw_p_value <- 1 - pchisq(test_logrank$chisq, df = length(test_logrank$n) - 1)
p_value <- ifelse(raw_p_value < 0.001, formatC(raw_p_value, format = "e", digits = 2),
                  formatC(raw_p_value, format = "f", digits = 3))

# Pourcentages de fermeture
taux_final_fermeture <- df_survie %>%
  group_by(groupe) %>%
  summarise(pourcentage_fermeture = mean(S_fermeturestomie) * 100, .groups = "drop")

# Médianes comme dans tbl_summary (non censurées)
mediane_fermeture <- m.final %>%
  filter(I_stomie == 1) %>%
  mutate(
    groupe = case_when(
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 0 ~ "Hartmann Non Converti",
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 1 ~ "Hartmann Converti",
      I_geste_comparaison_1HartmannCoelio == "Résection anastomose protégée" ~ "RA Protégée",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(groupe) %>%
  summarise(mediane_jours = median(S_duree_avec_stomie, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    y_label = c(-1, -8, -15),
    label_median = paste0(mediane_jours, "j")
  )

# Position verticale des médianes
get_surv_at_time <- function(km_fit, groupe, temps) {
  surv_data <- summary(km_fit, times = temps)$surv
  surv_percent <- 100 * (1 - surv_data)
  return(surv_percent[groupe])
}

mediane_fermeture$y_courbe <- sapply(1:nrow(mediane_fermeture), function(i) {
  get_surv_at_time(km_fit, i, mediane_fermeture$mediane_jours[i])
})

# Récupérer les couleurs
p_temp <- ggsurvplot(km_fit, data = df_survie)
colors_used <- ggplot_build(p_temp$plot)$data[[1]] %>%
  distinct(group, colour) %>%
  arrange(group) %>%
  pull(colour)

# Courbe principale
p <- ggsurvplot(
  km_fit,
  data = df_survie,
  pval = TRUE,
  risk.table = TRUE,
  conf.int = TRUE,
  break.time.by = 100,
  xlim = c(0, 365),
  xlab = "Temps (jours)",
  ylab = "Taux de rétablissement de la continuité (%)",
  legend.title = "Groupe",
  legend.labs = c("Hartmann Non Converti", "Hartmann Converti", "RA Protégée"),
  fun = function(x) 100 * (1 - x)
)

# Ajouter les éléments
p$plot <- p$plot +
  guides(fill = "none", linetype = "none", color = guide_legend(override.aes = list(linetype = 1))) +
  geom_hline(aes(yintercept = pourcentage_fermeture),
             data = taux_final_fermeture, linetype = "dashed", size = 1,
             color = "black", show.legend = FALSE) +
  geom_text(data = taux_final_fermeture, aes(x = 365, y = pourcentage_fermeture,
                                             label = paste0(round(pourcentage_fermeture, 1), "%")),
            vjust = -0.5, size = 5, fontface = "bold", color = "black") +
  annotate("text", x = 300, y = 10,
           label = paste0("Log-rank p = ", p_value),
           size = 5, hjust = 0) +
  geom_segment(data = mediane_fermeture,
               aes(x = 0, xend = mediane_jours,
                   y = y_courbe, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  geom_segment(data = mediane_fermeture,
               aes(x = mediane_jours, xend = mediane_jours,
                   y = 0, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  geom_text(data = mediane_fermeture,
            aes(x = mediane_jours, y = y_label,
                label = label_median),
            color = colors_used, size = 5, fontface = "bold",
            hjust = 0.5, show.legend = FALSE)

# Affichage
print(p)















# Vérification des durées de stomie pour chaque groupe de conversion
df_survie_hartmann_coelio <- df_survie %>%
  filter(I_geste_comparaison_1HartmannCoelio == "Hartmann") %>%  # Filtrer uniquement pour Hartmann Coelio
  group_by(I_conversion) %>%
  summarise(
    moyenne_duree_stomie = mean(S_duree_avec_stomie, na.rm = TRUE),  # Calculer la moyenne de la durée de stomie
    mediane_duree_stomie = median(S_duree_avec_stomie, na.rm = TRUE),  # Calculer la médiane de la durée de stomie
    .groups = "drop"
  )

# Afficher les résultats
print(df_survie_hartmann_coelio)

df_survie_hartmann_coelio






# Aller dans le dossier Git
setwd("/Users/thomashusson/Documents/R/R Hartmann/Hartmann Coelio")

# Nom du fichier à pusher (déjà connu)
fichier <- "script hartmann RA quasi parfait.R"

# Vérifier que le fichier existe
if (!file.exists(fichier)) {
  stop("❌ Le fichier spécifié est introuvable.")
}

# Message de commit (tu peux modifier ici)
message_commit <- "Mise à jour du script Hartmann RA quasi parfait"

# Ajouter, committer, pusher
system(paste("git add", shQuote(fichier)))
system(paste("git commit -m", shQuote(message_commit)))
system("git push")

